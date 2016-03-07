//
//  MessageManager.swift
//  chat
//
//  Created by Andreas Binnewies on 2/2/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import AVFoundation
import CoreData
import Foundation
import MagicalRecord
import SwiftyJSON

class MessageManager {
    // This is a bit annoying... I couldn't figure out how to cast between AnyObject and Message
    // since the latter is @objc wrapped. Therefore Message objects are wrapped in
    // NewMessageNotificationWrapper objects when passed through the userInfo attribute of
    // notifications.
    class NewMessagesNotificationWrapper {
        let messages: [Message]

        init(messages: [Message]) {
            self.messages = messages
        }
    }

    static let sharedManager = MessageManager()

    static let NewMessagesNotification = "NewMessages"
    static let NewChatNotification = "NewChat"
    static let UnreadMessageCountUpdated = "UnreadMessageCountUpdated"
    static let UnreadMessageCountReset = "UnreadMessageCountReset"
    static let TotalUnreadMessageCountUpdated = "TotalUnreadMessageCountUpdated"

    var unreadMessageCount: Int {
        var unreadMessageCount = 0
        for (_, count) in unreadMessageCountForChat {
            unreadMessageCount += count
        }
        return unreadMessageCount
    }

    private var unreadMessageCountForChat: [Chat:Int] = [:]

    private var messageSender = MessageSender()

    func setup() {
        NSNotificationCenter.defaultCenter().addObserver(self, selector: "didReceiveMessages:", name: ChatClient.ChatClientReceivedMessagesNotification, object: nil)
    }

    func sendMessageWithText(text: String, toChat chat: Chat, callback: Message?->Void) {
        let message = Message.createWithText(text, chat: chat)
        messageSender.sendMessage(message)
        CoreData.save()

        callback(message)
    }

    func sendMessageWithImage(image: UIImage, toChat chat: Chat, callback: Message?->Void) {
        APIManager.sharedManager.createFileForFriend(chat.participantsArray[0], numFiles: 2) {
            fileId in
            var message: Message?
            if let fileId = fileId {
                let imageFile = File.createWithId(fileId, data: UIImageJPEGRepresentation(image, 0.5)!, contentType: "image/jpeg")
                let thumbnailFile = File.createWithId(fileId + 1, data: UIImageJPEGRepresentation(image.resizeToPercentage(0.25), 0.2)!, contentType: "image/jpeg")
                message = Message.createWithImageFile(imageFile, thumbnailFile: thumbnailFile, chat: chat)
                if message != nil {
                    self.messageSender.sendMessage(message!, files: [imageFile, thumbnailFile])
                }
                CoreData.save()
            }
            callback(message)
        }
    }

    func sendMessageWithMediaUrl(mediaUrl: NSURL, toChat chat: Chat, callback: Message?->Void) {
        APIManager.sharedManager.createFileForFriend(chat.participantsArray[0], numFiles: 2) {
            fileId in
            guard let fileId = fileId else {
                callback(nil)
                return
            }

            self.imageFromMediaUrl(mediaUrl) {
                image in

                guard let image = image, movieData = NSData(contentsOfURL: mediaUrl) else {
                    callback(nil)
                    return
                }

                let movieFile = File.createWithId(fileId, data: movieData, contentType: "video/mp4")
                let thumbnailFile = File.createWithId(fileId + 1, data: UIImageJPEGRepresentation(image, 0.5)!, contentType: "image/jpeg")

                let message = Message.createWithMovieFile(movieFile, thumbnailFile: thumbnailFile, chat: chat)
                if message != nil {
                    self.messageSender.sendMessage(message!, files: [movieFile, thumbnailFile])
                }
                CoreData.save()

                callback(message)
            }
        }
    }

    func getMessagesForChat(chat: Chat, beforeDate: NSDate? = nil, fetchLimit: Int = 15) -> [Message] {
        return Message.findForChat(chat, beforeDate: beforeDate, fetchLimit: fetchLimit)
    }

    func unreadMessageCountForChat(chat: Chat) -> Int {
        return unreadMessageCountForChat[chat] ?? 0
    }

    func markMessagesForChatAsRead(chat: Chat) {
        unreadMessageCountForChat[chat] = 0
        sendUnreadMessagesCountUpdatedNotificationForChat(chat, withUnreadMessageCount: 0)
    }

    private func addMessages(messages: [Message], forChat chat: Chat) {
        unreadMessageCountForChat[chat] = (unreadMessageCountForChat[chat] ?? 0) + messages.count
        sendUnreadMessagesCountUpdatedNotificationForChat(chat, withUnreadMessageCount: unreadMessageCountForChat[chat]!)
        sendNewMessagesNotificationForChat(chat, withMessages: messages)
    }

    private func sendUnreadMessagesCountUpdatedNotificationForChat(chat: Chat, withUnreadMessageCount unreadMessageCount: Int) {
        NSNotificationCenter.defaultCenter().postNotificationName(MessageManager.UnreadMessageCountUpdated, object: chat, userInfo: ["unreadMessageCount": unreadMessageCount])

        var totalUnreadMessageCount = 0
        for (_, count) in unreadMessageCountForChat {
            totalUnreadMessageCount += count
        }
        NSNotificationCenter.defaultCenter().postNotificationName(MessageManager.TotalUnreadMessageCountUpdated, object: nil, userInfo: ["unreadMessageCount": totalUnreadMessageCount])
    }

    private func imageFromMediaUrl(mediaUrl: NSURL, completion: UIImage?->Void) {
        let asset = AVURLAsset(URL: mediaUrl)
        let assetGenerator = AVAssetImageGenerator(asset: asset)
        assetGenerator.appliesPreferredTrackTransform = true
        assetGenerator.generateCGImagesAsynchronouslyForTimes([NSValue(CMTime: CMTimeMake(0, 30))]) {
            requestedTime, image, actualTime, result, error in

            dispatch_async(dispatch_get_main_queue()) {
                guard let image = image else {
                    completion(nil)
                    return
                }

                completion(UIImage(CGImage: image))
            }
        }
    }

    private func sendNewMessagesNotificationForChat(chat: Chat, withMessages messages: [Message]) {
        NSNotificationCenter.defaultCenter().postNotificationName(MessageManager.NewMessagesNotification, object: chat, userInfo: ["messages": NewMessagesNotificationWrapper(messages: messages)])
    }

    @objc private func didReceiveMessages(notification: NSNotification) {
        guard let receivedMessages = notification.userInfo?["receivedMessages"] as? [ReceivedMessage] else {
            return
        }

        var messagesByChats: [Chat:[Message]] = [:]
        for receivedMessage in receivedMessages {
            if let friend = FriendManager.sharedManager.getFriendById(receivedMessage.fromId) {
                var chat = Chat.findWithFriends([friend])
                if chat == nil {
                    chat = Chat.createWithParticipants([friend])
                    CoreData.save()

                    NSNotificationCenter.defaultCenter().postNotificationName(MessageManager.NewChatNotification, object: friend, userInfo: ["chat": chat!])
                }

                if messagesByChats[chat!] == nil {
                    messagesByChats[chat!] = []
                }

                messagesByChats[chat!]!.append(Message.createWithFrom(friend, chat: chat!, date: NSDate(timeIntervalSince1970: NSTimeInterval(receivedMessage.timestamp)), messageJson: receivedMessage.messageJson))
            }
        }

        CoreData.save()

        for (chat, messages) in messagesByChats {
            addMessages(messages, forChat: chat)
        }
    }
}