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

public class MessageManager {
    // This is a bit annoying... I couldn't figure out how to cast between AnyObject and Message
    // since the latter is @objc wrapped. Therefore Message objects are wrapped in
    // NewMessageNotificationWrapper objects when passed through the userInfo attribute of
    // notifications.
    public class NewMessagesNotificationWrapper {
        public let messages: [Message]

        init(messages: [Message]) {
            self.messages = messages
        }
    }

    public static let sharedManager = MessageManager()

    public static let NewMessagesNotification = "NewMessages"
    public static let NewChatNotification = "NewChat"
    public static let UnreadMessageCountUpdated = "UnreadMessageCountUpdated"
    static let UnreadMessageCountReset = "UnreadMessageCountReset"
    public static let TotalUnreadMessageCountUpdated = "TotalUnreadMessageCountUpdated"

    var unreadMessageCount: Int {
        var unreadMessageCount = 0
        for (_, count) in unreadMessageCountForChat {
            unreadMessageCount += count
        }
        return unreadMessageCount
    }

    private var unreadMessageCountForChat: [Chat:Int] = [:]

    private var messageSender = MessageSender()

    private var receivedMessages: [ReceivedMessage] = []
    private var processingReceivedMessages = false

    public func setup() {
        NSNotificationCenter.defaultCenter().addObserver(self, selector: "didReceiveMessages:", name: ChatClient.ChatClientReceivedMessagesNotification, object: nil)
    }

    public func sendMessageWithText(text: String, toChat chat: Chat, callback: Message?->Void) {
        let message = Message.createWithText(text, chat: chat)
        messageSender.sendMessage(message)
        CoreData.save()

        callback(message)
    }

    public func sendMessageWithImage(image: UIImage, toChat chat: Chat, callback: Message?->Void) {
        APIManager.sharedManager.createFileForFriend(chat.participantsArray[0], numFiles: 2) {
            fileIds in
            guard let fileIds = fileIds where fileIds.count == 2 else {
                callback(nil)
                return
            }

            let imageFile = File.createWithId(fileIds[0], data: UIImageJPEGRepresentation(image, 0.5)!, contentType: "image/jpeg")
            let thumbnailFile = File.createWithId(fileIds[1], data: UIImageJPEGRepresentation(image.resizeToPercentage(min(1.0, image.size.width / 640)), 0.5)!, contentType: "image/jpeg")

            let message = Message.createWithImageFile(imageFile, thumbnailFile: thumbnailFile, chat: chat)
            CoreData.save()

            if message != nil {
                self.messageSender.sendMessage(message!, files: [imageFile, thumbnailFile])
            }
            callback(message)
        }
    }

    public func sendMessageWithMediaUrl(mediaUrl: NSURL, toChat chat: Chat, callback: Message?->Void) {
        APIManager.sharedManager.createFileForFriend(chat.participantsArray[0], numFiles: 2) {
            fileIds in
            guard let fileIds = fileIds where fileIds.count == 2 else {
                callback(nil)
                return
            }

            self.imageFromMediaUrl(mediaUrl) {
                image in

                guard let image = image, movieData = NSData(contentsOfURL: mediaUrl) else {
                    callback(nil)
                    return
                }

                let movieFile = File.createWithId(fileIds[0], data: movieData, contentType: "video/mp4")
                let thumbnailFile = File.createWithId(fileIds[1], data: UIImageJPEGRepresentation(image, 0.5)!, contentType: "image/jpeg")

                let message = Message.createWithMovieFile(movieFile, thumbnailFile: thumbnailFile, chat: chat)
                CoreData.save()

                if message != nil {
                    self.messageSender.sendMessage(message!, files: [movieFile, thumbnailFile])
                }
                callback(message)
            }
        }
    }

    public func getMessagesForChat(chat: Chat, beforeDate: NSDate? = nil, fetchLimit: Int = 15) -> [Message] {
        return Message.findForChat(chat, beforeDate: beforeDate, fetchLimit: fetchLimit)
    }

    public func unreadMessageCountForChat(chat: Chat) -> Int {
        return unreadMessageCountForChat[chat] ?? 0
    }

    public func markMessagesForChatAsRead(chat: Chat) {
        unreadMessageCountForChat[chat] = 0
        sendUnreadMessagesCountUpdatedNotificationForChat(chat, withUnreadMessageCount: 0)
    }

    private func addMessage(message: Message) {
        addMessages([message], forChat: message.chat)
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

    private func maybeProcessReceivedMessages() {
        if processingReceivedMessages {
            return
        }
        processingReceivedMessages = true

        processNextReceivedMessage()
    }

    private func processNextReceivedMessage() {
        if receivedMessages.isEmpty {
            processingReceivedMessages = false
            return
        }

        let nextMessage = receivedMessages.removeAtIndex(0)
        guard let participantIdsJson = nextMessage.messageJson["p"].array else {
            return
        }

        var participants: [Friend] = []
        var unknownIds: [Int] = []
        for participantIdJson in participantIdsJson {
            guard let participantId = participantIdJson.int else {
                continue
            }

            if participantId == User.userId {
                continue
            } else if let friend = FriendManager.sharedManager.getFriendById(participantId) where !friend.firstName.isEmpty {
                participants.append(friend)
            } else {
                unknownIds.append(participantId)
            }
        }

        if unknownIds.count > 0 {
            APIManager.sharedManager.getInfoForUsersWithIds(unknownIds) { names in
                guard let names = names else {
                    self.processNextReceivedMessage()
                    return
                }

                for i in 0..<names.count {
                    participants.append(Friend.createWithId(unknownIds[i], firstName: names[i].firstName, lastName: names[i].lastName))
                }
                CoreData.save()

                self.processReceivedMessage(nextMessage, withParticipants: participants)
                self.processNextReceivedMessage()
            }
        } else {
            processReceivedMessage(nextMessage, withParticipants: participants)
            processNextReceivedMessage()
        }
    }

    private func processReceivedMessage(receivedMessage: ReceivedMessage, withParticipants participants: [Friend]) {
        guard let friend = FriendManager.sharedManager.getFriendById(receivedMessage.fromId) else {
            return
        }

        var chat = Chat.findWithFriends(participants)
        if chat == nil {
            chat = Chat.createWithParticipants(participants)
            CoreData.save()

            NSNotificationCenter.defaultCenter().postNotificationName(MessageManager.NewChatNotification, object: nil, userInfo: ["chat": chat!])
        }

        let message = Message.createWithFrom(friend, chat: chat!, date: NSDate(timeIntervalSince1970: NSTimeInterval(receivedMessage.timestamp)), secretKey: receivedMessage.secretKey, messageJson: receivedMessage.messageJson)
        CoreData.save()

        addMessage(message)
    }

    private func sendNewMessagesNotificationForChat(chat: Chat, withMessages messages: [Message]) {
        NSNotificationCenter.defaultCenter().postNotificationName(MessageManager.NewMessagesNotification, object: chat, userInfo: ["messages": NewMessagesNotificationWrapper(messages: messages)])
    }

    @objc private func didReceiveMessages(notification: NSNotification) {
        guard let receivedMessages = notification.userInfo?["receivedMessages"] as? [ReceivedMessage] else {
            return
        }

        self.receivedMessages += receivedMessages
        maybeProcessReceivedMessages()
    }
}