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
    static let FriendUnreadMessageCountUpdated = "FriendUnreadMessageCountUpdated"
    static let FriendUnreadMessageCountReset = "FriendUnreadMessageCountReset"
    static let TotalUnreadMessageCountUpdated = "TotalUnreadMessageCountUpdated"

    var unreadMessageCount: Int {
        var unreadMessageCount = 0
        for (_, count) in unreadMessageCountForFriend {
            unreadMessageCount += count
        }
        return unreadMessageCount
    }

    private var unreadMessageCountForFriend: [Friend:Int] = [:]

    private var messageSender = MessageSender()

    func setup() {
        NSNotificationCenter.defaultCenter().addObserver(self, selector: "didReceiveMessages:", name: ChatClient.ChatClientReceivedMessagesNotification, object: nil)
    }

    func sendMessageWithText(text: String, to: Friend, callback: Message?->Void) {
        let message = Message.createWithText(text, to: to)
        messageSender.sendMessage(message)
        CoreData.save()

        callback(message)
    }

    func sendMessageWithImage(image: UIImage, to: Friend, callback: Message?->Void) {
        APIManager.sharedManager.createFileForFriend(to, numFiles: 2) {
            fileId in
            var message: Message?
            if let fileId = fileId {
                let imageFile = File.createWithId(fileId, data: UIImageJPEGRepresentation(image, 0.5)!, contentType: "image/jpeg")
                let thumbnailFile = File.createWithId(fileId + 1, data: UIImageJPEGRepresentation(image.resizeToPercentage(0.25), 0.2)!, contentType: "image/jpeg")
                message = Message.createWithImageFile(imageFile, thumbnailFile: thumbnailFile, to: to)
                if message != nil {
                    self.messageSender.sendMessage(message!, files: [imageFile, thumbnailFile])
                }
                CoreData.save()
            }
            callback(message)
        }
    }

    func sendMessageWithMediaUrl(mediaUrl: NSURL, to: Friend, callback: Message?->Void) {
        APIManager.sharedManager.createFileForFriend(to, numFiles: 2) {
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

                let message = Message.createWithMovieFile(movieFile, thumbnailFile: thumbnailFile, to: to)
                if message != nil {
                    self.messageSender.sendMessage(message!, files: [movieFile, thumbnailFile])
                }
                CoreData.save()

                callback(message)
            }
        }
    }

    func getMessagesForFriend(friend: Friend, beforeDate: NSDate? = nil, fetchLimit: Int = 15) -> [Message] {
        return Message.findForFriend(friend, beforeDate: beforeDate, fetchLimit: fetchLimit)
    }

    func unreadMessageCountForFriend(friend: Friend) -> Int {
        return unreadMessageCountForFriend[friend] ?? 0
    }

    func markMessagesForFriendAsRead(friend: Friend) {
        unreadMessageCountForFriend[friend] = 0
        sendUnreadMessagesCountUpdatedNotificationForFriend(friend, withUnreadMessageCount: 0)
    }

    private func addMessages(messages: [Message], forFriend friend: Friend) {
        unreadMessageCountForFriend[friend] = (unreadMessageCountForFriend[friend] ?? 0) + messages.count
        sendUnreadMessagesCountUpdatedNotificationForFriend(friend, withUnreadMessageCount: unreadMessageCountForFriend[friend]!)
        sendNewMessagesNotificationForFriend(friend, withMessages: messages)
    }

    private func sendUnreadMessagesCountUpdatedNotificationForFriend(friend: Friend, withUnreadMessageCount unreadMessageCount: Int) {
        NSNotificationCenter.defaultCenter().postNotificationName(MessageManager.FriendUnreadMessageCountUpdated, object: friend, userInfo: ["unreadMessageCount": unreadMessageCount])

        var totalUnreadMessageCount = 0
        for (_, count) in unreadMessageCountForFriend {
            totalUnreadMessageCount += count
        }
        NSNotificationCenter.defaultCenter().postNotificationName(MessageManager.TotalUnreadMessageCountUpdated, object: friend, userInfo: ["unreadMessageCount": totalUnreadMessageCount])
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

    private func sendNewMessagesNotificationForFriend(friend: Friend, withMessages messages: [Message]) {
        NSNotificationCenter.defaultCenter().postNotificationName(MessageManager.NewMessagesNotification, object: friend, userInfo: ["messages": NewMessagesNotificationWrapper(messages: messages)])
    }

    @objc private func didReceiveMessages(notification: NSNotification) {
        guard let receivedMessages = notification.userInfo?["receivedMessages"] as? [ReceivedMessage] else {
            return
        }

        var messagesForFriends: [Friend:[Message]] = [:]
        for receivedMessage in receivedMessages {
            if let friend = FriendManager.sharedManager.getFriendById(receivedMessage.fromId) {
                if messagesForFriends[friend] == nil {
                    messagesForFriends[friend] = []
                }

                messagesForFriends[friend]!.append(Message.createWithFrom(friend, to: nil, date: NSDate(timeIntervalSince1970: NSTimeInterval(receivedMessage.timestamp)), messageJson: receivedMessage.messageJson))
            }
        }

        CoreData.save()

        for (friend, messages) in messagesForFriends {
            addMessages(messages, forFriend: friend)
        }
    }
}