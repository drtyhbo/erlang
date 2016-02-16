//
//  MessageManager.swift
//  chat
//
//  Created by Andreas Binnewies on 2/2/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import CoreData
import Foundation
import MagicalRecord
import SwiftyJSON

class MessageManager {
    // This is a bit annoying... I couldn't figure out how to cast between AnyObject and Message
    // since the latter is @objc wrapped. Therefore Message objects are wrapped in
    // NewMessageNotificationWrapper objects when passed through the userInfo attribute of
    // notifications.
    class NewMessageNotificationWrapper {
        let message: Message

        init(message: Message) {
            self.message = message
        }
    }

    static let sharedManager = MessageManager()

    static let NewMessageNotification = "NewMessage"
    static let UnreadMessageCountUpdated = "UnreadMessageCountUpdated"
    static let UnreadMessageCountReset = "UnreadMessageCountReset"

    private var unreadMessageCountForFriend: [Friend:Int] = [:]

    private var messageSender = MessageSender()

    func setup() {
        NSNotificationCenter.defaultCenter().addObserver(self, selector: "didReceiveMessage:", name: ChatClient.ChatClientReceivedMessageNotification, object: nil)
    }

    func sendMessageWithText(text: String, to: Friend, callback: Message?->Void) {
        let message = Message.createWithText(text, to: to)
        CoreData.save()

        messageSender.sendMessage(message)
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

    func getMessagesForFriend(friend: Friend) -> [Message] {
        return Message.findForFriend(friend)
    }

    func unreadMessageCountForFriend(friend: Friend) -> Int {
        return unreadMessageCountForFriend[friend] ?? 0
    }

    func markMessagesForFriendAsRead(friend: Friend) {
        unreadMessageCountForFriend[friend] = 0
        sendUnreadMessagesCountUpdatedNotificationForFriend(friend, withUnreadMessageCount: 0)
    }

    private func addMessage(message: Message, forFriend friend: Friend) {
        unreadMessageCountForFriend[friend] = (unreadMessageCountForFriend[friend] ?? 0) + 1
        sendUnreadMessagesCountUpdatedNotificationForFriend(friend, withUnreadMessageCount: unreadMessageCountForFriend[friend]!)
        sendNewMessageNotificationForFriend(friend, withMessage: message)
    }

    private func sendUnreadMessagesCountUpdatedNotificationForFriend(friend: Friend, withUnreadMessageCount unreadMessageCount: Int) {
        NSNotificationCenter.defaultCenter().postNotificationName(MessageManager.UnreadMessageCountUpdated, object: friend, userInfo: ["unreadMessageCount": unreadMessageCount])
    }

    private func sendNewMessageNotificationForFriend(friend: Friend, withMessage message: Message) {
        NSNotificationCenter.defaultCenter().postNotificationName(MessageManager.NewMessageNotification, object: friend, userInfo: ["message": NewMessageNotificationWrapper(message: message)])
    }

    @objc private func didReceiveMessage(notification: NSNotification) {
        if let receivedMessage = notification.userInfo?["receivedMessage"] as? ReceivedMessage, let friend = FriendManager.sharedManager.getFriendById(receivedMessage.fromId) {
            let message = Message.createWithFrom(friend, to: nil, date: NSDate(timeIntervalSince1970: NSTimeInterval(receivedMessage.timestamp)), messageJson: receivedMessage.messageJson)
            CoreData.save()
            addMessage(message, forFriend: friend)
        }
    }
}