//
//  MessageManager.swift
//  chat
//
//  Created by Andreas Binnewies on 2/2/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation

class MessageManager {
    private class MessagesWrapper {
        var messages: [Message] = []
        var unreadMessageCount = 0
    }

    static let sharedManager = MessageManager()

    static let NewMessageNotification = "NewMessage"
    static let UnreadMessageCountUpdated = "UnreadMessageCountUpdated"
    static let UnreadMessageCountReset = "UnreadMessageCountReset"

    private var messagesByFriend: [Friend:MessagesWrapper] = [:]

    func setup() {
        NSNotificationCenter.defaultCenter().addObserver(self, selector: "didSendMessage:", name: ChatClient.ChatClientSentMessageNotification, object: nil)
        NSNotificationCenter.defaultCenter().addObserver(self, selector: "didReceiveMessage:", name: ChatClient.ChatClientReceivedMessageNotification, object: nil)
    }

    func messagesForFriend(friend: Friend) -> [Message] {
        return messagesByFriend[friend]?.messages ?? []
    }

    func unreadMessageCountForFriend(friend: Friend) -> Int {
        return messagesByFriend[friend]?.unreadMessageCount ?? 0
    }

    func markMessagesForFriendAsRead(friend: Friend) {
        if let messagesWrapper = messagesByFriend[friend] {
            messagesWrapper.unreadMessageCount = 0
            sendUnreadMessagesCountUpdatedNotificationForFriend(friend, withUnreadMessageCount: 0)
        }
    }

    func appendMessage(message: Message, forFriend friend: Friend) {
        if messagesByFriend[friend] == nil {
            messagesByFriend[friend] = MessagesWrapper()
        }

        let messagesWrapper = messagesByFriend[friend]!

        messagesWrapper.unreadMessageCount++
        sendUnreadMessagesCountUpdatedNotificationForFriend(friend, withUnreadMessageCount: messagesWrapper.unreadMessageCount)

        messagesWrapper.messages.append(message)
        sendNewMessageNotificationForFriend(friend, withMessage: message)
    }

    private func sendUnreadMessagesCountUpdatedNotificationForFriend(friend: Friend, withUnreadMessageCount unreadMessageCount: Int) {
        NSNotificationCenter.defaultCenter().postNotificationName(MessageManager.UnreadMessageCountUpdated, object: friend, userInfo: ["unreadMessageCount": unreadMessageCount])
    }

    private func sendNewMessageNotificationForFriend(friend: Friend, withMessage message: Message) {
        NSNotificationCenter.defaultCenter().postNotificationName(MessageManager.NewMessageNotification, object: friend, userInfo: ["message": message])
    }

    @objc private func didSendMessage(notification: NSNotification) {
        if let sentMessage = notification.userInfo?["sentMessage"] as? SentMessage, let friend = FriendManager.sharedManager.getFriendById(sentMessage.toId) {
            let message = Message(from: nil, date: NSDate(timeIntervalSince1970: NSTimeInterval(sentMessage.timestamp)), message: sentMessage.message)

            appendMessage(message, forFriend: friend)
        }
    }

    @objc private func didReceiveMessage(notification: NSNotification) {
        if let receivedMessage = notification.userInfo?["receivedMessage"] as? ReceivedMessage, let friend = FriendManager.sharedManager.getFriendById(receivedMessage.fromId) {
            let message = Message(from: friend, date: NSDate(timeIntervalSince1970: NSTimeInterval(receivedMessage.timestamp)), message: receivedMessage.message)

            appendMessage(message, forFriend: friend)
        }
    }
}