//
//  MessageManager.swift
//  chat
//
//  Created by Andreas Binnewies on 2/2/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation

class MessageManager {
    static let sharedManager = MessageManager()

    static let NewMessageNotification = "NewMessage"

    private(set) var messagesByFriend: [Friend:[Message]] = [:]

    func setup() {
        NSNotificationCenter.defaultCenter().addObserver(self, selector: "didReceiveMessage:", name: ChatClient.ChatClientReceivedMessageNotification, object: nil)
    }

    func messagesForFriend(friend: Friend) -> [Message] {
        return messagesByFriend[friend] ?? []
    }

    func appendMessage(message: Message, forFriend friend: Friend) {
        if messagesByFriend[friend] == nil {
            messagesByFriend[friend] = []
        }
        messagesByFriend[friend]!.append(message)

        NSNotificationCenter.defaultCenter().postNotificationName(MessageManager.NewMessageNotification, object: friend, userInfo: ["message": message])
    }

    @objc private func didReceiveMessage(notification: NSNotification) {
        if let receivedMessage = notification.userInfo?["receivedMessage"] as? ReceivedMessage, let friend = FriendManager.sharedManager.getFriendById(receivedMessage.fromId) {
            let message = Message(from: friend, date: NSDate(timeIntervalSince1970: NSTimeInterval(receivedMessage.timestamp)), message: receivedMessage.message)

            appendMessage(message, forFriend: friend)
        }
    }
}