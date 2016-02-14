//
//  MessageSender.swift
//  chat
//
//  Created by Andreas Binnewies on 2/14/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import SwiftyJSON

class MessageSender {
    private struct OutgoingMessage {
        let messageJson: JSON
        let to: Friend
        let messageId: Int
    }

    private var messageId = 0
    private var isSending = false
    private var outgoingMessages: [OutgoingMessage] = []

    init() {
        NSNotificationCenter.defaultCenter().addObserver(self, selector: "messageDidSend:", name: ChatClient.ChatClientMessageDidSend, object: nil)
    }

    func sendMessageWithJson(json: JSON, to: Friend) {
        outgoingMessages.append(OutgoingMessage(messageJson: json, to: to, messageId: messageId++))
        maybeSendNextOutgoingMessage()
    }

    private func maybeSendNextOutgoingMessage() {
        if isSending {
            return
        }

        sendNextOutgoingMessage()
    }

    private func sendNextOutgoingMessage() {
        if outgoingMessages.count == 0 {
            isSending = false
            return
        }

        isSending = true

        let outgoingMessage = outgoingMessages.first!
        ChatClient.sharedClient.sendMessageWithJson(outgoingMessage.messageJson, to: outgoingMessage.to, messageId: outgoingMessage.messageId)
    }

    @objc private func messageDidSend(notification: NSNotification) {
        if let messageId = notification.userInfo?["messageId"] as? Int {
            for i in 0..<outgoingMessages.count {
                if outgoingMessages[i].messageId == messageId {
                    outgoingMessages.removeAtIndex(i)
                    sendNextOutgoingMessage()
                    break
                }
            }
        }
    }
}