//
//  ChatClient.swift
//  chat
//
//  Created by Andreas Binnewies on 1/30/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import SwiftyJSON

class ChatClient {
    enum State {
        case Disconnected
        case Connected
    }

    static let sharedClient = ChatClient()

    static let ChatClientReceivedMessagesNotification = "ChatClientReceivedMessages"
    static let ChatClientMessageDidSend = "ChatClientMessageDidSend"
    static let ChatClientConnectingNotification = "ChatClientConnecting"
    static let ChatClientDidConnectNotification = "ChatClientDidConnect"
    static let ChatClientDidDisconnectNotification = "ChatClientDidDisconnect"

    private(set) var state: State = .Disconnected

    private let host = Constants.host
    private let port: UInt16 = 49165

    private let connection: ChatConnection

    private var receivedMessages: [ReceivedMessage] = []
    private var receivedMessagesTimer: NSTimer?

    init() {
        connection = ChatConnection(host: host, port: port)
        connection.delegate = self

        NSNotificationCenter.defaultCenter().addObserver(self, selector: "applicationDidBecomeActive", name: UIApplicationDidBecomeActiveNotification, object: nil)
    }

    func maybeConnect() {
        if !connection.isConnected {
            connect()
        }
    }

    func sendMessageWithJson(json: JSON, to: Friend, messageId: Int) -> Bool {
        if let messageToEncrypt = json.rawString(), encryptedMessage = SecurityHelper.sharedHelper.encrypt(messageToEncrypt, publicTag: "com.drtyhbo.\(to.id)", withKey: to.key) {
            let messageJson = JSON([
                "t": "m",
                "r": String(to.id),
                "i": messageId,
                "m": encryptedMessage
            ])

            connection.sendJson(messageJson)

            return true
        }

        return false
    }

    private func connect() {
        NSNotificationCenter.defaultCenter().postNotificationName(ChatClient.ChatClientConnectingNotification, object: nil)
        if let userId = User.userId, sessionToken = User.sessionToken {
            let connectJson = JSON([
                "t": "c",
                "u": userId,
                "s": sessionToken,
            ])
            connection.sendJson(connectJson)
        }
    }

    private func sendReceivedOfflineMessagesResponse() {
        connection.sendJson(JSON([
            "t": "a",
            "w": "offline"
        ]))
    }

    private func handleJson(json: JSON) {
        if json["r"] == "connected" {
            state = .Connected
            NSNotificationCenter.defaultCenter().postNotificationName(ChatClient.ChatClientDidConnectNotification, object: nil)
        } else if let messages = json["m"].array {
            handleMessagesJson(messages)
        } else if let offlineMessages = json["o"].array {
            handleMessagesJson(offlineMessages)
            sendReceivedOfflineMessagesResponse()
        } else if let sentMessageId = json["did"].int {
            NSNotificationCenter.defaultCenter().postNotificationName(ChatClient.ChatClientMessageDidSend, object: nil, userInfo: ["messageId": sentMessageId])
        } else {
            print ("unknown json \(json.rawString()!)")
        }
    }

    private func handleMessagesJson(messagesJson: [JSON]) {
        for messageJson in messagesJson {
            if let fromId = Int(messageJson["f"].string!), timestamp = messageJson["d"].int, encryptedMessage = messageJson["m"].string, decryptedMessage = SecurityHelper.sharedHelper.decrypt(encryptedMessage) {
                self.receivedMessages.append(ReceivedMessage(fromId: fromId, timestamp: timestamp, messageJson: JSON.parse(decryptedMessage)))
            }
        }

        self.receivedMessagesTimer?.invalidate()
        self.receivedMessagesTimer = NSTimer.scheduledTimerWithTimeInterval(0.25, target: self, selector: "sendReceivedMessagesNotification", userInfo: nil, repeats: false)
    }

    @objc private func sendReceivedMessagesNotification() {
        let receivedMessages = self.receivedMessages
        NSNotificationCenter.defaultCenter().postNotificationName(ChatClient.ChatClientReceivedMessagesNotification, object: nil, userInfo: ["receivedMessages": receivedMessages])
        self.receivedMessages = []
    }

    // Notifications

    @objc private func applicationDidBecomeActive() {
        maybeConnect()
    }
}

extension ChatClient: ChatConnectionDelegate {
    func chatConnectionOnConnect(chatConnection: ChatConnection) {

    }

    func chatConnectionOnDisconnect(chatConnection: ChatConnection) {
        state = .Disconnected
        NSNotificationCenter.defaultCenter().postNotificationName(ChatClient.ChatClientDidDisconnectNotification, object: nil)
    }

    func chatConnection(chatConnection: ChatConnection, didReceiveJSON json: JSON) {
        handleJson(json)
    }
}