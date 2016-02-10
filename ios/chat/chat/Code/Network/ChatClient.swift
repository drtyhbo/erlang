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

    static let ChatClientSentMessageNotification = "ChatClientSentMessage"
    static let ChatClientReceivedMessageNotification = "ChatClientReceivedMessage"
    static let ChatClientConnectingNotification = "ChatClientConnecting"
    static let ChatClientDidConnectNotification = "ChatClientDidConnect"
    static let ChatClientDidDisconnectNotification = "ChatClientDidDisconnect"

    private(set) var state: State = .Disconnected

    private let host = Constants.host
    private let port: UInt16 = 49165

    private let connection: ChatConnection

    private var messageId = 0

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

    func sendMessageWithText(text: String, to: Friend) {
        NSNotificationCenter.defaultCenter().postNotificationName(ChatClient.ChatClientSentMessageNotification, object: nil, userInfo: ["sentMessage": SentMessage(toId: to.id, timestamp: Int(NSDate.timeIntervalSinceReferenceDate()), message: text)])

        let messageJson = JSON([
            "t": "m",
            "r": String(to.id),
            "i": messageId++,
            "m": [
                "m": text
            ]
        ])
        connection.sendJson(messageJson)
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
        } else {
            print ("unknown json \(json.rawString()!)")
        }
    }

    private func handleMessagesJson(messagesJson: [JSON]) {
        for messageJson in messagesJson {
            let receivedMessage = ReceivedMessage(fromId: Int(messageJson["f"].string!)!, timestamp: messageJson["d"].int!, message: messageJson["m"]["m"].string!)
            NSNotificationCenter.defaultCenter().postNotificationName(ChatClient.ChatClientReceivedMessageNotification, object: nil, userInfo: ["receivedMessage": receivedMessage])
        }
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