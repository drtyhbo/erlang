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

    static let ChatClientReceivedMessageNotification = "ChatClientReceivedMessage"

    private(set) var state: State = .Disconnected

    private let connection: ChatConnection

    private var messageId = 0

    init() {
        connection = ChatConnection(host: "192.168.1.109", port: 49165)
        connection.delegate = self
    }

    func connect() {
        if let userId = User.userId, sessionToken = User.sessionToken {
            let connectJson = JSON([
                "t": "c",
                "u": userId,
                "s": sessionToken,
            ])
            connection.sendJson(connectJson)
        }
    }

    func sendMessageWithText(text: String, to: Friend) {
        let messageJson = JSON([
            "t": "m",
            "r": to.id,
            "i": messageId++,
            "m": [
                "m": text
            ]
        ])
        connection.sendJson(messageJson)
    }

    private func handleJson(json: JSON) {
        if json["r"] == "connected" {
            state = .Connected
        } else if let messages = json["m"].array {
            handleMessagesJson(messages)
        } else if let offlineMessages = json["o"].array {
            handleMessagesJson(offlineMessages)
        } else {
            print ("unknown json \(json.rawString()!)")
        }
    }

    private func handleMessagesJson(messagesJson: [JSON]) {
        for messageJson in messagesJson {
            let receivedMessage = ReceivedMessage(fromId: messageJson["f"].string!, timestamp: messageJson["d"].int!, message: messageJson["m"]["m"].string!)
            NSNotificationCenter.defaultCenter().postNotificationName(ChatClient.ChatClientReceivedMessageNotification, object: nil, userInfo: ["receivedMessage": receivedMessage])
        }
    }
}

extension ChatClient: ChatConnectionDelegate {
    func chatConnectionOnConnect(chatConnection: ChatConnection) {

    }

    func chatConnectionOnDisconnect(chatConnection: ChatConnection) {
        state = .Disconnected
    }

    func chatConnection(chatConnection: ChatConnection, didReceiveJSON json: JSON) {
        handleJson(json)
    }
}