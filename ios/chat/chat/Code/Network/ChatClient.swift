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

    func sendMessageWithData(data: NSData, toChat chat: Chat, messageId: Int, secretKey: NSData) {
        let dispatchGroup = dispatch_group_create()

        var recipients:[[String:AnyObject]] = []
        for participant in chat.participantsArray {
            dispatch_group_enter(dispatchGroup)
            createRecipientPayloadForFriend(participant, secretKey: secretKey) { payloadJson in
                guard let payloadJson = payloadJson else {
                    dispatch_group_leave(dispatchGroup)
                    return
                }

                recipients.append(payloadJson)
                dispatch_group_leave(dispatchGroup)
            }
        }

        dispatch_group_notify(dispatchGroup, dispatch_get_main_queue()) {
            let messageToSendJson = JSON([
                "t": "m",
                "r": recipients,
                "i": messageId,
                "m": data.base64
            ])

            print (messageToSendJson.rawString() ?? "")
            self.connection.sendJson(messageToSendJson)
        }
    }

    private func createRecipientPayloadForFriend(friend: Friend, secretKey: NSData, callback: [String:AnyObject]?->Void) {
        MessageCrypter.sharedCrypter.encryptData(secretKey, forFriend: friend) { secretKeyMessage in
            guard let secretKeyMessage = secretKeyMessage else {
                callback(nil)
                return
            }

            callback([
                "r": friend.id,
                "k": secretKeyMessage])
        }
    }

    private func connect() {
        NSNotificationCenter.defaultCenter().postNotificationName(ChatClient.ChatClientConnectingNotification, object: nil)
        if let sessionToken = User.sessionToken {
            let connectJson = JSON([
                "t": "c",
                "u": User.userId,
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

    // TODO: Clean up this code
    private func handleMessagesJson(messagesJson: [JSON]) {
        for messageJson in messagesJson {
            let messageContainer = messageJson["m"]
            if let fromId = messageJson["f"].int,
                friend = Friend.findWithId(fromId),
                timestamp = messageJson["d"].int,
                encryptedKeyPayload = messageContainer["k"].object as? [String:AnyObject],
                decryptedKey = MessageCrypter.sharedCrypter.decryptMessage(encryptedKeyPayload, forFriend: friend),
                encryptedMessageBase64 = messageContainer["m"].string,
                encryptedMessageData = NSData.fromBase64(encryptedMessageBase64),
                decryptedMessage = MessageCrypter.sharedCrypter.decryptData(encryptedMessageData, withSharedSecret: decryptedKey) {
                self.receivedMessages.append(ReceivedMessage(fromId: fromId, timestamp: timestamp, messageJson: JSON(data: decryptedMessage)))
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