//
//  ChatClient.swift
//  chat
//
//  Created by Andreas Binnewies on 1/30/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import SwiftyJSON

public class ChatClient {
    enum State {
        case Disconnected
        case Connected
    }

    public static let sharedClient = ChatClient()

    static let ChatClientReceivedMessagesNotification = "ChatClientReceivedMessages"
    static let ChatClientMessageDidSend = "ChatClientMessageDidSend"
    public static let ChatClientConnectingNotification = "ChatClientConnecting"
    public static let ChatClientDidConnectNotification = "ChatClientDidConnect"
    public static let ChatClientDidDisconnectNotification = "ChatClientDidDisconnect"

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

    public func maybeConnect() {
        if !connection.isConnected {
            connect()
        }
    }

    func sendMessageWithData(data: NSData, toChat chat: Chat, messageId: Int, secretKey: NSData) {
        guard let encryptedMessage = MessageCrypter.sharedCrypter.encryptData(data, withSharedSecret: secretKey) else {
            return
        }

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
                "m": encryptedMessage.base64
            ])

            self.sendJson(messageToSendJson)
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
        if connection.isConnecting {
            return
        }

        NSNotificationCenter.defaultCenter().postNotificationName(ChatClient.ChatClientConnectingNotification, object: nil)
        if let sessionToken = User.sessionToken {
            let connectJson = JSON([
                "t": "c",
                "u": User.userId,
                "s": sessionToken,
            ])
            connection.connect()
            // Don't use self.sendJson() for this.
            connection.sendJson(connectJson)
        }
    }

    private func sendReceivedOfflineMessagesResponse() {
        sendJson(JSON([
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
            guard let fromId = messageJson["f"].int else {
                continue
            }

            var friend: Friend! = Friend.findWithId(fromId)
            if friend == nil {
                friend = Friend.createWithId(fromId, firstName: "", lastName: "")
                CoreData.save()
            }

            let messageContainer = messageJson["m"]
            guard let timestamp = messageJson["d"].int,
                encryptedKeyPayload = messageContainer["k"].object as? [String:AnyObject],
                decryptedKey = MessageCrypter.sharedCrypter.decryptMessage(encryptedKeyPayload, forFriend: friend),
                encryptedMessageBase64 = messageContainer["m"].string,
                encryptedMessageData = NSData.fromBase64(encryptedMessageBase64),
                decryptedMessage = MessageCrypter.sharedCrypter.decryptData(encryptedMessageData, withSharedSecret: decryptedKey) else {
                continue
            }

            self.receivedMessages.append(ReceivedMessage(fromId: fromId, timestamp: timestamp, secretKey: decryptedKey, messageJson: JSON(data: decryptedMessage)))
        }

        self.receivedMessagesTimer?.invalidate()
        self.receivedMessagesTimer = NSTimer.scheduledTimerWithTimeInterval(0.25, target: self, selector: "sendReceivedMessagesNotification", userInfo: nil, repeats: false)
    }

    private func sendJson(json: JSON) {
        if !self.connection.isConnected {
            self.connect()
        }
        self.connection.sendJson(json)
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