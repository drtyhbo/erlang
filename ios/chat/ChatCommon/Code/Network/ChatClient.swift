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

    public var timeToReconnect: NSTimeInterval? {
        if let reconnectionTimer = reconnectionTimer {
            let timeRemaining = reconnectionTimer.fireDate.timeIntervalSinceNow
            return timeRemaining < 0 ? nil : timeRemaining
        }
        return nil
    }

    private(set) var state: State = .Disconnected

    private let host = Constants.host
    private let port: UInt16 = 49165

    private let connection: ChatConnection

    private var receivedMessages: [ReceivedMessage] = []
    private var receivedMessagesTimer: NSTimer?

    private var reconnectionTimer: NSTimer?
    private var reconnectionDelay: NSTimeInterval = Constants.Connection.reconnectionTimeInterval

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

        dispatch_group_enter(dispatchGroup)
        DeviceManager.sharedManager.activeDevicesForFriends(chat.participantsArray) { devices in
            for device in devices {
                dispatch_group_enter(dispatchGroup)
                self.createRecipientPayloadForDevice(device, secretKey: secretKey) { payloadJson in
                    if let payloadJson = payloadJson {
                        recipients.append(payloadJson)
                    }
                    dispatch_group_leave(dispatchGroup)
                }
            }

            dispatch_group_leave(dispatchGroup)
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

    private func createRecipientPayloadForDevice(device: Device, secretKey: NSData, callback: [String:AnyObject]?->Void) {
        MessageCrypter.sharedCrypter.encryptData(secretKey, forDevice: device) { secretKeyMessage in
            guard let secretKeyMessage = secretKeyMessage else {
                callback(nil)
                return
            }

            callback([
                "r": device.id,
                "k": secretKeyMessage])
        }
    }

    private func connect() {
        if connection.isConnecting {
            return
        }

        reconnectionTimer?.invalidate()
        reconnectionTimer = nil

        NSNotificationCenter.defaultCenter().postNotificationName(ChatClient.ChatClientConnectingNotification, object: nil)
        if User.sessionToken != nil {
            connection.connect()
        }
    }

    private func scheduleReconnect() {
        if let reconnectionTimer = reconnectionTimer where reconnectionTimer.valid {
            return
        }

        reconnectionTimer?.invalidate()
        reconnectionTimer = NSTimer.scheduledTimerWithTimeInterval(reconnectionDelay, target: self, selector: "reconnect", userInfo: nil, repeats: false)
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
            reconnectionDelay = Constants.Connection.reconnectionTimeInterval
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
            guard let fromDeviceId = messageJson["f"].int, fromUserId = messageJson["fu"].int else {
                continue
            }

            var friend: Friend! = Friend.findWithId(fromUserId)
            if friend == nil {
                friend = Friend.createWithId(fromUserId, firstName: "", lastName: "")
            }
            let device = Device.createWithId(fromDeviceId, owner: friend)

            let messageContainer = messageJson["m"]
            guard let timestamp = messageJson["d"].int,
                encryptedKeyPayload = messageContainer["k"].object as? [String:AnyObject],
                decryptedKey = MessageCrypter.sharedCrypter.decryptMessage(encryptedKeyPayload, forDevice: device),
                encryptedMessageBase64 = messageContainer["m"].string,
                encryptedMessageData = NSData.fromBase64(encryptedMessageBase64),
                decryptedMessage = MessageCrypter.sharedCrypter.decryptData(encryptedMessageData, withSharedSecret: decryptedKey) else {
                continue
            }

            self.receivedMessages.append(ReceivedMessage(fromId: fromUserId, timestamp: timestamp, secretKey: decryptedKey, messageJson: JSON(data: decryptedMessage)))
        }

        CoreData.save()

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

    @objc private func reconnect() {
        reconnectionDelay *= 2
        maybeConnect()
    }
}

extension ChatClient: ChatConnectionDelegate {
    func chatConnectionOnConnect(chatConnection: ChatConnection) {
        guard let sessionToken = User.sessionToken else {
            return
        }

        sendJson(JSON([
            "t": "c",
            "d": User.deviceId,
            "s": sessionToken,
        ]))
    }

    func chatConnectionOnDisconnect(chatConnection: ChatConnection) {
        state = .Disconnected
        NSNotificationCenter.defaultCenter().postNotificationName(ChatClient.ChatClientDidDisconnectNotification, object: nil)
        scheduleReconnect()
    }

    func chatConnection(chatConnection: ChatConnection, didReceiveJSON json: JSON) {
        handleJson(json)
    }
}