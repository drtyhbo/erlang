//
//  MessageSender.swift
//  chat
//
//  Created by Andreas Binnewies on 2/14/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import SwiftyJSON

protocol MessageSenderDelegate: class {
    func messageSender(messageSender: MessageSender, message: Message, didUpdateProgress progress: Float)
    func messageSender(messageSender: MessageSender, didFinishSendingMessage message: Message)
    func messageSenderDidFinishSendingAllMessages(messageSender: MessageSender)
}

class MessageSender {
    weak var delegate: MessageSenderDelegate?

    var isSending: Bool {
        return currentPendingMessage != nil
    }

    private var currentPendingMessage: PendingMessage?
    private var messageId = 0

    init() {
        NSNotificationCenter.defaultCenter().addObserver(self, selector: "messageDidSend:", name: ChatClient.ChatClientMessageDidSend, object: nil)
        NSNotificationCenter.defaultCenter().addObserver(self, selector: "clientDidConnect", name: ChatClient.ChatClientDidConnectNotification, object: nil)
        NSNotificationCenter.defaultCenter().addObserver(self, selector: "clientDidDisconnect", name: ChatClient.ChatClientDidDisconnectNotification, object: nil)
    }

    func sendMessage(message: Message, files: [File] = []) {
        PendingMessage.createWithMessage(message, messageId: messageId++, files: files, secretKey: MessageCrypter.sharedCrypter.sharedSecret())
        CoreData.save()
        sendNextPendingMessage()
    }

    private func sendNextPendingMessage() {
        if let currentPendingMessage = currentPendingMessage where currentPendingMessage.files.count == 0 {
            return
        }

        guard let pendingMessage = PendingMessage.nextPendingMessage() else {
            delegate?.messageSenderDidFinishSendingAllMessages(self)
            return
        }

        currentPendingMessage = pendingMessage
        handlePendingMessage(pendingMessage)
    }

    private func handlePendingMessage(pendingMessage: PendingMessage) {
        if pendingMessage.files == 0 {
            sendPendingMessage(pendingMessage)
        } else {
            uploadNextFileFromPendingMessage(pendingMessage)
        }
    }

    private func uploadNextFileFromPendingMessage(pendingMessage: PendingMessage) {
        guard let file = pendingMessage.files.anyObject() as? File else {
            sendPendingMessage(pendingMessage)
            return
        }

        APIManager.sharedManager.getUrlForFileWithId(file.id, method: "PUT", contentType: file.contentType) {
            uploadUrl in

            if let uploadUrl = uploadUrl, encryptedFileData = MessageCrypter.sharedCrypter.encryptData(file.data, withSharedSecret: pendingMessage.secretKey) {
                APIManager.sharedManager.uploadData(
                    encryptedFileData,
                    toS3Url: uploadUrl,
                    contentType: file.contentType,
                    progressCallback: {
                        bytesSent, totalBytes in
                        dispatch_async(dispatch_get_main_queue()) {
                            let percentComplete = Float(pendingMessage.bytesSent + bytesSent) / Float(pendingMessage.totalBytes)
                            self.delegate?.messageSender(self, message: pendingMessage.message, didUpdateProgress: percentComplete)
                        }
                    }) {
                    success in
                    if success {
                        pendingMessage.finishFile(file)
                        self.handlePendingMessage(pendingMessage)
                    }
                }
            }
        }
    }

    private func sendPendingMessage(pendingMessage: PendingMessage) {
        let message = pendingMessage.message
        ChatClient.sharedClient.sendMessageWithData(message.message.utf8Data, toChat: message.chat, messageId: pendingMessage.messageId, secretKey: pendingMessage.secretKey)
    }

    @objc private func messageDidSend(notification: NSNotification) {
        if let messageId = notification.userInfo?["messageId"] as? Int, let pendingMessage = PendingMessage.findWithMessageId(messageId) {
            delegate?.messageSender(self, didFinishSendingMessage: pendingMessage.message)

            pendingMessage.MR_deleteEntity()
            CoreData.save()

            if let currentPendingMessage = currentPendingMessage where currentPendingMessage == pendingMessage {
                self.currentPendingMessage = nil
                sendNextPendingMessage()
            }
        }
    }

    @objc private func clientDidConnect() {
        sendNextPendingMessage()
    }

    @objc private func clientDidDisconnect() {
        currentPendingMessage = nil
    }
}