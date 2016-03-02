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
    static let SendingProgressNotification = "MessageSenderSendingProgressNotification"
    static let SendingCompleteNotification = "MessageSenderSendingCompleteNotification"

    private class OutgoingMessage {
        let message: Message
        let messageId: Int
        private(set) var files: [File]
        private(set) var bytesSent: Int
        private(set) var totalBytes: Int

        private let secretKey: NSData?

        init(message: Message, messageId: Int, files: [File]) {
            self.message = message
            self.messageId = messageId
            self.files = files
            bytesSent = 0
            totalBytes = files.reduce(0, combine: {$0 + $1.data.length})
            secretKey = files.count > 0 ? MessageCrypter.sharedCrypter.sharedSecret() : nil
        }

        func finishFileAtIndex(fileIndex: Int) {
            bytesSent += files[fileIndex].data.length
            files.removeAtIndex(fileIndex)
        }
    }

    private var messageId = 0
    private var isSending = false
    private var outgoingMessages: [OutgoingMessage] = []

    init() {
        NSNotificationCenter.defaultCenter().addObserver(self, selector: "messageDidSend:", name: ChatClient.ChatClientMessageDidSend, object: nil)
    }

    func sendMessage(message: Message, files: [File] = []) {
        PendingMessage.createWithMessage(message)
        outgoingMessages.append(OutgoingMessage(message: message, messageId: messageId++, files: files))
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
        if let secretKey = outgoingMessage.secretKey where outgoingMessage.files.count > 0 {
            uploadFile(outgoingMessage.files.first!, toUser: outgoingMessage.message.to!, withSecretKey: secretKey)
        } else {
            sendOutgoingMessage(outgoingMessage)
        }
    }

    private func uploadFile(file: File, toUser to: Friend, withSecretKey secretKey: MessageCrypter.SharedSecret) {
        APIManager.sharedManager.getUrlForFileWithId(file.id, method: "PUT", contentType: file.contentType) {
            uploadUrl in

            if let uploadUrl = uploadUrl, encryptedFileData = MessageCrypter.sharedCrypter.encryptData(file.data, withSharedSecret: secretKey) {
                APIManager.sharedManager.uploadData(
                    encryptedFileData,
                    toS3Url: uploadUrl,
                    contentType: file.contentType,
                    progressCallback: {
                        bytesSent, totalBytes in
                        guard let outgoingMessage = self.outgoingMessages.first else {
                            return
                        }

                        dispatch_async(dispatch_get_main_queue()) {
                            let percentComplete = Float(outgoingMessage.bytesSent + bytesSent) / Float(outgoingMessage.totalBytes)
                            NSNotificationCenter.defaultCenter().postNotificationName(MessageSender.SendingProgressNotification, object: outgoingMessage.message, userInfo: ["percentComplete": percentComplete])
                        }
                    }) {
                    success in
                    if success {
                        if let outgoingMessage = self.outgoingMessages.first {
                            outgoingMessage.finishFileAtIndex(0)
                        }
                        self.sendNextOutgoingMessage()
                    }
                }
            }
        }
    }

    private func sendOutgoingMessage(outgoingMessage: OutgoingMessage) {
        let message = outgoingMessage.message
        let messageJson = outgoingMessage.secretKey != nil ? message.wrapJsonWithKey(outgoingMessage.secretKey!) : message.json
        ChatClient.sharedClient.sendMessageWithJson(messageJson, to: message.to!, messageId: outgoingMessage.messageId)
    }

    @objc private func messageDidSend(notification: NSNotification) {
        if let messageId = notification.userInfo?["messageId"] as? Int {
            for i in 0..<outgoingMessages.count {
                if outgoingMessages[i].messageId == messageId {
                    PendingMessage.deletePendingMessage(outgoingMessages[i].message)
                    CoreData.save()

                    NSNotificationCenter.defaultCenter().postNotificationName(MessageSender.SendingCompleteNotification, object: outgoingMessages[i].message, userInfo: nil)

                    outgoingMessages.removeAtIndex(i)
                    sendNextOutgoingMessage()
                    break
                }
            }
        }
    }
}