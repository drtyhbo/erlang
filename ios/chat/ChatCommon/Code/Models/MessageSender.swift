//
//  MessageSender.swift
//  chat
//
//  Created by Andreas Binnewies on 2/14/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import SwiftyJSON

public class MessageSender {
    public static let SendingProgressNotification = "MessageSenderSendingProgressNotification"
    public static let SendingCompleteNotification = "MessageSenderSendingCompleteNotification"

    private var messageId = 0
    private var isSending = false

    init() {
        NSNotificationCenter.defaultCenter().addObserver(self, selector: "messageDidSend:", name: ChatClient.ChatClientMessageDidSend, object: nil)
    }

    func sendMessage(message: Message, files: [File] = []) {
        PendingMessage.createWithMessage(message, messageId: messageId++, files: files, secretKey: MessageCrypter.sharedCrypter.sharedSecret())
        CoreData.save()
        maybeSendNextPendingMessage()
    }

    func maybeSendNextPendingMessage() {
        if isSending {
            return
        }

        sendNextPendingMessage()
    }

    private func sendNextPendingMessage() {
        guard let pendingMessage = PendingMessage.nextPendingMessage() else {
            isSending = false
            return
        }

        isSending = true

        if let file = pendingMessage.files.anyObject() as? File {
            uploadFile(file, fromPendingMessage: pendingMessage)
        } else {
            sendPendingMessage(pendingMessage)
        }
    }

    private func uploadFile(file: File, fromPendingMessage pendingMessage: PendingMessage) {
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
                            NSNotificationCenter.defaultCenter().postNotificationName(MessageSender.SendingProgressNotification, object: pendingMessage.message, userInfo: ["percentComplete": percentComplete])
                        }
                    }) {
                    success in
                    if success {
                        pendingMessage.finishFile(file)
                        self.sendNextPendingMessage()
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
            NSNotificationCenter.defaultCenter().postNotificationName(MessageSender.SendingCompleteNotification, object: pendingMessage.message, userInfo: nil)

            pendingMessage.MR_deleteEntity()
            CoreData.save()

            sendNextPendingMessage()
        }
    }
}