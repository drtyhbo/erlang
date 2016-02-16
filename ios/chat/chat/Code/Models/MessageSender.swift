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
    private class OutgoingMessage {
        let message: Message
        let messageId: Int
        var files: [File]

        init(message: Message, messageId: Int, files: [File]) {
            self.message = message
            self.messageId = messageId
            self.files = files
        }
    }

    private var messageId = 0
    private var isSending = false
    private var outgoingMessages: [OutgoingMessage] = []

    init() {
        NSNotificationCenter.defaultCenter().addObserver(self, selector: "messageDidSend:", name: ChatClient.ChatClientMessageDidSend, object: nil)
    }

    func sendMessage(message: Message, files: [File] = []) {
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
        if outgoingMessage.files.count > 0 {
            uploadFile(outgoingMessage.files.first!)
        } else {
            sendOutgoingMessage(outgoingMessage)
        }
    }

    private func writeDataToTemporaryFile(data: NSData) -> NSURL {
        let tempUrl = NSURL(fileURLWithPath: NSTemporaryDirectory()).URLByAppendingPathComponent(NSUUID().UUIDString)
        data.writeToURL(tempUrl, atomically: true)
        return tempUrl
    }

    private func uploadFile(file: File) {
        APIManager.sharedManager.getUrlForFileWithId(file.id, method: "PUT", contentType: file.contentType) {
            uploadUrl in

            if let uploadUrl = uploadUrl {
                let tempUrl = self.writeDataToTemporaryFile(file.data)
                APIManager.sharedManager.uploadFileWithLocalUrl(tempUrl, toS3Url: uploadUrl, contentType: file.contentType) {
                    success in
                    if success {
                        if let outgoingMessage = self.outgoingMessages.first {
                            outgoingMessage.files.removeAtIndex(0)
                        }
                        self.sendNextOutgoingMessage()
                    }
                }
            }
        }
    }

    private func sendOutgoingMessage(outgoingMessage: OutgoingMessage) {
        let message = outgoingMessage.message
        ChatClient.sharedClient.sendMessageWithJson(message.json, to: message.to!, messageId: outgoingMessage.messageId)
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