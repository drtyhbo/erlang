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
    struct File {
        let localUrl: NSURL
        let contentType: String
        let fileId: Int
        let uploadUrl: NSURL

        init(data: NSData, contentType: String, fileId: Int, uploadUrl: NSURL) {
            self.fileId = fileId
            self.contentType = contentType
            self.uploadUrl = uploadUrl

            let documentsDirectory = NSFileManager.defaultManager().URLsForDirectory(.DocumentDirectory, inDomains: .UserDomainMask).first!
            localUrl = documentsDirectory.URLByAppendingPathComponent(NSUUID().UUIDString)
            data.writeToURL(localUrl, atomically: true)
        }
    }

    private class OutgoingMessage {
        let messageJson: JSON
        let to: Friend
        let messageId: Int
        var files: [File]

        init(messageJson: JSON, to: Friend, messageId: Int, files: [File]) {
            self.messageJson = messageJson
            self.to = to
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

    func sendMessageWithJson(json: JSON, to: Friend, files: [File] = []) {
        outgoingMessages.append(OutgoingMessage(messageJson: json, to: to, messageId: messageId++, files: files))
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

    private func uploadFile(file: File) {
        APIManager.sharedManager.uploadFileWithLocalUrl(file.localUrl, toS3Url: file.uploadUrl, contentType: file.contentType) {
            success in
            if success {
                if let outgoingMessage = self.outgoingMessages.first {
                    outgoingMessage.files.removeAtIndex(0)
                }
                self.sendNextOutgoingMessage()
            }
        }
    }

    private func sendOutgoingMessage(outgoingMessage: OutgoingMessage) {
        ChatClient.sharedClient.sendMessageWithJson(outgoingMessage.messageJson, to: outgoingMessage.to, messageId: outgoingMessage.messageId)
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