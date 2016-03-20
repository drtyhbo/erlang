//
//  PendingMessage.swift
//  chat
//
//  Created by Andreas Binnewies on 2/19/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import CoreData
import Foundation

@objc(PendingMessage)
class PendingMessage: NSManagedObject {
    @NSManaged private(set) var bytesSent: Int
    @NSManaged private(set) var date: NSDate
    @NSManaged private(set) var messageId: Int
    @NSManaged private(set) var secretKey: NSData
    @NSManaged private(set) var totalBytes: Int

    @NSManaged private(set) var message: Message
    @NSManaged private(set) var files: NSSet

    static func createWithMessage(message: Message, messageId: Int, files: [File], secretKey: NSData) {
        let pendingMessage = PendingMessage.MR_createEntity()!
        pendingMessage.date = message.date
        pendingMessage.message = message
        pendingMessage.messageId = messageId
        pendingMessage.secretKey = secretKey
        pendingMessage.totalBytes = files.reduce(0, combine: {$0 + $1.data.length})
        pendingMessage.files = NSSet(array: files)
    }

    static func isMessagePending(message: Message) -> Bool {
        return PendingMessage.MR_findFirstByAttribute("message", withValue: message) != nil
    }

    static func findWithMessageId(messageId: Int) -> PendingMessage? {
        return PendingMessage.MR_findFirstByAttribute("messageId", withValue: messageId)
    }

    static func nextPendingMessage() -> PendingMessage? {
        return PendingMessage.MR_findAllSortedBy("date", ascending: true)?.first as? PendingMessage
    }

    func finishFile(file: File) {
        bytesSent += file.data.length

        let mutableFiles = mutableSetValueForKey("files")
        mutableFiles.removeObject(file)

        CoreData.save()
    }
}