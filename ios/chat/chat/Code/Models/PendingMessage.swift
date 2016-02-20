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
    @NSManaged var date: NSDate
    @NSManaged var message: Message

    static func createWithMessage(message: Message) {
        let pendingMessage = PendingMessage.MR_createEntity()!
        pendingMessage.date = NSDate()
        pendingMessage.message = message
    }

    static func isMessagePending(message: Message) -> Bool {
        return PendingMessage.MR_findFirstByAttribute("message", withValue: message) != nil
    }

    static func deletePendingMessage(message: Message) {
        let predicate = NSPredicate(format: "message == %@", message)
        PendingMessage.MR_deleteAllMatchingPredicate(predicate)
    }
}