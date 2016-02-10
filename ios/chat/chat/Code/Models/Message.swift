//
//  Message.swift
//  chat
//
//  Created by Andreas Binnewies on 1/13/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import CoreData
import Foundation
import MagicalRecord

@objc(Message)
class Message: NSManagedObject {
    @NSManaged private(set) var from: Friend?
    @NSManaged private(set) var to: Friend?
    @NSManaged private(set) var date: NSDate
    @NSManaged private(set) var message: String

    static func createWithFrom(from: Friend?, to: Friend?, date: NSDate, messageData: String) -> Message {
        let message = Message.MR_createEntity()!
        message.from = from
        message.to = to
        message.date = date
        message.message = messageData
        return message
    }

    static func findForFriend(friend: Friend) -> [Message] {
        let predicate = NSPredicate(format: "from == %@ || to == %@", friend, friend)
        let fetchRequest = Message.MR_requestAllSortedBy("date", ascending: false, withPredicate: predicate)
        return (Message.MR_executeFetchRequest(fetchRequest) as? [Message] ?? []).reverse()
    }
}