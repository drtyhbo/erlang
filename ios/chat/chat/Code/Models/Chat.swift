//
//  Chat.swift
//  chat
//
//  Created by Andreas Binnewies on 3/5/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import CoreData
import Foundation
import MagicalRecord

@objc(Chat)
class Chat: NSManagedObject {
    @NSManaged var participants: NSSet

    static func createWithParticipants(participants: [Friend]) -> Chat {
        if let chat = findWithFriends(participants) {
            return chat
        }

        let chat = Chat.MR_createEntity()!
        chat.participants = NSSet(array: participants)

        return chat
    }

    static func findWithFriends(var friends: [Friend]) -> Chat? {
        let predicate = NSPredicate(format: "ANY participants == %@", friends[0])
        let chats = Chat.MR_findAllWithPredicate(predicate) as? [Chat] ?? []

        for chat in chats {
            if chat.containsPartipants(friends) {
                return chat
            }
        }
        return nil
    }

    static func findAll() -> [Chat] {
        print (Chat.MR_findAll())
        return Chat.MR_findAll() as! [Chat]
    }

    func containsPartipants(friends: [Friend]) -> Bool {
        if friends.count != participants.count {
            return false
        }

        for friend in friends {
            if !participants.containsObject(friend) {
                return false
            }
        }
        return true
    }
}