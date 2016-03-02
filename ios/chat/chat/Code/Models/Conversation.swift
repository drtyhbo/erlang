//
//  Conversation.swift
//  chat
//
//  Created by Andreas Binnewies on 3/1/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import CoreData
import Foundation

@objc(Conversation)
class Conversation: NSManagedObject {
    @NSManaged var isRatcheting: Bool
    @NSManaged var messageNumber: Int
    @NSManaged var preKeyIndex: Int
    @NSManaged var publicKey: NSData?
    @NSManaged var friend: Friend

    static func createWithFriend(friend: Friend) -> Conversation {
        let conversation = Conversation.MR_createEntity()!
        conversation.friend = friend
        return conversation
    }

    static func getOrCreateWithFriend(friend: Friend) -> Conversation {
        if let conversation = Conversation.findWithFriend(friend) {
            return conversation
        }
        return createWithFriend(friend)
    }

    static func findWithFriend(friend: Friend) -> Conversation? {
        return Conversation.MR_findFirstByAttribute("friend", withValue: friend)
    }
}