//
//  GroupChat.swift
//  chat
//
//  Created by Andreas Binnewies on 3/5/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import CoreData
import Foundation
import MagicalRecord

@objc(GroupChat)
class GroupChat: NSManagedObject {
    @NSManaged var members: NSSet

    static func createWithMembers(members: [Friend]) -> GroupChat {
        let groupChat = GroupChat.MR_createEntity()!
        groupChat.members = NSSet(array: members)

        return groupChat
    }

    static func findWithFriends(var friends: [Friend]) -> GroupChat? {
        let predicate = NSPredicate(format: "ANY members == %@", friends[0])
        let groupChats = GroupChat.MR_findAllWithPredicate(predicate) as? [GroupChat] ?? []

        for groupChat in groupChats {
            if groupChat.containsMembers(friends) {
                return groupChat
            }
        }
        return nil
    }

    func containsMembers(friends: [Friend]) -> Bool {
        if friends.count != members.count {
            return false
        }

        for friend in friends {
            if !members.containsObject(friend) {
                return false
            }
        }
        return true
    }
}