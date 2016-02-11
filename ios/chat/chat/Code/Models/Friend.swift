//
//  Friend.swift
//  chat
//
//  Created by Andreas Binnewies on 2/1/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import CoreData
import Foundation

@objc(Friend)
class Friend: NSManagedObject {
    @NSManaged var id: Int
    @NSManaged var name: String
    @NSManaged var key: NSData

    static func createWithId(id: Int, name: String, key: NSData) -> Friend {
        let friend = Friend.MR_createEntity()!
        friend.id = id
        friend.name = name
        friend.key = key
        return friend
    }

    static func findWithId(id: Int) -> Friend? {
        return Friend.MR_findFirstByAttribute("id", withValue: id)
    }

    static func findAll() -> [Friend] {
        return Friend.MR_findAllSortedBy("name", ascending: true) as? [Friend] ?? []
    }
}

func ==(lhs: Friend, rhs: Friend) -> Bool {
    return lhs.id == rhs.id
}