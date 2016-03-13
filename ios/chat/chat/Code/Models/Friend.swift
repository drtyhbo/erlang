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
    @NSManaged var firstName: String
    @NSManaged var lastName: String?

    var fullName: String {
        return lastName != nil ? "\(firstName) \(lastName!)" : firstName
    }

    var profilePicUrl: NSURL {
        return Constants.profilePicBaseUrl.URLByAppendingPathComponent("\(id)")
    }

    override var hashValue: Int {
        return id
    }

    static func createWithId(id: Int, firstName: String, lastName: String?) -> Friend {
        if let friend = findWithId(id) {
            friend.firstName = firstName
            friend.lastName = lastName
            return friend
        }

        let friend = Friend.MR_createEntity()!
        friend.id = id
        friend.firstName = firstName
        friend.lastName = lastName
        return friend
    }

    static func findWithId(id: Int) -> Friend? {
        return Friend.MR_findFirstByAttribute("id", withValue: id)
    }

    static func findAll() -> [Friend] {
        return Friend.MR_findAllSortedBy("firstName", ascending: true) as? [Friend] ?? []
    }
}

func ==(lhs: Friend, rhs: Friend) -> Bool {
    return lhs.id == rhs.id
}