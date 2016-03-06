//
//  FriendManager.swift
//  chat
//
//  Created by Andreas Binnewies on 2/2/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import CoreData
import Foundation
import MagicalRecord

class FriendManager {
    static let sharedManager = FriendManager()

    var friends: [Friend] {
        return Friend.findAll()
    }

    func getFriendById(id: Int) -> Friend? {
        for friend in friends {
            if friend.id == id {
                return friend
            }
        }

        return nil
    }

    func loadFriendsFromContacts(contacts: [Contact], completion: Void->Void) {
        var contactsByPhoneNumber: [String: Contact] = [:]
        for contact in contacts {
            contactsByPhoneNumber[contact.phoneNumber.fullNumber] = contact
        }

        APIManager.sharedManager.getFriendsWithPhoneNumbers(contacts.map({ $0.phoneNumber })) {
            friendsData in

            for i in 0..<friendsData.count {
                let friendData = friendsData[i]

                if let name = contactsByPhoneNumber[friendData.phoneNumber]?.name {
                    if let friend = Friend.findWithId(friendData.id) {
                        friend.name = name
                    } else {
                        Friend.createWithId(friendData.id, name: name)
                    }
                }
            }

            CoreData.save()

            completion()
        }
    }
}