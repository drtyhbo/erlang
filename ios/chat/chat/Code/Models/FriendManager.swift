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

    private(set) lazy var friends: [Friend] = {
        return Friend.findAll()
    }();

    func loadFriendsFromContacts(contacts: [Contact], completion: Void->Void) {
        var contactsByPhoneNumber: [String: Contact] = [:]
        for contact in contacts {
            contactsByPhoneNumber[contact.phoneNumber.fullNumber] = contact
        }

        APIManager.sharedManager.getFriendsWithPhoneNumbers(contacts.map({ $0.phoneNumber })) {
            friendsData in

            for i in 0..<friendsData.count {
                let friendData = friendsData[i]

                if let key = NSData(base64EncodedString: friendData.base64Key, options: NSDataBase64DecodingOptions(rawValue: 0)), name = contactsByPhoneNumber[friendData.phoneNumber]?.name {
                    if let friend = Friend.findWithId(friendData.id) {
                        friend.key = key
                        friend.name = name
                    } else {
                        let friend = Friend.createWithId(friendData.id, name: name, key: key)
                        self.friends.append(friend)
                    }
                }
            }

            CoreData.save()

            completion()
        }
    }

    func getFriendById(id: Int) -> Friend? {
        for friend in friends {
            if friend.id == id {
                return friend
            }
        }

        return nil
    }
}