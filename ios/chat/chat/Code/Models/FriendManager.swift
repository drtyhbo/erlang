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

    func loadFriends(phoneNumbers: [PhoneNumber], completion: Void->Void) {
        APIManager.sharedManager.getFriendsWithPhoneNumbers(phoneNumbers) {
            friendsData in

            for friendData in friendsData {
                var friend = Friend.findWithId(friendData.id)
                if let key = NSData(base64EncodedString: friendData.base64Key, options: NSDataBase64DecodingOptions(rawValue: 0)) where friend == nil {
                    friend = Friend.createWithId(friendData.id, name: friendData.name, key: key)
                    self.friends.append(friend!)
                }
            }

            NSManagedObjectContext.MR_defaultContext().MR_saveToPersistentStoreAndWait()

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