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

    func loadFriends(phoneNumbers: [String], completion: Void->Void) {
        APIManager.getFriendsWithPhoneNumbers(phoneNumbers) {
            friendsData in

            for friendData in friendsData {
                var friend = Friend.findWithId(friendData.id)
                if friend == nil {
                    friend = Friend.createWithId(friendData.id, name: friendData.name)
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