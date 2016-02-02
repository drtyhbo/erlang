//
//  FriendManager.swift
//  chat
//
//  Created by Andreas Binnewies on 2/2/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation

class FriendManager {
    static let sharedManager = FriendManager()

    private(set) var friends: [Friend] = []

    func loadFriends(completion: [Friend]->Void) {
        APIManager.getFriends {
            friends in

            self.friends = friends
            completion(friends)
        }
    }

    func getFriendById(id: String) -> Friend? {
        for friend in friends {
            if friend.id == id {
                return friend
            }
        }

        return nil
    }
}