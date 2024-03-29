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

public class FriendManager {
    public static let sharedManager = FriendManager()

    public var friends: [Friend] {
        return Friend.findAll()
    }

    func getFriendById(id: String) -> Friend? {
        for friend in friends {
            if friend.id == id {
                return friend
            }
        }

        return nil
    }

    public func loadFriendsFromContacts(contacts: [Contact], completion: Void->Void) {
        var contactsByPhoneNumber: [String: Contact] = [:]
        for contact in contacts {
            contactsByPhoneNumber[contact.phoneNumber.fullNumber] = contact
        }

        APIManager.sharedManager.getFriendsWithPhoneNumbers(contacts.map({ $0.phoneNumber })) {
            friendsData in

            for i in 0..<friendsData.count {
                let friendData = friendsData[i]

                if let contact = contactsByPhoneNumber[friendData.phoneNumber] {
                    Friend.createWithId(friendData.id, firstName: contact.firstName, lastName: contact.lastName)
                }
            }

            CoreData.save()

            completion()
        }
    }
}