//
//  Chat.swift
//  chat
//
//  Created by Andreas Binnewies on 3/5/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import CoreData
import Foundation
import MagicalRecord

@objc(Chat)
class Chat: NSManagedObject {
    typealias ChatId = NSURL

    @NSManaged var participants: NSSet

    override var hashValue: Int {
        return id.hashValue
    }

    var id: ChatId {
        return objectID.URIRepresentation()
    }

    var participantsArray: [Friend] {
        return participants.allObjects as? [Friend] ?? []
    }

    var participantIds: [Int] {
        var participantIds = participantsArray.map({ $0.id })
        participantIds.append(User.userId)
        return participantIds
    }

    var name: String {
        let sortedParticipants = participantsArray.sort({ $0.name < $1.name })
        return sortedParticipants.map({ $0.name }).joinWithSeparator(", ")
    }

    static func createWithParticipants(participants: [Friend]) -> Chat {
        if let chat = findWithFriends(participants) {
            return chat
        }

        let chat = Chat.MR_createEntity()!
        chat.participants = NSSet(array: participants)

        return chat
    }

    static func findWithFriends(var friends: [Friend]) -> Chat? {
        let predicate = NSPredicate(format: "ANY participants == %@", friends[0])
        let chats = Chat.MR_findAllWithPredicate(predicate) as? [Chat] ?? []

        for chat in chats {
            if chat.containsPartipants(friends) {
                return chat
            }
        }
        return nil
    }

    static func findAll() -> [Chat] {
        return Chat.MR_findAll() as! [Chat]
    }

    static func findWithId(id: ChatId) -> Chat? {
        let context = NSManagedObjectContext.MR_defaultContext()
        if let objectId = context.persistentStoreCoordinator?.managedObjectIDForURIRepresentation(id) {
            do {
                return try context.existingObjectWithID(objectId) as? Chat
            } catch {
            }
        }

        return nil
    }

    func containsPartipants(friends: [Friend]) -> Bool {
        if friends.count != participants.count {
            return false
        }

        for friend in friends {
            if !participants.containsObject(friend) {
                return false
            }
        }
        return true
    }
}

func ==(lhs: Chat, rhs: Chat) -> Bool {
    return lhs.id == rhs.id
}