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
public class Chat: NSManagedObject {
    public typealias ChatId = NSURL

    @NSManaged var participants: NSSet

    public override var hashValue: Int {
        return id.hashValue
    }

    public var id: ChatId {
        return objectID.URIRepresentation()
    }

    public var participantsArray: [Friend] {
        return participants.allObjects as? [Friend] ?? []
    }

    var participantIds: [String] {
        var participantIds = participantsArray.map({ $0.id })
        if let userId = User.userId {
            participantIds.append(userId)
        }
        return participantIds
    }

    public var name: String {
        let sortedParticipants = participantsArray.sort({ $0.fullName < $1.fullName })
        return sortedParticipants.map({ $0.firstName }).joinWithSeparator(", ")
    }

    public static func createWithParticipants(participants: [Friend]) -> Chat {
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

    public static func findAll() -> [Chat] {
        return Chat.MR_findAll() as! [Chat]
    }

    public static func findWithId(id: ChatId) -> Chat? {
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