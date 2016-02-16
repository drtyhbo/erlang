//
//  Message.swift
//  chat
//
//  Created by Andreas Binnewies on 1/13/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import CoreData
import Foundation
import MagicalRecord
import SwiftyJSON

@objc(Message)
class Message: NSManagedObject {
    struct ImageInfo {
        let imageId: Int
        let width: Int
        let height: Int

        let thumbnailId: Int
        let thumbnailWidth: Int
        let thumbnailHeight: Int
    }

    @NSManaged private(set) var from: Friend?
    @NSManaged private(set) var to: Friend?
    @NSManaged private(set) var date: NSDate
    @NSManaged private(set) var message: String

    var imageInfo: ImageInfo? {
        let json = JSON.parse(message)
        let imageInfo = json["i"]

        guard let imageId = imageInfo["i"].int, width = imageInfo["w"].int, height = imageInfo["h"].int, thumbnailId = imageInfo["ti"].int, thumbnailWidth = imageInfo["tw"].int, thumbnailHeight = imageInfo["th"].int else {
            return nil
        }

        return ImageInfo(imageId: imageId, width: width, height: height, thumbnailId: thumbnailId, thumbnailWidth: thumbnailWidth, thumbnailHeight: thumbnailHeight)
    }

    static func createWithFrom(from: Friend?, to: Friend?, date: NSDate, messageData: String) -> Message {
        let message = Message.MR_createEntity()!
        message.from = from
        message.to = to
        message.date = date
        message.message = messageData
        return message
    }

    static func findForFriend(friend: Friend) -> [Message] {
        let predicate = NSPredicate(format: "from == %@ || to == %@", friend, friend)
        let fetchRequest = Message.MR_requestAllSortedBy("date", ascending: false, withPredicate: predicate)
        return (Message.MR_executeFetchRequest(fetchRequest) as? [Message] ?? []).reverse()
    }
}