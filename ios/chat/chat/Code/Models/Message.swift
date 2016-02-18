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
        let imageInfo = json["i"]

        guard let imageId = imageInfo["i"].int, width = imageInfo["w"].int, height = imageInfo["h"].int, thumbnailId = imageInfo["ti"].int, thumbnailWidth = imageInfo["tw"].int, thumbnailHeight = imageInfo["th"].int else {
            return nil
        }

        return ImageInfo(imageId: imageId, width: width, height: height, thumbnailId: thumbnailId, thumbnailWidth: thumbnailWidth, thumbnailHeight: thumbnailHeight)
    }

    var text: String? {
        return json["m"].string
    }

    var json: JSON {
        return JSON.parse(message)
    }

    static func createFromCurrentUserTo(to: Friend?, messageJson: JSON) -> Message {
        return createWithFrom(nil, to: to, date: NSDate(), messageJson: messageJson)
    }

    static func createWithFrom(from: Friend?, to: Friend?, date: NSDate, messageJson: JSON) -> Message {
        let message = Message.MR_createEntity()!
        message.from = from
        message.to = to
        message.date = date
        message.message = messageJson.rawString()!
        return message
    }

    static func findForFriend(friend: Friend, beforeDate: NSDate?, fetchLimit: Int) -> [Message] {
        let predicate = NSPredicate(format: "(from == %@ || to == %@) && date < %@", friend, friend, beforeDate ?? NSDate())
        let fetchRequest = Message.MR_requestAllSortedBy("date", ascending: false, withPredicate: predicate)
        fetchRequest.fetchLimit = fetchLimit
        return (Message.MR_executeFetchRequest(fetchRequest) as? [Message] ?? []).reverse()
    }

    static func createWithText(text: String, to: Friend) -> Message {
        let messageJson = JSON([
            "m": text])
        return createFromCurrentUserTo(to, messageJson: messageJson)
    }

    static func createWithImageFile(imageFile: File, thumbnailFile: File, to: Friend) -> Message? {
        guard let image = imageFile.image, thumbnail = thumbnailFile.image else {
            return nil
        }

        let messageJson = JSON([
            "i": [
                "i": imageFile.id,
                "w": image.size.width,
                "h": image.size.height,
                "ti": thumbnailFile.id,
                "tw": thumbnail.size.width,
                "th": thumbnail.size.height
            ]])
        return createFromCurrentUserTo(to, messageJson: messageJson)
    }
}