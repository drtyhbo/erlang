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
    enum Type {
        case Text
        case Image
        case Movie
    }

    struct ThumbnailInfo {
        let id: Int
        let width: Int
        let height: Int
    }

    struct MovieInfo {
        let id: Int
    }

    @NSManaged private(set) var from: Friend?
    @NSManaged private(set) var to: Friend?
    @NSManaged private(set) var date: NSDate
    @NSManaged private(set) var message: String

    var localId: String {
        return objectID.URIRepresentation().lastPathComponent ?? ""
    }

    var type: Type {
        if json["i"].null == nil {
            return .Image
        } else if json["v"].null == nil {
            return .Movie
        } else {
            return .Text
        }
    }

    var thumbnailInfo: ThumbnailInfo? {
        let thumbnailJson: JSON
        switch(type) {
            case .Image:
                thumbnailJson = json["i"]
            case .Movie:
                thumbnailJson = json["v"]
            default:
                return nil
        }

        guard let thumbnailId = thumbnailJson["ti"].int, thumbnailWidth = thumbnailJson["tw"].int, thumbnailHeight = thumbnailJson["th"].int else {
            return nil
        }

        return ThumbnailInfo(id: thumbnailId, width: thumbnailWidth, height: thumbnailHeight)
    }

    var movieInfo: MovieInfo? {
        let movieJson: JSON
        switch(type) {
            case .Movie:
                movieJson = json["v"]
            default:
                return nil
        }

        guard let movieId = movieJson["v"].int else {
            return nil
        }

        return MovieInfo(id: movieId)
    }

    var text: String? {
        return json["m"].string
    }

    private(set) var json = JSON([:])

    static func createFromCurrentUserTo(to: Friend?, messageJson: JSON) -> Message {
        return createWithFrom(nil, to: to, date: NSDate(), messageJson: messageJson)
    }

    static func createWithFrom(from: Friend?, to: Friend?, date: NSDate, messageJson: JSON) -> Message {
        let message = Message.MR_createEntity()!
        message.from = from
        message.to = to
        message.date = date
        message.message = messageJson.rawString()!
        message.json = messageJson
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

    static func createWithMovieFile(movieFile: File, thumbnailFile: File, to: Friend) -> Message? {
        guard let thumbnail = thumbnailFile.image else {
            return nil
        }

        let messageJson = JSON([
            "v": [
                "v": movieFile.id,
                "ti": thumbnailFile.id,
                "tw": thumbnail.size.width,
                "th": thumbnail.size.height
            ]])
        return createFromCurrentUserTo(to, messageJson: messageJson)
    }

    override func awakeFromFetch() {
        json = JSON.parse(message)
    }
}