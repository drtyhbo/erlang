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
public class Message: NSManagedObject {
    public enum Type {
        case Text
        case Image
        case Movie
    }

    public struct ThumbnailInfo {
        public let id: Int
        public let width: Int
        public let height: Int
    }

    public struct MovieInfo {
        public let id: Int
    }

    @NSManaged private(set) var chat: Chat
    @NSManaged public var from: Friend?
    @NSManaged public var date: NSDate
    @NSManaged private(set) var message: String
    @NSManaged public var secretKey: NSData?

    public var localId: String {
        return objectID.URIRepresentation().lastPathComponent ?? ""
    }

    public var type: Type {
        if json["i"].null == nil {
            return .Image
        } else if json["v"].null == nil {
            return .Movie
        } else {
            return .Text
        }
    }

    public var thumbnailInfo: ThumbnailInfo? {
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

    public var movieInfo: MovieInfo? {
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

    public var text: String? {
        return json["m"].string
    }

    private(set) var json = JSON([:])

    static func createFromCurrentUserToChat(chat: Chat, messageJson: JSON) -> Message {
        return createWithFrom(nil, chat: chat, date: NSDate(), secretKey: nil, messageJson: messageJson)
    }

    static func createWithFrom(from: Friend?, chat: Chat, date: NSDate, secretKey: NSData?, messageJson: JSON) -> Message {
        let message = Message.MR_createEntity()!
        message.from = from
        message.chat = chat
        message.date = date
        message.secretKey = secretKey
        message.message = messageJson.rawString()!
        message.json = messageJson
        return message
    }

    static func findForChat(chat: Chat, beforeDate: NSDate?, fetchLimit: Int) -> [Message] {
        let predicate = NSPredicate(format: "chat == %@ && date < %@", chat, beforeDate ?? NSDate())
        let fetchRequest = Message.MR_requestAllSortedBy("date", ascending: false, withPredicate: predicate)
        fetchRequest.fetchLimit = fetchLimit
        return (Message.MR_executeFetchRequest(fetchRequest) as? [Message] ?? []).reverse()
    }

    static func createWithText(text: String, chat: Chat) -> Message {
        let messageJson = JSON([
            "p": chat.participantIds,
            "m": text
        ])
        return createFromCurrentUserToChat(chat, messageJson: messageJson)
    }

    static func createWithImageFile(imageFile: File, thumbnailFile: File, chat: Chat) -> Message? {
        guard let image = imageFile.image, thumbnail = thumbnailFile.image else {
            return nil
        }

        let messageJson = JSON([
            "p": chat.participantIds,
            "i": [
                "i": imageFile.id,
                "w": image.size.width,
                "h": image.size.height,
                "ti": thumbnailFile.id,
                "tw": thumbnail.size.width,
                "th": thumbnail.size.height
            ]])
        return createFromCurrentUserToChat(chat, messageJson: messageJson)
    }

    static func createWithMovieFile(movieFile: File, thumbnailFile: File, chat: Chat) -> Message? {
        guard let thumbnail = thumbnailFile.image else {
            return nil
        }

        let messageJson = JSON([
            "p": chat.participantIds,
            "v": [
                "v": movieFile.id,
                "ti": thumbnailFile.id,
                "tw": thumbnail.size.width,
                "th": thumbnail.size.height
            ]])
        return createFromCurrentUserToChat(chat, messageJson: messageJson)
    }

    public override func awakeFromFetch() {
        json = JSON.parse(message)
    }

    func wrapJsonWithKey(secretKey: NSData) -> JSON {
        var messageJson = json
        messageJson["k"] = JSON(secretKey.base64)
        return messageJson
    }
}