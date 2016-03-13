//
//  ChatMessages.swift
//  chat
//
//  Created by Andreas Binnewies on 2/2/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import SwiftyJSON

class ReceivedMessage {
    let fromId: Int
    let timestamp: Int
    let secretKey: NSData
    let messageJson: JSON

    init(fromId: Int, timestamp: Int, secretKey: NSData, messageJson: JSON) {
        self.fromId = fromId
        self.timestamp = timestamp
        self.secretKey = secretKey
        self.messageJson = messageJson
    }
}

class SentMessage {
    let toId: Int
    let timestamp: Int
    let message: JSON

    init(toId: Int, timestamp: Int, message: JSON) {
        self.toId = toId
        self.timestamp = timestamp
        self.message = message
    }
}