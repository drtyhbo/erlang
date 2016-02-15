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
    let message: String

    init(fromId: Int, timestamp: Int, message: String) {
        self.fromId = fromId
        self.timestamp = timestamp
        self.message = message
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