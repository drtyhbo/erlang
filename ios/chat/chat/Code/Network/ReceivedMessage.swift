//
//  ReceivedMessage.swift
//  chat
//
//  Created by Andreas Binnewies on 2/2/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation

class ReceivedMessage {
    let fromId: String
    let timestamp: Int
    let message: String

    init(fromId: String, timestamp: Int, message: String) {
        self.fromId = fromId
        self.timestamp = timestamp
        self.message = message
    }
}