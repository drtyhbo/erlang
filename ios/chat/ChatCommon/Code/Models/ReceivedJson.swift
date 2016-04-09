//
//  ReceivedJson.swift
//  chat
//
//  Created by Andreas Binnewies on 3/18/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import SwiftyJSON

class ReceivedMessage {
    let fromId: String
    let timestamp: Int
    let secretKey: NSData
    let messageJson: JSON

    init(fromId: String, timestamp: Int, secretKey: NSData, messageJson: JSON) {
        self.fromId = fromId
        self.timestamp = timestamp
        self.secretKey = secretKey
        self.messageJson = messageJson
    }
}