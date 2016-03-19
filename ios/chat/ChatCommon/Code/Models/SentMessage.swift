//
//  SentMessage.swift
//  chat
//
//  Created by Andreas Binnewies on 3/18/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import SwiftyJSON

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