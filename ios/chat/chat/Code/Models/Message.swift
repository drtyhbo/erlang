//
//  Message.swift
//  chat
//
//  Created by Andreas Binnewies on 1/13/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation

class Message {
    let from: Friend?
    let date: NSDate
    let message: String

    init(from: Friend?, date: NSDate, message: String) {
        self.from = from
        self.date = date
        self.message = message
    }
}