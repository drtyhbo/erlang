//
//  Contact.swift
//  chat
//
//  Created by Andreas Binnewies on 2/1/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation

class Contact {
    let name: String
    let phoneNumber: String
    var userExists = false
    var requestSent = false

    init(name: String, phoneNumber: String) {
        self.name = name
        self.phoneNumber = phoneNumber
    }
}
