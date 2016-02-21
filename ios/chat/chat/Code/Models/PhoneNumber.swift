//
//  PhoneNumber.swift
//  chat
//
//  Created by Andreas Binnewies on 2/13/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation

class PhoneNumber {
    private var phoneNumber: String

    init(phoneNumber: String) {
        self.phoneNumber = phoneNumber.characters.count != 11 ? ("1" + phoneNumber) : phoneNumber
    }

    func toString() -> String {
        return phoneNumber
    }
}