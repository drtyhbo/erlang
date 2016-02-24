//
//  PhoneNumber.swift
//  chat
//
//  Created by Andreas Binnewies on 2/13/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation

class PhoneNumber {
    var fullNumber: String {
        return "\(countryCode)\(phoneNumber)"
    }

    private(set) var countryCode: String
    private(set) var phoneNumber: String

    init(phoneNumber: String) {
        countryCode = "1"
        self.phoneNumber = phoneNumber
    }
}