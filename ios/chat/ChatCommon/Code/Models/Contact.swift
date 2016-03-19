//
//  Contact.swift
//  chat
//
//  Created by Andreas Binnewies on 3/18/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation

public class Contact {
    public let name: String
    public let phoneNumber: PhoneNumber

    var firstName: String {
        return name.componentsSeparatedByString(" ")[0]
    }

    var lastName: String? {
        let names = name.componentsSeparatedByString(" ")
        return names.count > 1 ? names.last : nil
    }

    public init(name: String, phoneNumber: String) {
        self.name = name
        self.phoneNumber = PhoneNumber(phoneNumber: phoneNumber)
    }
}