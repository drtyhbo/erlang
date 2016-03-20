//
//  PhoneNumber.swift
//  chat
//
//  Created by Andreas Binnewies on 2/13/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation

public class PhoneNumber {
    public var fullNumber: String {
        return "\(countryCode)\(phoneNumber)"
    }

    private(set) var countryCode: String
    public var phoneNumber: String

    public init(phoneNumber: String) {
        countryCode = "1"
        self.phoneNumber = phoneNumber
    }
}