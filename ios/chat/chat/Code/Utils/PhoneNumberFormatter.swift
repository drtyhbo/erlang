//
//  PhoneNumberFormatter.swift
//  chat
//
//  Created by Andreas Binnewies on 2/23/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation

class PhoneNumberFormatter {
    func formatPhoneNumber(phoneNumber: String) -> String {
        if phoneNumber.characters.count <= 3 {
            return String(format: "(%@",
                phoneNumber.substringWithRange(phoneNumber.startIndex..<phoneNumber.startIndex.advancedBy(phoneNumber.characters.count)))
        } else if phoneNumber.characters.count <= 6 {
            return String(format: "(%@) %@",
                phoneNumber.substringWithRange(phoneNumber.startIndex..<phoneNumber.startIndex.advancedBy(3)),
                phoneNumber.substringWithRange(phoneNumber.startIndex.advancedBy(3)..<phoneNumber.startIndex.advancedBy(phoneNumber.characters.count)))
        } else {
            return String(format: "(%@) %@-%@",
                phoneNumber.substringWithRange(phoneNumber.startIndex..<phoneNumber.startIndex.advancedBy(3)),
                phoneNumber.substringWithRange(phoneNumber.startIndex.advancedBy(3)..<phoneNumber.startIndex.advancedBy(6)),
                phoneNumber.substringWithRange(phoneNumber.startIndex.advancedBy(6)..<phoneNumber.startIndex.advancedBy(phoneNumber.characters.count)))
        }
    }
}