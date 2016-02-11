//
//  SecurityHelper.swift
//  chat
//
//  Created by Andreas Binnewies on 2/11/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import Heimdall

class SecurityHelper {
    static let sharedHelper = SecurityHelper()

    var publicKey: String? {
        if let heimdall = Heimdall(tagPrefix: "com.dryhbo"), publicKeyData = heimdall.publicKeyDataX509() {
            return publicKeyData.base64EncodedStringWithOptions(NSDataBase64EncodingOptions(rawValue: 0))
        } else {
            return nil
        }
    }

    func encrypt(stringToEncrypt: String, withKey key: NSData) -> String? {
        if let heimdall = Heimdall(publicTag: "com.drtyhbo", publicKeyData: key) {
            return heimdall.encrypt(stringToEncrypt)
        } else {
            return nil
        }
    }

    func decrypt(stringToDecrypt: String) -> String? {
        if let heimdall = Heimdall(tagPrefix: "com.dryhbo") {
            return heimdall.decrypt(stringToDecrypt)
        } else {
            return nil
        }
    }
}