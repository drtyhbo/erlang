//
//  NSData.swift
//  chat
//
//  Created by Andreas Binnewies on 2/29/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation

extension NSData {
    var UInt8Bytes: UnsafePointer<UInt8> {
        return UnsafePointer<UInt8>(bytes)
    }

    var utf8String: String? {
        return String(data: self, encoding: NSUTF8StringEncoding)
    }

    var base64: String {
        return base64EncodedStringWithOptions(NSDataBase64EncodingOptions(rawValue: 0))
    }
}