//
//  NSMutableData.swift
//  chat
//
//  Created by Andreas Binnewies on 2/29/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation

extension NSMutableData {
    var mutableUInt8Bytes: UnsafeMutablePointer<UInt8> {
        return UnsafeMutablePointer<UInt8>(mutableBytes)
    }
}