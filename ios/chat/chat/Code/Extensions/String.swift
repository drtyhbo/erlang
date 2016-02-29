//
//  String.swift
//  chat
//
//  Created by Andreas Binnewies on 2/29/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation

extension String {
    var utf8Data: NSData {
        return dataUsingEncoding(NSUTF8StringEncoding)!
    }
}