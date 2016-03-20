//
//  NSUserDefaults.swift
//  chat
//
//  Created by Andreas Binnewies on 3/18/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation

extension NSUserDefaults {
    public static func sharedUserDefaults() -> NSUserDefaults {
        return NSUserDefaults(suiteName: "group.drtyhbo.Chat")!
    }
}