//
//  User.swift
//  chat
//
//  Created by Andreas Binnewies on 1/13/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

class User {
    static var userId: String? {
        get {
            return "5"
//            return NSUserDefaults.standardUserDefaults().stringForKey(userIdKey)
        }
        set {
            NSUserDefaults.standardUserDefaults().setObject(newValue, forKey: userIdKey)
            NSUserDefaults.standardUserDefaults().synchronize()
        }
    }

    static var sessionToken: String? {
        get {
            return "46ac41ec736b1371e01d002a615d17092ef816981b87d8f4aa1f266b95ea333c"
//            return NSUserDefaults.standardUserDefaults().stringForKey(sessionTokenKey)
        }
        set {
            NSUserDefaults.standardUserDefaults().setObject(newValue, forKey: sessionTokenKey)
            NSUserDefaults.standardUserDefaults().synchronize()
        }
    }

    private static let userIdKey = "userId"
    private static let sessionTokenKey = "sessionToken"
}