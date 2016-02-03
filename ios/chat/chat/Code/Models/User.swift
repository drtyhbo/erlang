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
            return NSUserDefaults.standardUserDefaults().stringForKey(userIdKey)
        }
        set {
            NSUserDefaults.standardUserDefaults().setObject(newValue, forKey: userIdKey)
            NSUserDefaults.standardUserDefaults().synchronize()
        }
    }

    static var sessionToken: String? {
        get {
            return NSUserDefaults.standardUserDefaults().stringForKey(sessionTokenKey)
        }
        set {
            NSUserDefaults.standardUserDefaults().setObject(newValue, forKey: sessionTokenKey)
            NSUserDefaults.standardUserDefaults().synchronize()
        }
    }

    private static let userIdKey = "userId"
    private static let sessionTokenKey = "sessionToken"
}