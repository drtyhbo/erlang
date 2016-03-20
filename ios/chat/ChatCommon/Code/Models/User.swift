//
//  User.swift
//  chat
//
//  Created by Andreas Binnewies on 1/13/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

public class User {
    public static var phoneNumber: String? {
        get {
            return NSUserDefaults.sharedUserDefaults().stringForKey(phoneNumberKey)
        }
        set {
            let userDefaults = NSUserDefaults.sharedUserDefaults()
            userDefaults.setObject(newValue, forKey: phoneNumberKey)
            userDefaults.synchronize()
        }
    }

    public static var userId: Int {
        get {
            return NSUserDefaults.sharedUserDefaults().integerForKey(userIdKey)
        }
        set {
            let userDefaults = NSUserDefaults.sharedUserDefaults()
            userDefaults.setInteger(newValue, forKey: userIdKey)
            userDefaults.synchronize()
        }
    }

    public static var sessionToken: String? {
        get {
            return NSUserDefaults.sharedUserDefaults().stringForKey(sessionTokenKey)
        }
        set {
            let userDefaults = NSUserDefaults.sharedUserDefaults()
            userDefaults.setObject(newValue, forKey: sessionTokenKey)
            userDefaults.synchronize()
        }
    }

    public static var firstName: String? {
        get {
            return NSUserDefaults.sharedUserDefaults().stringForKey(firstNameKey)
        }
        set {
            let userDefaults = NSUserDefaults.sharedUserDefaults()
            userDefaults.setObject(newValue, forKey: firstNameKey)
            userDefaults.synchronize()
        }
    }

    public static var lastName: String? {
        get {
            return NSUserDefaults.sharedUserDefaults().stringForKey(lastNameKey)
        }
        set {
            let userDefaults = NSUserDefaults.sharedUserDefaults()
            userDefaults.setObject(newValue, forKey: lastNameKey)
            userDefaults.synchronize()
        }
    }

    public static var profilePic: UIImage? {
        get {
            return UIImage(contentsOfFile: profilePicUrl.path!)
        }
        set {
            if let profilePic = newValue {
                let profilePicData = UIImagePNGRepresentation(profilePic)
                profilePicData?.writeToURL(profilePicUrl, atomically: true)
            }
        }
    }

    static private var profilePicUrl: NSURL {
        let documentsDirectoryUrl = NSURL(fileURLWithPath: NSSearchPathForDirectoriesInDomains(.DocumentDirectory, .UserDomainMask, true).last!)
        return documentsDirectoryUrl.URLByAppendingPathComponent("ProfilePic.png")
    }

    private static let phoneNumberKey = "phoneNumber"
    private static let userIdKey = "userId"
    private static let sessionTokenKey = "sessionToken"
    private static let firstNameKey = "firstName"
    private static let lastNameKey = "lastName"

}