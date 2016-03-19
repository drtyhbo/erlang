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
            return NSUserDefaults.standardUserDefaults().stringForKey(phoneNumberKey)
        }
        set {
            NSUserDefaults.standardUserDefaults().setObject(newValue, forKey: phoneNumberKey)
            NSUserDefaults.standardUserDefaults().synchronize()
        }
    }

    public static var userId: Int {
        get {
            return NSUserDefaults.standardUserDefaults().integerForKey(userIdKey)
        }
        set {
            NSUserDefaults.standardUserDefaults().setInteger(newValue, forKey: userIdKey)
            NSUserDefaults.standardUserDefaults().synchronize()
        }
    }

    public static var sessionToken: String? {
        get {
            return NSUserDefaults.standardUserDefaults().stringForKey(sessionTokenKey)
        }
        set {
            NSUserDefaults.standardUserDefaults().setObject(newValue, forKey: sessionTokenKey)
            NSUserDefaults.standardUserDefaults().synchronize()
        }
    }

    public static var firstName: String? {
        get {
            return NSUserDefaults.standardUserDefaults().stringForKey(firstNameKey)
        }
        set {
            NSUserDefaults.standardUserDefaults().setObject(newValue, forKey: firstNameKey)
            NSUserDefaults.standardUserDefaults().synchronize()
        }
    }

    public static var lastName: String? {
        get {
            return NSUserDefaults.standardUserDefaults().stringForKey(lastNameKey)
        }
        set {
            NSUserDefaults.standardUserDefaults().setObject(newValue, forKey: lastNameKey)
            NSUserDefaults.standardUserDefaults().synchronize()
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