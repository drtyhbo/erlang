//
//  AppDelegate.swift
//  chat
//
//  Created by Andreas Binnewies on 1/13/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import MagicalRecord
import UIKit

@UIApplicationMain
class AppDelegate: UIResponder, UIApplicationDelegate {
    var window: UIWindow?

    func application(application: UIApplication, didFinishLaunchingWithOptions launchOptions: [NSObject: AnyObject]?) -> Bool {
        MagicalRecord.setupAutoMigratingCoreDataStack()

        MessageManager.sharedManager.setup()

        let rootViewController: UIViewController
        if User.userId == nil {
            rootViewController = UINavigationController(rootViewController: PhoneNumberViewController())
        } else {
            rootViewController = MainViewController()
        }

        window = UIWindow(frame: UIScreen.mainScreen().bounds)
        window?.rootViewController = rootViewController
        window?.makeKeyAndVisible()

        return true
    }

    func application(application: UIApplication, didRegisterForRemoteNotificationsWithDeviceToken deviceToken: NSData) {
        // http://stackoverflow.com/a/24979960
        let tokenChars = UnsafePointer<CChar>(deviceToken.bytes)
        var tokenString = ""

        for i in 0..<deviceToken.length {
            tokenString += String(format: "%02.2hhx", arguments: [tokenChars[i]])
        }

        APIManager.registerDeviceToken(tokenString) {
            success in
            print (success)
        }
    }

    func application(application: UIApplication, didReceiveRemoteNotification userInfo: [NSObject : AnyObject]) {
    }

    func applicationDidBecomeActive(application: UIApplication) {
    }
}
