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

    private var fetchCompletionHandler: (Void->Void)?
    private var fetchTimer: NSTimer?

    func application(application: UIApplication, didFinishLaunchingWithOptions launchOptions: [NSObject: AnyObject]?) -> Bool {
        NSUserDefaults.standardUserDefaults().setValue(false, forKey: "_UIConstraintBasedLayoutLogUnsatisfiable")

        MagicalRecord.setupAutoMigratingCoreDataStack()

        MessageManager.sharedManager.setup()
        NSNotificationCenter.defaultCenter().addObserver(self, selector: "didReceiveNewMessagesNotification:", name: MessageManager.NewMessagesNotification, object: nil)

        let rootViewController: UIViewController
        if User.userId == 0 || User.firstName == nil {
            rootViewController = ThemedNavigationController(rootViewController: User.userId == 0 ? PhoneNumberViewController() : UserInfoViewController())
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

        APIManager.sharedManager.registerDeviceToken(tokenString) {
            success in
        }
    }

    func application(application: UIApplication, didReceiveRemoteNotification userInfo: [NSObject : AnyObject]) {
    }

    func application(application: UIApplication, didReceiveRemoteNotification userInfo: [NSObject : AnyObject], fetchCompletionHandler completionHandler: (UIBackgroundFetchResult) -> Void) {
        fetchCompletionHandler = {
            self.fetchTimer?.invalidate()
            self.fetchCompletionHandler = nil
            completionHandler(.NewData)
        }

        fetchTimer?.invalidate()
        fetchTimer = NSTimer.scheduledTimerWithTimeInterval(2, target: self, selector: "fetchTimerDidFire", userInfo: nil, repeats: false)
    }

    func applicationDidBecomeActive(application: UIApplication) {
        UIApplication.sharedApplication().applicationIconBadgeNumber = 0
    }

    @objc private func fetchTimerDidFire() {
        fetchCompletionHandler?()
    }

    @objc private func didReceiveNewMessagesNotification(notification: NSNotification) {
        fetchCompletionHandler?()
    }
}
