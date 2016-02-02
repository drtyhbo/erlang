//
//  AppDelegate.swift
//  chat
//
//  Created by Andreas Binnewies on 1/13/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import APLSlideMenu
import UIKit

@UIApplicationMain
class AppDelegate: UIResponder, UIApplicationDelegate {
    var window: UIWindow?

    func application(application: UIApplication, didFinishLaunchingWithOptions launchOptions: [NSObject: AnyObject]?) -> Bool {
        registerApplicationForNotifications(application)

        let slideViewController = APLSlideMenuViewController()
        slideViewController.bouncing = true
        slideViewController.gestureSupport = .Drag
        slideViewController.leftMenuViewController = AddFriendsViewController()
        slideViewController.contentViewController = ChatViewController()

        window = UIWindow(frame: UIScreen.mainScreen().bounds)
        window?.rootViewController = slideViewController
        window?.makeKeyAndVisible()

        ChatClient.sharedClient.connect()

        return true
    }

    func application(application: UIApplication, didRegisterForRemoteNotificationsWithDeviceToken deviceToken: NSData) {
    }

    func application(application: UIApplication, didReceiveRemoteNotification userInfo: [NSObject : AnyObject]) {
    }

    private func registerApplicationForNotifications(application: UIApplication) {
        let settings = UIUserNotificationSettings(forTypes: [.Alert, .Badge, .Sound], categories: nil)
        application.registerUserNotificationSettings(settings)
        application.registerForRemoteNotifications()
    }
}
