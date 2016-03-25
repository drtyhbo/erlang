//
//  ChatLayout.swift
//  chat
//
//  Created by Andreas Binnewies on 3/25/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation

enum ChatLayout: String {
    case Normal = "normal"
    case Bubble = "bubble"

    static let LayoutChangedNotification = "LayoutChanged"

    var displayName: String {
        switch (self) {
            case .Normal:
                return "Inline"
            case .Bubble:
                return "Bubbles"
        }
    }

    private static let currentLayoutKey = "CurrentLayout"

    static func currentLayout() -> ChatLayout {
        guard let layoutName = NSUserDefaults.sharedUserDefaults().stringForKey(currentLayoutKey) else {
            return .Bubble
        }
        return ChatLayout(rawValue: layoutName) ?? .Bubble
    }

    static func setCurrentLayout(chatLayout: ChatLayout) {
        let userDefaults = NSUserDefaults.sharedUserDefaults()
        userDefaults.setObject(chatLayout.rawValue, forKey: currentLayoutKey)
        userDefaults.synchronize()

        NSNotificationCenter.defaultCenter().postNotificationName(ChatLayout.LayoutChangedNotification, object: nil, userInfo: nil)
    }
}