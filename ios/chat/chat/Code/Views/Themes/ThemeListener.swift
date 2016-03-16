//
//  ThemeListener.swift
//  chat
//
//  Created by Andreas Binnewies on 3/16/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation

class ThemeListener {
    // Most of the time you'll be
    var themeChangeListener: (ColorTheme->Void)? {
        didSet {
            if !notificationRegistered {
                notificationRegistered = true
                NSNotificationCenter.defaultCenter().addObserver(self, selector: "updateTheme", name: ColorTheme.ThemeChangedNotification, object: nil)
            }
        }
    }

    private var notificationRegistered = false

    deinit {
        NSNotificationCenter.defaultCenter().removeObserver(self)
    }

    @objc private func updateTheme() {
        themeChangeListener?(ColorTheme.currentTheme)
    }
}