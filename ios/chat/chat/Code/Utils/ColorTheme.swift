//
//  ColorTheme.swift
//  chat
//
//  Created by Andreas Binnewies on 3/13/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

class ColorTheme {
    enum ThemeType: Int {
        case Pink = 0
        case Blue = 1
        case Green = 2
        case Grey = 3
    }

    static let ThemeChangedNotification = "ThemeChanged"

    static var currentTheme: ColorTheme {
        return Constants.themes[currentThemeType.rawValue]
    }

    static var currentThemeType: ThemeType {
        get {
            return ThemeType(rawValue: NSUserDefaults.standardUserDefaults().integerForKey(currentThemeIndexKey))!
        }
        set {
            NSUserDefaults.standardUserDefaults().setInteger(newValue.rawValue, forKey: currentThemeIndexKey)
            NSUserDefaults.standardUserDefaults().synchronize()

            NSNotificationCenter.defaultCenter().postNotificationName(ColorTheme.ThemeChangedNotification, object: nil, userInfo: nil)
        }
    }

    private static let currentThemeIndexKey = "CurrentThemeIndex"

    let lightBackgroundColor: UIColor
    let darkBackgroundColor: UIColor
    let borderColor: UIColor
    let buttonColor: UIColor

    init(lightBackgroundColor: UIColor, darkBackgroundColor: UIColor, borderColor: UIColor, buttonColor: UIColor) {
        self.lightBackgroundColor = lightBackgroundColor
        self.darkBackgroundColor = darkBackgroundColor
        self.borderColor = borderColor
        self.buttonColor = buttonColor
    }
}