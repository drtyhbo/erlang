//
//  CustomFont.swift
//  chat
//
//  Created by Andreas Binnewies on 3/23/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

class CustomFont {
    static let fontNames = [
        CustomFont(fontName: "AvenirNext", displayName: "Avenir Next"),
        CustomFont(fontName: "AvenirNextCondensed", displayName: "Avenir Next Condensed"),
        CustomFont(fontName: "ChalkboardSE", displayName: "Chalkboard"),
        CustomFont(fontName: "Lato", displayName: "Lato"),
        CustomFont(fontName: "Menlo", displayName: "Menlo"),
        CustomFont(fontName: "Raleway", displayName: "Raleway"),
        CustomFont(fontName: "SanFranciscoRounded", displayName: "San Francisco")]

    static let FontChangedNotification = "ThemeChanged"

    static var currentFontName: String {
        get {
            return NSUserDefaults.sharedUserDefaults().stringForKey(currentFontKey) ?? "Lato"
        }
        set {
            let userDefaults = NSUserDefaults.sharedUserDefaults()
            userDefaults.setObject(newValue, forKey: currentFontKey)
            userDefaults.synchronize()

            NSNotificationCenter.defaultCenter().postNotificationName(FontChangedNotification, object: nil, userInfo: nil)
        }
    }

    private static let currentFontKey = "CurrentFont"

    let fontName: String
    let displayName: String

    init(fontName: String, displayName: String) {
        self.fontName = fontName
        self.displayName = displayName
    }
}