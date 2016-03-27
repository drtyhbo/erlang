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
    static let fonts = [
        CustomFont(fontName: "AvenirNext", displayName: "Avenir Next"),
        CustomFont(fontName: "ChalkboardSE", displayName: "Chalkboard"),
        CustomFont(fontName: "Lato", displayName: "Lato"),
        CustomFont(fontName: "Menlo", displayName: "Menlo"),
        CustomFont(fontName: "Raleway", displayName: "Raleway"),
        CustomFont(fontName: "SanFranciscoRounded", displayName: "San Francisco")]

    static let FontChangedNotification = "ThemeChanged"

    static var currentFont: CustomFont {
        get {
            let fontName = NSUserDefaults.sharedUserDefaults().stringForKey(currentFontKey) ?? "Lato"
            for font in fonts {
                if font.fontName == fontName {
                    return font
                }
            }
            return fonts[0]
        }
        set {
            let userDefaults = NSUserDefaults.sharedUserDefaults()
            userDefaults.setObject(newValue.fontName, forKey: currentFontKey)
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

func ==(lhs: CustomFont, rhs: CustomFont) -> Bool {
    return lhs.fontName == rhs.fontName
}