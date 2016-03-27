//
//  ThemeListener.swift
//  chat
//
//  Created by Andreas Binnewies on 3/16/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

class ThemeListener {
    private var isBold = false
    private var pointSize: CGFloat = 16

    private var themeChangeListener: (ColorTheme->Void)?
    private var fontChangeListener: (UIFont->Void)?

    deinit {
        NSNotificationCenter.defaultCenter().removeObserver(self)
    }

    func listenForThemeChangesWithCallback(callback: ColorTheme->Void) {
        if themeChangeListener == nil {
            NSNotificationCenter.defaultCenter().addObserver(self, selector: "updateTheme", name: ColorTheme.ThemeChangedNotification, object: nil)
        }
        themeChangeListener = callback

        updateTheme()
    }

    func listenForFontChangesWithCurrentFont(currentFont: UIFont?, callback: UIFont->Void) {
        isBold = currentFont?.fontName.rangeOfString("Heavy") != nil
        pointSize = currentFont?.pointSize ?? 16

        if fontChangeListener == nil {
            NSNotificationCenter.defaultCenter().addObserver(self, selector: "updateFont", name: CustomFont.FontChangedNotification, object: nil)
        }
        fontChangeListener = callback

        updateFont()
    }

    @objc private func updateTheme() {
        themeChangeListener?(ColorTheme.currentTheme)
    }

    @objc private func updateFont() {
        fontChangeListener?(isBold ? UIFont.boldCustomFontOfSize(pointSize) : UIFont.customFontOfSize(pointSize))
    }
}