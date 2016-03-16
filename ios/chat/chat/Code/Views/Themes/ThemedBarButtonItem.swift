//
//  ThemedBarButtonItem.swift
//  chat
//
//  Created by Andreas Binnewies on 3/13/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

class ThemedBarButtonItem: UIBarButtonItem {
    private let themeListener = ThemeListener()

    init(title: String?, style: UIBarButtonItemStyle, target: AnyObject?, action: Selector) {
        super.init()
        self.title = title
        self.style = style
        self.target = target
        self.action = action

        updateTheme(ColorTheme.currentTheme)
        themeListener.themeChangeListener = { [weak self] theme in
            self?.updateTheme(theme)
        }
    }

    required init?(coder aDecoder: NSCoder) {
        super.init(coder: aDecoder)

        updateTheme(ColorTheme.currentTheme)
        themeListener.themeChangeListener = { [weak self] theme in
            self?.updateTheme(theme)
        }
    }

    private func updateTheme(theme: ColorTheme) {
        setTitleTextAttributes([NSForegroundColorAttributeName: theme.buttonColor], forState: .Normal)
        tintColor = theme.buttonColor
    }
}