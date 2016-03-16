//
//  ThemedLabel.swift
//  chat
//
//  Created by Andreas Binnewies on 3/13/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

class ThemedLabel: UILabel {
    private let themeListener = ThemeListener()

    override func awakeFromNib() {
        super.awakeFromNib()

        updateTheme(ColorTheme.currentTheme)

        themeListener.themeChangeListener = { [weak self] theme in
            self?.updateTheme(theme)
        }
    }

    private func updateTheme(theme: ColorTheme) {
        textColor = theme.buttonColor
    }
}