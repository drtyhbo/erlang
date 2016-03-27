//
//  ThemedButton.swift
//  chat
//
//  Created by Andreas Binnewies on 3/13/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

class ThemedButton: UIButton {
    private let themeListener = ThemeListener()

    override func awakeFromNib() {
        super.awakeFromNib()

        setImage(imageForState(.Normal)?.imageWithRenderingMode(.AlwaysTemplate), forState: .Normal)

        themeListener.listenForThemeChangesWithCallback { [weak self] theme in
            self?.updateTheme(theme)
        }
        themeListener.listenForFontChangesWithCurrentFont(titleLabel?.font) { [weak self] font in
            self?.updateFont(font)
        }
    }

    private func updateTheme(theme: ColorTheme) {
        let buttonColor = theme.buttonColor
        tintColor = buttonColor
        setTitleColor(buttonColor, forState: .Normal)
    }

    private func updateFont(font: UIFont) {
        titleLabel?.font = font
    }
}