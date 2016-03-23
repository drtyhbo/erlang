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

        updateTheme(ColorTheme.currentTheme)
        themeListener.themeChangeListener = { [weak self] theme in
            self?.updateTheme(theme)
        }

        titleLabel?.font = UIFont.customFontOfSize(titleLabel?.font.pointSize ?? 16)
    }

    private func updateTheme(theme: ColorTheme) {
        let buttonColor = theme.buttonColor
        tintColor = buttonColor
        setTitleColor(buttonColor, forState: .Normal)
    }
}