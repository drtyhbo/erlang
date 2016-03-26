//
//  ThemedView.swift
//  chat
//
//  Created by Andreas Binnewies on 3/15/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

class ThemedView: UIView {
    enum ColorType {
        case Light
        case Dark
    }

    var colorType: ColorType = .Light {
        didSet {
            updateTheme(ColorTheme.currentTheme)
        }
    }

    private let themeListener = ThemeListener()

    override func awakeFromNib() {
        super.awakeFromNib()

        themeListener.listenForThemeChangesWithCallback { [weak self] theme in
            self?.updateTheme(theme)
        }
    }

    private func updateTheme(theme: ColorTheme) {
        backgroundColor = colorType == .Light ? theme.lightBackgroundColor : theme.buttonColor
    }
}