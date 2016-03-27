//
//  ThemedImageView.swift
//  chat
//
//  Created by Andreas Binnewies on 3/13/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

class ThemedImageView: UIImageView {
    private let themeListener = ThemeListener()

    override func awakeFromNib() {
        super.awakeFromNib()

        themeListener.listenForThemeChangesWithCallback { [weak self] theme in
            self?.updateTheme(theme)
        }
    }

    private func updateTheme(theme: ColorTheme) {
        image = image?.imageWithRenderingMode(.AlwaysTemplate)
        tintColor = theme.buttonColor
    }
}