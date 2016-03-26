//
//  ThemedTableView.swift
//  chat
//
//  Created by Andreas Binnewies on 3/15/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

class ThemedTableView: UITableView {
    private let themeListener = ThemeListener()

    override func awakeFromNib() {
        super.awakeFromNib()

        themeListener.listenForThemeChangesWithCallback { [weak self] theme in
            self?.updateTheme(theme)
        }
    }

    private func updateTheme(theme: ColorTheme) {
        backgroundColor = UIColor.currentTheme.lightBackgroundColor
        separatorColor = UIColor.currentTheme.borderColor
    }
}