//
//  LabelWithCustomFont.swift
//  chat
//
//  Created by Andreas Binnewies on 3/23/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

class LabelWithCustomFont: UILabel {
    private let themeListener = ThemeListener()

    override func awakeFromNib() {
        super.awakeFromNib()

        themeListener.listenForFontChangesWithCurrentFont(font) { [weak self] font in
            self?.updateFont(font)
        }
    }

    @objc private func updateFont(font: UIFont) {
        self.font = font
    }
}