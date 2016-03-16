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
    deinit {
        NSNotificationCenter.defaultCenter().removeObserver(self)
    }

    override func awakeFromNib() {
        super.awakeFromNib()

        NSNotificationCenter.defaultCenter().addObserver(self, selector: "updateTheme", name: ColorTheme.ThemeChangedNotification, object: nil)
        updateTheme()
    }

    @objc private func updateTheme() {
        image = image?.imageWithRenderingMode(.AlwaysTemplate)
        tintColor = UIColor.currentTheme.buttonColor
    }
}