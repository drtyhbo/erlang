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
    deinit {
        NSNotificationCenter.defaultCenter().removeObserver(self)
    }

    override func awakeFromNib() {
        super.awakeFromNib()
        setImage(imageForState(.Normal)?.imageWithRenderingMode(.AlwaysTemplate), forState: .Normal)

        NSNotificationCenter.defaultCenter().addObserver(self, selector: "updateTheme", name: ColorTheme.ThemeChangedNotification, object: nil)
        updateTheme()
    }

    @objc private func updateTheme() {
        let buttonColor = UIColor.currentTheme.buttonColor
        tintColor = buttonColor
        setTitleColor(buttonColor, forState: .Normal)
    }
}