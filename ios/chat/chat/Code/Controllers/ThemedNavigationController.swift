//
//  ThemedNavigationController.swift
//  chat
//
//  Created by Andreas Binnewies on 3/13/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

class ThemedNavigationController: UINavigationController {
    deinit {
        NSNotificationCenter.defaultCenter().removeObserver(self)
    }

    override func viewDidLoad() {
        super.viewDidLoad()

        NSNotificationCenter.defaultCenter().addObserver(self, selector: "updateTheme", name: ColorTheme.ThemeChangedNotification, object: nil)
        updateTheme()
    }

    @objc private func updateTheme() {
        navigationBar.translucent = false
        navigationBar.tintColor = UIColor.currentTheme.buttonColor
        navigationBar.barTintColor = UIColor.currentTheme.lightBackgroundColor
        navigationBar.setBackgroundImage(UIImage(), forBarMetrics: .Default)
        navigationBar.shadowImage = UIImage.imageWithColor(UIColor.currentTheme.borderColor)
        navigationBar.titleTextAttributes = [NSForegroundColorAttributeName: UIColor.currentTheme.buttonColor]
    }
}