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
    override func viewDidLoad() {
        super.viewDidLoad()
        navigationBar.translucent = false
        navigationBar.barTintColor = UIColor.currentTheme.lightBackgroundColor
        navigationBar.setBackgroundImage(UIImage(), forBarMetrics: .Default)
        navigationBar.shadowImage = UIImage.imageWithColor(UIColor.currentTheme.borderColor)
    }
}