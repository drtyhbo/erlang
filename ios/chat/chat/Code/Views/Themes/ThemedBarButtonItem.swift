//
//  ThemedBarButtonItem.swift
//  chat
//
//  Created by Andreas Binnewies on 3/13/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

class ThemedBarButtonItem: UIBarButtonItem {
    init(title: String?, style: UIBarButtonItemStyle, target: AnyObject?, action: Selector) {
        super.init()
        self.title = title
        self.style = style
        self.target = target
        self.action = action

        applyTheme()
    }

    required init?(coder aDecoder: NSCoder) {
        super.init(coder: aDecoder)

        applyTheme()
    }

    private func applyTheme() {
        setTitleTextAttributes([NSForegroundColorAttributeName: UIColor.currentTheme.buttonColor], forState: .Normal)
        tintColor = UIColor.currentTheme.buttonColor
    }
}