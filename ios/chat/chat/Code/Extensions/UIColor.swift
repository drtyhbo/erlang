//
//  UIColor.swift
//  chat
//
//  Created by Andreas Binnewies on 2/21/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

extension UIColor {
    static var currentTheme: ColorTheme {
        return ColorTheme.currentTheme
    }

    convenience init(_ hex: Int) {
        let red = CGFloat(hex >> 16 & 0xFF) / 255.0
        let green = CGFloat(hex >> 8 & 0xFF) / 255.0
        let blue = CGFloat(hex & 0xFF) / 255.0

        self.init(red: red, green: green, blue: blue, alpha: 1.0)
    }
}