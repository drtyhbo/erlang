//
//  ColorTheme.swift
//  chat
//
//  Created by Andreas Binnewies on 3/13/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

class ColorTheme {
    let lightBackgroundColor: UIColor
    let darkBackgroundColor: UIColor
    let borderColor: UIColor
    let buttonColor: UIColor
    let darkButtonColor: UIColor

    init(lightBackgroundColor: UIColor, darkBackgroundColor: UIColor, borderColor: UIColor, buttonColor: UIColor, darkButtonColor: UIColor) {
        self.lightBackgroundColor = lightBackgroundColor
        self.darkBackgroundColor = darkBackgroundColor
        self.borderColor = borderColor
        self.buttonColor = buttonColor
        self.darkButtonColor = darkButtonColor
    }
}