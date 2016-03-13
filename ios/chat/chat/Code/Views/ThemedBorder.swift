//
//  ThemedBorder.swift
//  chat
//
//  Created by Andreas Binnewies on 3/13/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

class ThemedBorder: UIView {
    override func awakeFromNib() {
        super.awakeFromNib()
        backgroundColor = UIColor.currentTheme.borderColor
    }
}