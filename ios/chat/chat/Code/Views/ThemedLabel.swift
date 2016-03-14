//
//  ThemedLabel.swift
//  chat
//
//  Created by Andreas Binnewies on 3/13/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

class ThemedLabel: UILabel {
    override func awakeFromNib() {
        super.awakeFromNib()
        textColor = UIColor.currentTheme.buttonColor
    }
}