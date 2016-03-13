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
    override func awakeFromNib() {
        super.awakeFromNib()
        image = image?.imageWithRenderingMode(.AlwaysTemplate)
        tintColor = UIColor.currentTheme.buttonColor
    }
}