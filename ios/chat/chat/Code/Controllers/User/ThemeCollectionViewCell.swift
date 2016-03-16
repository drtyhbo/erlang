//
//  ThemeCollectionViewCell.swift
//  chat
//
//  Created by Andreas Binnewies on 3/15/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

class ThemeCollectionViewCell: UICollectionViewCell {
    @IBOutlet weak var themeColorBlock: UIView!

    var themeColor: UIColor = UIColor.clearColor() {
        didSet {
            themeColorBlock.backgroundColor = themeColor
        }
    }

    override func awakeFromNib() {
        super.awakeFromNib()
        themeColorBlock.layer.cornerRadius = 10
    }
}