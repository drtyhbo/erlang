//
//  CheckMark.swift
//  chat
//
//  Created by Andreas Binnewies on 3/6/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

class CheckMark: UIImageView {
    var isChecked: Bool = false {
        didSet {
            alpha = isChecked ? 1 : 0.5
            image = isChecked ? UIImage(named: "Check")!.imageWithRenderingMode(.AlwaysTemplate) : nil
        }
    }

    override func awakeFromNib() {
        super.awakeFromNib()
        alpha = 0.5

        layer.borderColor = tintColor.CGColor
        layer.borderWidth = 1
    }

    override func tintColorDidChange() {
        super.tintColorDidChange()
        layer.borderColor = tintColor.CGColor
    }

    override func layoutSubviews() {
        super.layoutSubviews()
        layer.cornerRadius = bounds.size.width / 2
    }
}