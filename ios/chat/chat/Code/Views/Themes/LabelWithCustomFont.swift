//
//  LabelWithCustomFont.swift
//  chat
//
//  Created by Andreas Binnewies on 3/23/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

class LabelWithCustomFont: UILabel {
    deinit {
        NSNotificationCenter.defaultCenter().removeObserver(self)
    }

    override func awakeFromNib() {
        super.awakeFromNib()

        NSNotificationCenter.defaultCenter().addObserver(self, selector: "updateFont", name: CustomFont.FontChangedNotification, object: nil)
        updateFont()
    }

    @objc private func updateFont() {
        font = UIFont.customFontOfSize(font?.pointSize ?? 16)
    }
}