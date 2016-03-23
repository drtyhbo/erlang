//
//  FontPickerTableViewCell.swift
//  chat
//
//  Created by Andreas Binnewies on 3/15/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

class FontPickerTableViewCell: UITableViewCell {
    @IBOutlet weak var currentFontLabel: LabelWithCustomFont!

    static let cellHeight: CGFloat = 60

    override func awakeFromNib() {
        super.awakeFromNib()
        selectionStyle = .None
        currentFontLabel.text = CustomFont.currentFontName
    }
}