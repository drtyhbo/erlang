//
//  LayoutPickerTableViewCell.swift
//  chat
//
//  Created by Andreas Binnewies on 3/15/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

class LayoutPickerTableViewCell: UITableViewCell {
    @IBOutlet weak var currentLayoutLabel: LabelWithCustomFont!

    static let cellHeight: CGFloat = 60

    override func awakeFromNib() {
        super.awakeFromNib()
        selectionStyle = .None
        currentLayoutLabel.text = ChatLayout.currentLayout().displayName
    }

    override func prepareForReuse() {
        super.prepareForReuse()
        currentLayoutLabel.text = ChatLayout.currentLayout().displayName
    }
}