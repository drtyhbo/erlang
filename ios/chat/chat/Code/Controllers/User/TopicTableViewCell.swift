//
//  TopicTableViewCell.swift
//  chat
//
//  Created by Andreas Binnewies on 2/1/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

class TopicTableViewCell: UITableViewCell {
    @IBOutlet weak var nameLabel: UILabel!
    @IBOutlet weak var nameLabelLeadingConstraint: NSLayoutConstraint!

    @IBOutlet weak var badge: UIView!
    @IBOutlet weak var unreadMessageCountLabel: UILabel!

    static let cellHeight: CGFloat = 48

    var name: String = "" {
        didSet {
            configureCell()
        }
    }

    override func awakeFromNib() {
        super.awakeFromNib()
        badge.layer.cornerRadius = badge.bounds.size.height / 2
    }

    override func setSelected(selected: Bool, animated: Bool) {
        super.setSelected(selected, animated: animated)
        nameLabel.textColor = selected ? UIColor(0x1F2124) : UIColor.whiteColor()
        nameLabel.font = selected ? UIFont.boldSystemFontOfSize(nameLabel.font.pointSize) : UIFont.systemFontOfSize(nameLabel.font.pointSize)
    }

    private func configureCell() {
        nameLabel.text = name
    }

    private func updateBadgeWithCount(unreadMessageCount: Int) {
        badge.hidden = unreadMessageCount == 0
        unreadMessageCountLabel.text = String(unreadMessageCount)
    }
}
