//
//  MessageTableViewCell.swift
//  chat
//
//  Created by Andreas Binnewies on 1/18/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

class MessageTableViewCell: UITableViewCell {
    @IBOutlet weak var userImage: UIImageView!
    @IBOutlet weak var userName: UILabel!
    @IBOutlet weak var dateLabel: UILabel!

    @IBOutlet weak var userImageHeightConstraint: NSLayoutConstraint!
    @IBOutlet weak var headerHeightConstraint: NSLayoutConstraint!

    var message: Message! {
        didSet {
            userName.text = message.from?.name ?? "Me"

            let timeFormatter = NSDateFormatter()
            timeFormatter.dateFormat = "H:mm"
            dateLabel.text = timeFormatter.stringFromDate(message.date)
        }
    }

    var hasHeader = true {
        didSet {
            userImageHeightConstraint.constant = hasHeader ? userImageHeight : 0
            headerHeightConstraint.constant = hasHeader ? headerHeight : 0
        }
    }

    private let userImageHeight: CGFloat = 48
    private let headerHeight: CGFloat = 17
}