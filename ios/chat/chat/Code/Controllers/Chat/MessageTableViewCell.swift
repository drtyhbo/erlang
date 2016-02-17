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

    @IBOutlet weak var userImageTopConstraint: NSLayoutConstraint!
    @IBOutlet weak var userImageHeightConstraint: NSLayoutConstraint!
    @IBOutlet var headerHeightConstraint: NSLayoutConstraint!

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
            userImageTopConstraint.constant = hasHeader ? MessageTableViewCell.userImageTopMax : MessageTableViewCell.userImageTopMin
            userImageHeightConstraint.constant = hasHeader ? MessageTableViewCell.userImageHeight : 0

            if !hasHeader {
                headerHeightConstraint.constant = 0
            }
            headerHeightConstraint.active = !hasHeader
        }
    }

    private static let userImageTopMax: CGFloat = 24
    private static let userImageTopMin: CGFloat = 8
    private static let userImageHeight: CGFloat = 48

    class func estimatedHeightForMessage(message: Message, hasHeader: Bool) -> CGFloat {
        return hasHeader ? (userImageTopMax + 19) : userImageTopMin
    }
}