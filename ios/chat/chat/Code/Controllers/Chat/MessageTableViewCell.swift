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
    enum HeaderType {
        case Full
        case FullNoPadding
        case PaddingOnly
        case NoPadding
    }

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

            if let from = message.from {
                userImage.sd_setImageWithURL(from.profilePicUrl, placeholderImage: UIImage(named: "ProfilePic"))
            } else {
                userImage.image = User.profilePic ?? UIImage(named: "ProfilePic")
            }
        }
    }

    var headerType: HeaderType = .Full {
        didSet {
            switch (headerType) {
                case .Full:
                    userImageTopConstraint.constant = MessageTableViewCell.paddingTopMax
                    userImageHeightConstraint.constant = MessageTableViewCell.userImageHeight
                    headerHeightConstraint.active = false
                case .FullNoPadding:
                    userImageTopConstraint.constant = 0
                    userImageHeightConstraint.constant = MessageTableViewCell.userImageHeight
                    headerHeightConstraint.active = false
                default:
                    userImageTopConstraint.constant = headerType == .PaddingOnly ? MessageTableViewCell.paddingTopMin : 0
                    userImageHeightConstraint.constant = 0
                    headerHeightConstraint.constant = 0
                    headerHeightConstraint.active = true
            }
        }
    }

    private static let paddingTopMax: CGFloat = 24
    private static let paddingTopMin: CGFloat = 2
    private static let userImageHeight: CGFloat = 48

    class func estimatedHeightForMessage(message: Message, headerType: HeaderType) -> CGFloat {
        if headerType == .Full {
            return paddingTopMax + 17
        } else if headerType == .FullNoPadding {
            return 17
        } else {
            return paddingTopMin
        }
    }
}