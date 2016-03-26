//
//  MessageTableViewCell.swift
//  chat
//
//  Created by Andreas Binnewies on 1/18/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import ChatCommon
import Foundation
import UIKit

class MessageTableViewCell: UITableViewCell {
    enum HeaderType {
        case Full
        case FullNoPadding
        case PaddingOnly
    }

    @IBOutlet weak var userImage: ChatProfilePic!
    @IBOutlet weak var userName: UILabel!
    @IBOutlet weak var dateLabel: UILabel!

    @IBOutlet weak var userImageTopConstraint: NSLayoutConstraint!

    var message: Message! {
        didSet {
            if let userName = userName {
                userName.text = message.from?.firstName ?? "Me"
            }

            if let dateLabel = dateLabel {
                let timeFormatter = NSDateFormatter()
                timeFormatter.dateFormat = "H:mm"
                dateLabel.text = timeFormatter.stringFromDate(message.date)
            }

            if let userImage = userImage {
                userImage.friend = message.from
            }
        }
    }

    var headerType: HeaderType = .Full {
        didSet {
            guard let userImageTopConstraint = userImageTopConstraint else {
                return
            }

            switch (headerType) {
                case .Full:
                    userImageTopConstraint.constant = MessageTableViewCell.paddingTopMax
                case .FullNoPadding:
                    userImageTopConstraint.constant = 0
                default:
                    break
            }
        }
    }

    private static let paddingTopMax: CGFloat = 18
    private static let paddingTopMin: CGFloat = 2
    private static let userImageHeight: CGFloat = 48
    private static let nameHeight: CGFloat = 17
    private static let paddingBetweenNameAndContent: CGFloat = 6

    class func estimatedHeightForMessage(message: Message, headerType: HeaderType) -> CGFloat {
        if headerType == .Full {
            return paddingTopMax + nameHeight + paddingBetweenNameAndContent
        } else if headerType == .FullNoPadding {
            return nameHeight + paddingBetweenNameAndContent
        } else {
            return paddingTopMin
        }
    }
}