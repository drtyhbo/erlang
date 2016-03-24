//
//  BubbleTableViewCell.swift
//  chat
//
//  Created by Andreas Binnewies on 3/21/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import ChatCommon
import Foundation
import UIKit

class BubbleTableViewCell: UITableViewCell {
    enum HeaderType {
        case Small
        case Large
    }

    enum Alignment {
        case Left
        case Right
    }

    @IBOutlet weak var bubbleBackground: UIView!
    @IBOutlet weak var bubbleImageBackground: BubbleImageView!

    @IBOutlet weak var bubbleTopConstraint: NSLayoutConstraint!
    @IBOutlet var bubbleLeadingConstraint: NSLayoutConstraint!
    @IBOutlet var bubbleTrailingConstraint: NSLayoutConstraint!
    @IBOutlet var bubbleFlexibleLeadingConstraint: NSLayoutConstraint!
    @IBOutlet var bubbleFlexibleTrailingConstraint: NSLayoutConstraint!

    var message: Message! {
        didSet {
            let fromCurrentUser = message.from == nil

            updateBubbleConstraints()
            bubbleImageBackground.setColor(fromCurrentUser ? ColorTheme.currentTheme.buttonColor : Constants.otherChatBubbleColor, direction: alignment == .Left ? .Left : .Right)
        }
    }

    var headerType: HeaderType = .Small {
        didSet {
            bubbleTopConstraint.constant = headerType == .Small ? 1 : 16
        }
    }

    var alignment: Alignment {
        return message.from == nil ? .Right : .Left
    }

    class func estimatedHeightForMessage(message: Message) -> CGFloat {
        return 21
    }

    private func updateBubbleConstraints() {
        let leftAligned = alignment == .Left
        bubbleFlexibleLeadingConstraint.active = !leftAligned
        bubbleTrailingConstraint.active = !leftAligned
        bubbleLeadingConstraint.active = leftAligned
        bubbleFlexibleTrailingConstraint.active = leftAligned
    }
}