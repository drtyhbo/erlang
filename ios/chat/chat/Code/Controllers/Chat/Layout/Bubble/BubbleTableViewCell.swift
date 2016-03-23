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
            alignment = fromCurrentUser ? .Right : .Left

            bubbleImageBackground.setColor(fromCurrentUser ? ColorTheme.currentTheme.buttonColor : Constants.otherChatBubbleColor, direction: alignment == .Left ? .Left : .Right)
        }
    }

    var headerType: HeaderType = .Small {
        didSet {
            bubbleTopConstraint.constant = headerType == .Small ? 1 : 16
        }
    }

    private(set) var alignment: Alignment = .Left {
        didSet {
            switch alignment {
            case .Left:
                bubbleFlexibleLeadingConstraint.active = false
                bubbleTrailingConstraint.active = false

                bubbleLeadingConstraint.constant = 10
                bubbleLeadingConstraint.active = true
                bubbleFlexibleTrailingConstraint.constant = 70
                bubbleFlexibleTrailingConstraint.active = true
            case .Right:
                bubbleLeadingConstraint.active = false
                bubbleFlexibleTrailingConstraint.active = false

                bubbleFlexibleLeadingConstraint.constant = 70
                bubbleFlexibleLeadingConstraint.active = true
                bubbleTrailingConstraint.constant = 10
                bubbleTrailingConstraint.active = true
            }
        }
    }

    class func estimatedHeightForMessage(message: Message) -> CGFloat {
        return 21
    }
}