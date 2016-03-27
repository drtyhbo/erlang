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
    enum FooterType {
        case Small
        case Large
    }

    enum Alignment {
        case Left
        case Right
    }

    @IBOutlet weak var bubbleBackground: UIView!
    @IBOutlet weak var bubbleImageBackground: BubbleImageView!

    @IBOutlet weak var bubbleBottomConstraint: NSLayoutConstraint!

    var footerType: FooterType = .Small {
        didSet {
            bubbleBottomConstraint.constant = BubbleTableViewCell.footerHeightForType(footerType)
        }
    }

    var alignment: Alignment {
        return message.from == nil ? .Right : .Left
    }

    private(set) var message: Message!

    private var hasTail: Bool = true

    class func heightForMessage(message: Message, footerType: FooterType) -> CGFloat {
        return footerHeightForType(footerType)
    }

    private class func footerHeightForType(footerType: FooterType) -> CGFloat {
        return footerType == .Small ? 1 : 16
    }

    func updateWithMessage(message: Message, hasTail: Bool) {
        self.message = message

        let fromCurrentUser = message.from == nil
        bubbleImageBackground.hasTail = hasTail
        bubbleImageBackground.setColor(fromCurrentUser ? ColorTheme.currentTheme.buttonColor : Constants.otherChatBubbleColor, direction: alignment == .Left ? .Left : .Right)
    }
}