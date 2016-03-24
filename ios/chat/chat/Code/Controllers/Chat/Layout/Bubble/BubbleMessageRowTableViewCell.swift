//
//  BubbleMessageRowTableViewCell.swift
//  chat
//
//  Created by Andreas Binnewies on 1/13/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import ChatCommon
import Foundation
import SDWebImage
import UIKit

class BubbleMessageRowTableViewCell: BubbleTableViewCell {
    @IBOutlet weak var messageLabel: UILabel!

    @IBOutlet weak var messageLeadingConstraint: NSLayoutConstraint!
    @IBOutlet weak var messageTrailingConstraint: NSLayoutConstraint!

    override class func estimatedHeightForMessage(message: Message) -> CGFloat {
        let text = message.text ?? ""
        let boundingRect = (text as NSString).boundingRectWithSize(CGSize(width: UIScreen.mainScreen().bounds.size.width - 64, height: 9999), options: NSStringDrawingOptions.UsesLineFragmentOrigin, attributes: [NSFontAttributeName: UIFont.customFontOfSize(17)], context: nil)
        return super.estimatedHeightForMessage(message) + round(boundingRect.height)
    }

    override func updateWithMessage(message: Message, hasTail: Bool) {
        super.updateWithMessage(message, hasTail: hasTail)

        messageLabel.text = message.text ?? ""
        messageLabel.textColor = message.from == nil ? UIColor.whiteColor() : UIColor.blackColor()

        messageLeadingConstraint.constant = alignment == .Left ? 15 : 10
        messageTrailingConstraint.constant = alignment == .Right ? 15 : 10
    }
}