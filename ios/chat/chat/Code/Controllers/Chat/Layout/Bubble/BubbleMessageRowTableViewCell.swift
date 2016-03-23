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

    override var message: Message! {
        didSet {
            messageLabel.text = message.text ?? ""
            messageLabel.textColor = message.from == nil ? UIColor.whiteColor() : UIColor.blackColor()

            messageLeadingConstraint.constant = alignment == .Left ? 15 : 10
            messageTrailingConstraint.constant = alignment == .Right ? 15 : 5
        }
    }

    override class func estimatedHeightForMessage(message: Message) -> CGFloat {
        let text = message.text ?? ""
        let boundingRect = (text as NSString).boundingRectWithSize(CGSize(width: UIScreen.mainScreen().bounds.size.width - 64, height: 9999), options: NSStringDrawingOptions.UsesLineFragmentOrigin, attributes: [NSFontAttributeName: UIFont.customFontOfSize(16)], context: nil)
        return super.estimatedHeightForMessage(message) + round(boundingRect.height)
    }
}