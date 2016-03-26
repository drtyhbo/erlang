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

    private static let minPadding: CGFloat = 10
    private static let maxPadding: CGFloat = 15

    override class func heightForMessage(message: Message, footerType: FooterType) -> CGFloat {
        let text = message.text ?? ""
        let boundingSize = (text as NSString).boundingRectWithSize(CGSize(width: UIScreen.mainScreen().bounds.size.width - (Constants.BubbleLayout.minPadding + Constants.BubbleLayout.maxPadding) - (minPadding + maxPadding), height: 9999), options: [NSStringDrawingOptions.UsesLineFragmentOrigin, NSStringDrawingOptions.UsesFontLeading], attributes: [NSFontAttributeName: UIFont.customFontOfSize(17)], context: nil)
        return super.heightForMessage(message, footerType: footerType) + ceil(boundingSize.height) + 20
    }

    override func updateWithMessage(message: Message, hasTail: Bool) {
        super.updateWithMessage(message, hasTail: hasTail)

        messageLabel.text = message.text ?? ""
        messageLabel.textColor = message.from == nil ? UIColor.whiteColor() : UIColor.blackColor()

        messageLeadingConstraint.constant = alignment == .Left ? BubbleMessageRowTableViewCell.maxPadding : BubbleMessageRowTableViewCell.minPadding
        messageTrailingConstraint.constant = alignment == .Left ? BubbleMessageRowTableViewCell.minPadding : BubbleMessageRowTableViewCell.maxPadding
    }
}