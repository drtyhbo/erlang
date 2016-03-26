//
//  ChatRowTableViewCell.swift
//  chat
//
//  Created by Andreas Binnewies on 1/13/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import ChatCommon
import Foundation
import SDWebImage
import UIKit

class ChatRowTableViewCell: MessageTableViewCell {
    @IBOutlet weak var messageLabel: UILabel!

    override var message: Message! {
        didSet {
            messageLabel.text = message.text ?? ""
        }
    }

    private static let totalHorizontalPadding: CGFloat = 68

    override class func estimatedHeightForMessage(message: Message, headerType: HeaderType) -> CGFloat {
        let text = message.text ?? ""
        let boundingRect = (text as NSString).boundingRectWithSize(CGSize(width: UIScreen.mainScreen().bounds.size.width - totalHorizontalPadding, height: 9999), options: NSStringDrawingOptions.UsesLineFragmentOrigin, attributes: [NSFontAttributeName: UIFont.customFontOfSize(16)], context: nil)
        return super.estimatedHeightForMessage(message, headerType: headerType) + ceil(boundingRect.height)
    }
}