//
//  ChatRowContinuationTableViewCell.swift
//  chat
//
//  Created by Andreas Binnewies on 1/13/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import SDWebImage
import UIKit

class ChatRowContinuationTableViewCell: MessageTableViewCell {
    @IBOutlet weak var messageLabel: UILabel!

    @IBOutlet weak var messageImageView: UIImageView!
    @IBOutlet weak var messageImageViewWidthConstraint: NSLayoutConstraint!
    @IBOutlet weak var messageImageViewHeightConstraint: NSLayoutConstraint!

    override var message: Message! {
        didSet {
/*            messageLabel.text = message.message

            if let imageInfo = message.imageInfo {
                let screenSize = UIScreen.mainScreen().bounds.size
                var imageSize = CGSize(width: imageInfo.1, height: imageInfo.2)
                if imageInfo.1 + 60 > Int(imageSize.width) {
                    let newWidth = screenSize.width - 60
                    imageSize = CGSize(width: newWidth, height: imageSize.height / imageSize.width * newWidth)
                }
                messageImageViewWidthConstraint.constant = imageSize.width
                messageImageViewHeightConstraint.constant = imageSize.height
                messageImageView.sd_setImageWithURL(imageInfo.0) {
                    image, error, cacheType, imageURL in
                    if image != nil {
                        self.messageImageView.hidden = false
                    }
                }
            } else {
                messageImageViewHeightConstraint.constant = 0
                messageImageView.hidden = true
            }*/
        }
    }
}