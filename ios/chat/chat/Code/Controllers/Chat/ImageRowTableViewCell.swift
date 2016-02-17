//
//  ImageRowTableViewCell.swift
//  chat
//
//  Created by Andreas Binnewies on 2/15/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

class ImageRowTableViewCell: MessageTableViewCell {
    @IBOutlet weak var messageImageView: UIImageView!

    @IBOutlet weak var widthConstraint: NSLayoutConstraint!
    @IBOutlet weak var heightConstraint: NSLayoutConstraint!

    override var message: Message! {
        didSet {
            imageInfo = message.imageInfo
        }
    }

    private var imageInfo: Message.ImageInfo? {
        didSet {
            if let imageInfo = imageInfo {
                let screenSize = UIScreen.mainScreen().bounds.size
                var imageSize = CGSize(width: imageInfo.width, height: imageInfo.height)
                if imageInfo.width + 60 > Int(screenSize.width) {
                    let newWidth = screenSize.width - 60
                    imageSize = CGSize(width: newWidth, height: imageSize.height / imageSize.width * newWidth)
                }
                widthConstraint.constant = imageSize.width
                heightConstraint.constant = imageSize.height

                loadImageWithId(imageInfo.thumbnailId)
            } else {
                heightConstraint.constant = 0
                messageImageView.hidden = true
            }
        }
    }

    private func loadImageWithId(thumbnailId: Int) {
        messageImageView.image = nil

        FileHelper.getFileWithId(thumbnailId) {
            file in

            dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0)) {
                if let image = file?.image {
                    dispatch_async(dispatch_get_main_queue()) {
                        self.messageImageView.image = image
                        self.messageImageView.hidden = false
                    }
                }
            }
        }
    }
}