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

    @IBOutlet weak var percentUploadedView: UIView!
    @IBOutlet weak var percentUploadedWidthConstraint: NSLayoutConstraint!

    @IBOutlet weak var widthConstraint: NSLayoutConstraint!
    @IBOutlet weak var heightConstraint: NSLayoutConstraint!

    override var message: Message! {
        didSet {
            imageInfo = message.imageInfo

            NSNotificationCenter.defaultCenter().removeObserver(self)

            let isPending = PendingMessage.isMessagePending(message)
            if isPending {
                NSNotificationCenter.defaultCenter().addObserver(self, selector: "didUpdateProgressNotification:", name: MessageSender.SendingProgressNotification, object: nil)
                NSNotificationCenter.defaultCenter().addObserver(self, selector: "didFinishSendingNotification:", name: MessageSender.SendingCompleteNotification, object: nil)
            }

            messageImageView.alpha = isPending ? 0.5 : 1
            percentUploadedView.hidden = !isPending
            percentUploadedWidthConstraint.constant = 0
        }
    }

    private static let paddingBetweenHeaderAndContent: CGFloat = 8

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
                heightConstraint.constant = round(imageSize.height)

                loadImageWithId(imageInfo.thumbnailId)
            } else {
                heightConstraint.constant = 0
                messageImageView.hidden = true
            }
        }
    }

    override class func estimatedHeightForMessage(message: Message, hasHeader: Bool) -> CGFloat {
        guard let imageInfo = message.imageInfo else {
            return super.estimatedHeightForMessage(message, hasHeader: hasHeader)
        }

        return super.estimatedHeightForMessage(message, hasHeader: hasHeader) + dimensionsForImageWithInfo(imageInfo).height + ImageRowTableViewCell.paddingBetweenHeaderAndContent
    }

    private static func dimensionsForImageWithInfo(imageInfo: Message.ImageInfo) -> (width: CGFloat, height: CGFloat) {
        let screenSize = UIScreen.mainScreen().bounds.size
        var imageSize = CGSize(width: imageInfo.width, height: imageInfo.height)
        if imageInfo.width + 60 > Int(screenSize.width) {
            let newWidth = screenSize.width - 60
            imageSize = CGSize(width: newWidth, height: imageSize.height / imageSize.width * newWidth)
        }

        return (width: imageSize.width, height: round(imageSize.height))
    }

    private func loadImageWithId(thumbnailId: Int) {
        messageImageView.image = nil

        FileHelper.getFileWithId(thumbnailId) {
            file in

            if let image = file?.image {
                self.messageImageView.image = image
                self.messageImageView.hidden = false
            }
        }
    }

    @objc private func didUpdateProgressNotification(notification: NSNotification) {
        guard let senderMessage = notification.object as? Message, percentComplete = notification.userInfo?["percentComplete"] as? Float where senderMessage.localId == message.localId else {
            return
        }

        percentUploadedWidthConstraint.constant = CGFloat(percentComplete) * messageImageView.bounds.size.width
    }

    @objc private func didFinishSendingNotification(notification: NSNotification) {
        guard let senderMessage = notification.object as? Message where senderMessage.localId == message.localId else {
            return
        }

        messageImageView.alpha = 1
        percentUploadedView.hidden = true
    }
}