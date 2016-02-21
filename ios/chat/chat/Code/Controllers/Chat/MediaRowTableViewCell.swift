//
//  MediaRowTableViewCell.swift
//  chat
//
//  Created by Andreas Binnewies on 2/15/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import AVFoundation
import Foundation
import UIKit

class MediaRowTableViewCell: MessageTableViewCell {
    @IBOutlet weak var messageImageView: UIImageView!
    @IBOutlet weak var playIcon: UIImageView!
    @IBOutlet weak var moviePlayer: MoviePlayer!
    @IBOutlet weak var loadingIndicator: UIActivityIndicatorView!

    @IBOutlet weak var percentUploadedView: UIView!
    @IBOutlet weak var percentUploadedWidthConstraint: NSLayoutConstraint!

    @IBOutlet weak var widthConstraint: NSLayoutConstraint!
    @IBOutlet weak var heightConstraint: NSLayoutConstraint!

    override var message: Message! {
        didSet {
            NSNotificationCenter.defaultCenter().removeObserver(self)

            let isPending = PendingMessage.isMessagePending(message)
            if isPending {
                NSNotificationCenter.defaultCenter().addObserver(self, selector: "didUpdateProgressNotification:", name: MessageSender.SendingProgressNotification, object: nil)
                NSNotificationCenter.defaultCenter().addObserver(self, selector: "didFinishSendingNotification:", name: MessageSender.SendingCompleteNotification, object: nil)
            }

            updateThumbnail()

            messageImageView.alpha = isPending ? 0.5 : 1
            percentUploadedView.hidden = !isPending
            percentUploadedWidthConstraint.constant = 0
        }
    }

    private static let paddingBetweenHeaderAndContent: CGFloat = 8

    private let thumbnailPadding: CGFloat = 80

    override class func estimatedHeightForMessage(message: Message, headerType: HeaderType) -> CGFloat {
        guard let thumbnailInfo = message.thumbnailInfo else {
            return super.estimatedHeightForMessage(message, headerType: headerType)
        }

        return super.estimatedHeightForMessage(message, headerType: headerType) + dimensionsForThumbnailWithInfo(thumbnailInfo).height + MediaRowTableViewCell.paddingBetweenHeaderAndContent
    }

    private static func dimensionsForThumbnailWithInfo(thumbnailInfo: Message.ThumbnailInfo) -> (width: CGFloat, height: CGFloat) {
        let screenSize = UIScreen.mainScreen().bounds.size
        var imageSize = CGSize(width: thumbnailInfo.width, height: thumbnailInfo.height)
        if thumbnailInfo.width + 60 > Int(screenSize.width) {
            let newWidth = screenSize.width - 60
            imageSize = CGSize(width: newWidth, height: imageSize.height / imageSize.width * newWidth)
        }

        return (width: imageSize.width, height: round(imageSize.height))
    }

    override func awakeFromNib() {
        super.awakeFromNib()

        moviePlayer.delegate = self

        messageImageView.addGestureRecognizer(UITapGestureRecognizer(target: self, action: "didTapImage"))
        moviePlayer.addGestureRecognizer(UITapGestureRecognizer(target: self, action: "didTapMovie"))
    }

    override func prepareForReuse() {
        super.prepareForReuse()

        moviePlayer.pause()
        hideMoviePlayer()
    }

    private func updateThumbnail() {
        guard let thumbnailInfo = message.thumbnailInfo else {
            heightConstraint.constant = 0
            messageImageView.hidden = true
            return
        }

        playIcon.hidden = message.type != .Movie

        let screenSize = UIScreen.mainScreen().bounds.size
        var imageSize = CGSize(width: thumbnailInfo.width, height: thumbnailInfo.height)
        if CGFloat(thumbnailInfo.width) + thumbnailPadding > screenSize.width {
            let newWidth = screenSize.width - thumbnailPadding
            imageSize = CGSize(width: newWidth, height: imageSize.height / imageSize.width * newWidth)
        }

        widthConstraint.constant = imageSize.width
        heightConstraint.constant = round(imageSize.height)

        loadImageWithId(thumbnailInfo.id)
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

    private func hideMoviePlayer() {
        loadingIndicator.hidden = true
        playIcon.hidden = false
        moviePlayer.hidden = true
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

    @objc private func didTapImage() {
        if let movieInfo = message.movieInfo {
            playIcon.hidden = true

            loadingIndicator.hidden = false
            loadingIndicator.startAnimating()

            FileHelper.getFileWithId(movieInfo.id) {
                file in

                if let file = file {
                    let movieUrl = FileHelper.saveFileToTemporaryLocation(file)
                    self.moviePlayer.asset = AVURLAsset(URL: movieUrl)
                }
            }
        }
    }

    @objc private func didTapMovie() {
        moviePlayer.pause()
        hideMoviePlayer()
    }
}

extension MediaRowTableViewCell: MoviePlayerDelegate {
    func moviePlayerDidBeginPlaying(moviePlayer: MoviePlayer) {
        moviePlayer.hidden = false
        loadingIndicator.hidden = true
    }

    func moviePlayerDidReachEnd(moviePlayer: MoviePlayer) {
        hideMoviePlayer()
    }
}