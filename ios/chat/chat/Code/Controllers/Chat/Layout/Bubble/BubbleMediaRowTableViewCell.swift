//
//  BubbleMediaRowTableViewCell.swift
//  chat
//
//  Created by Andreas Binnewies on 2/15/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import AVFoundation
import ChatCommon
import Foundation
import UIKit

class BubbleMediaRowTableViewCell: BubbleTableViewCell {
    @IBOutlet weak var mediaContainer: UIView!
    @IBOutlet weak var messageImageView: UIImageView!
    @IBOutlet weak var playIcon: UIImageView!
    @IBOutlet weak var moviePlayer: MoviePlayer!
    @IBOutlet weak var loadingIndicator: UIActivityIndicatorView!

    @IBOutlet weak var progressView: ProgressView!

    @IBOutlet weak var topConstraint: NSLayoutConstraint!
    @IBOutlet weak var widthConstraint: NSLayoutConstraint!
    @IBOutlet weak var heightConstraint: NSLayoutConstraint!

    private static let thumbnailPadding: CGFloat = 80

    private var pendingMessageListener: PendingMessageListener?

    override class func heightForMessage(message: Message, footerType: FooterType) -> CGFloat {
        guard let thumbnailInfo = message.thumbnailInfo else {
            return 0
        }

        return super.heightForMessage(message, footerType: footerType) + dimensionsForThumbnailWithInfo(thumbnailInfo).height
    }

    private static func dimensionsForThumbnailWithInfo(thumbnailInfo: Message.ThumbnailInfo) -> (width: CGFloat, height: CGFloat) {
        let screenSize = UIScreen.mainScreen().bounds.size
        var imageSize = CGSize(width: thumbnailInfo.width, height: thumbnailInfo.height)
        if CGFloat(thumbnailInfo.width) + thumbnailPadding > screenSize.width {
            let newWidth = screenSize.width - thumbnailPadding
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

        pendingMessageListener?.stopListening()

        moviePlayer.pause()
        hideMoviePlayer()
    }

    override func updateWithMessage(message: Message, hasTail: Bool) {
        super.updateWithMessage(message, hasTail: hasTail)

        NSNotificationCenter.defaultCenter().removeObserver(self)

        let isPending = message.isPending
        if isPending {
            pendingMessageListener = PendingMessageListener(message: message, delegate: self)
        }

        updateThumbnail()

        mediaContainer.maskView = BubbleImageView(frame: CGRect(x: 0, y: 0, width: widthConstraint.constant, height: heightConstraint.constant), color: UIColor.blackColor(), direction: alignment == .Left ? .Left : .Right, hasTail: hasTail)
        messageImageView.alpha = isPending ? 0.5 : 1

        progressView.hidden = !isPending
        progressView.progress = 0
    }

    private func updateThumbnail() {
        guard let thumbnailInfo = message.thumbnailInfo else {
            heightConstraint.constant = 0
            messageImageView.hidden = true
            return
        }

        playIcon.hidden = message.type != .Movie || message.isPending

        let screenSize = UIScreen.mainScreen().bounds.size
        var imageSize = CGSize(width: thumbnailInfo.width, height: thumbnailInfo.height)
        if CGFloat(thumbnailInfo.width) + BubbleMediaRowTableViewCell.thumbnailPadding > screenSize.width {
            let newWidth = screenSize.width - BubbleMediaRowTableViewCell.thumbnailPadding
            imageSize = CGSize(width: newWidth, height: imageSize.height / imageSize.width * newWidth)
        }

        widthConstraint.constant = imageSize.width
        heightConstraint.constant = round(imageSize.height)

        loadImageWithId(thumbnailInfo.id)
    }

    private func loadImageWithId(thumbnailId: String) {
        messageImageView.image = nil

        FileHelper.getFileWithId(thumbnailId, secretKey: message.secretKey) {
            file in

            dispatch_async(dispatch_get_main_queue()) {
                self.messageImageView.image = nil
            }

            if let image = file?.image {
                dispatch_async(dispatch_get_main_queue()) {
                    self.messageImageView.image = image
                    self.messageImageView.hidden = false
                }
            }
        }
    }

    private func hideMoviePlayer() {
        loadingIndicator.hidden = true
        playIcon.hidden = false
        moviePlayer.hidden = true
    }

    @objc private func didTapImage() {
        if message.isPending {
            return
        }

        if let movieInfo = message.movieInfo {
            playIcon.hidden = true

            loadingIndicator.hidden = false
            loadingIndicator.startAnimating()

            FileHelper.getFileWithId(movieInfo.id, secretKey: message.secretKey) {
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

extension BubbleMediaRowTableViewCell: MoviePlayerDelegate {
    func moviePlayerDidBeginPlaying(moviePlayer: MoviePlayer) {
        moviePlayer.hidden = false
        loadingIndicator.hidden = true
    }

    func moviePlayerDidReachEnd(moviePlayer: MoviePlayer) {
        hideMoviePlayer()
    }
}

extension BubbleMediaRowTableViewCell: PendingMessageListenerDelegate {
    func pendingMessageListener(pendingMessageListener: PendingMessageListener, didUpdateProgress progress: Float) {
        progressView.progress = progress
    }

    func pendingMessageListenerDidComplete(pendingMessageListener: PendingMessageListener) {
        messageImageView.alpha = 1
        progressView.hidden = true
        updateThumbnail()
    }
}