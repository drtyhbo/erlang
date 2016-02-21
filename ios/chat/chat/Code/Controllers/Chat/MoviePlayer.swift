//
//  MoviePlayer.swift
//  chat
//
//  Created by Andreas Binnewies on 2/20/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import AVFoundation
import Foundation
import UIKit

protocol MoviePlayerDelegate: class {
    func moviePlayerDidBeginPlaying(moviePlayer: MoviePlayer)
    func moviePlayerDidReachEnd(moviePlayer: MoviePlayer)
}

class MoviePlayer: UIView {
    weak var delegate: MoviePlayerDelegate?

    var asset: AVAsset? {
        didSet {
            playerLayer.player?.pause()

            if let asset = asset {
                let playerItem = AVPlayerItem(asset: asset)

                NSNotificationCenter.defaultCenter().removeObserver(self)
                NSNotificationCenter.defaultCenter().addObserver(self, selector: "didReachEnd", name: AVPlayerItemDidPlayToEndTimeNotification, object:
                    playerItem)

                let player = AVPlayer(playerItem: playerItem)
                playerLayer.player = player
                player.play()
            }
        }
    }

    var playerLayer: AVPlayerLayer {
        return layer as! AVPlayerLayer
    }

    private var kvoContext = 0
    
    deinit {
        removeObserver(self, forKeyPath: "playerLayer.readyForDisplay", context: &kvoContext)
    }
    
    override class func layerClass() -> AnyClass {
        return AVPlayerLayer.self
    }
    
    required init?(coder aDecoder: NSCoder) {
        super.init(coder: aDecoder)
        
        self.playerLayer.videoGravity = AVLayerVideoGravityResizeAspectFill
        addObserver(self, forKeyPath: "playerLayer.readyForDisplay", options: [.New, .Initial], context: &kvoContext)
    }
    
    override func observeValueForKeyPath(keyPath: String?, ofObject object: AnyObject?, change: [String : AnyObject]?, context: UnsafeMutablePointer<Void>) {
        if keyPath == "playerLayer.readyForDisplay" && playerLayer.readyForDisplay {
            delegate?.moviePlayerDidBeginPlaying(self)
        }
    }

    func pause() {
        NSNotificationCenter.defaultCenter().removeObserver(self)
        playerLayer.player?.pause()
    }

    @objc private func didReachEnd() {
        delegate?.moviePlayerDidReachEnd(self)
    }
}