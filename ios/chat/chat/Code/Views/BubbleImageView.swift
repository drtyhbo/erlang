//
//  BubbleImageView.swift
//  chat
//
//  Created by Andreas Binnewies on 3/22/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

class BubbleImageView: UIImageView {
    enum Direction {
        case Left
        case Right
    }

    private var direction: Direction!
    private var color: UIColor!

    init(frame: CGRect, color: UIColor, direction: Direction) {
        super.init(frame: frame)
        setColor(color, direction: direction)
    }

    required init?(coder aDecoder: NSCoder) {
        super.init(coder: aDecoder)
    }

    func setColor(color: UIColor, direction: Direction) {
        self.color = color
        self.direction = direction
        updateBubble()
    }

    private func updateBubble() {
        let bubbleImage = UIImage(named: "Bubble")!
        let flippedBubbleImage = UIImage(CGImage: bubbleImage.CGImage!, scale: bubbleImage.scale, orientation: direction == .Right ? .Up : .UpMirrored).imageMaskedWithColor(color)
        let center = CGPoint(x: bubbleImage.size.width / 2, y: bubbleImage.size.height / 2)
        let capInsets = UIEdgeInsets(top: center.y, left: center.x, bottom: center.y, right: center.x)
        image = flippedBubbleImage.resizableImageWithCapInsets(capInsets, resizingMode: .Stretch)
    }
}