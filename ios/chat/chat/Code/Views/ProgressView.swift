//
//  ProgressView.swift
//  chat
//
//  Created by Andreas Binnewies on 3/20/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import TWTToast

class ProgressView: TWTNibBackedView {
    @IBOutlet weak var backgroundView: ThemedView!

    var progress: Float = 0 {
        didSet {
            setNeedsDisplay()
        }
    }

    private let lineWidth: CGFloat = 6

    override class func nibName() -> String! {
        return "ProgressView"
    }

    required init?(coder aDecoder: NSCoder) {
        super.init(coder: aDecoder)
        backgroundView.colorType = .Dark
        backgroundView.clipsToBounds = true
    }

    override func layoutSubviews() {
        super.layoutSubviews()
        backgroundView.layoutIfNeeded()
        backgroundView.layer.cornerRadius = backgroundView.bounds.size.width / 2
    }

    override func drawRect(rect: CGRect) {
        super.drawRect(rect)

        let centerPosition = bounds.size.width / 2
        let radius = backgroundView.bounds.size.width / 2 + (lineWidth / 2 - 0.25)
        let startAngle = CGFloat(-M_PI / 2)
        let endAngle = startAngle + CGFloat(2 * M_PI * Double(progress))
        let bezierPath = UIBezierPath(arcCenter: CGPoint(x: centerPosition, y: centerPosition), radius: radius, startAngle: startAngle, endAngle: endAngle, clockwise: true)
        
        bezierPath.lineWidth = lineWidth
        ColorTheme.currentTheme.buttonColor.setStroke()
        bezierPath.stroke()
    }
}