//
//  ReconnectView.swift
//  chat
//
//  Created by Andreas Binnewies on 2/8/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import ChatCommon
import Foundation
import TWTToast

class ReconnectView: TWTNibBackedView {
    @IBOutlet weak var backgroundView: ThemedView!
    @IBOutlet weak var reconnectionLabel: UILabel!

    private var updateReconnectionTimer: NSTimer?

    override class func nibName() -> String! {
        return "ReconnectView"
    }

    override func awakeFromNib() {
        super.awakeFromNib()
        backgroundView.colorType = .Dark
        addGestureRecognizer(UITapGestureRecognizer(target: self, action: "didTap"))
    }

    func show() {
        updateReconnectionLabel()
        updateReconnectionTimer = NSTimer.scheduledTimerWithTimeInterval(0.5, target: self, selector: "updateReconnectionLabel", userInfo: nil, repeats: true)
    }

    @objc private func didTap() {
        ChatClient.sharedClient.maybeConnect()
    }

    @objc private func updateReconnectionLabel() {
        if let timeToReconnect = ChatClient.sharedClient.timeToReconnect {
            reconnectionLabel.text = "Reconnecting in \(Int(round(timeToReconnect)))..."
        } else {
            reconnectionLabel.text = "Reconnecting..."
        }
    }
}