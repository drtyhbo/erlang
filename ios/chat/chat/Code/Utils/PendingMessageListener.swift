//
//  PendingMessageListener.swift
//  chat
//
//  Created by Andreas Binnewies on 3/26/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import ChatCommon
import Foundation

@objc protocol PendingMessageListenerDelegate {
    optional func pendingMessageListener(pendingMessageListener: PendingMessageListener, didUpdateProgress progress: Float)
    optional func pendingMessageListenerDidComplete(pendingMessageListener: PendingMessageListener)
}

class PendingMessageListener: NSObject {
    deinit {
        stopListening()
    }

    private let message: Message
    private let delegate: PendingMessageListenerDelegate

    init(message: Message, delegate: PendingMessageListenerDelegate) {
        self.message = message
        self.delegate = delegate

        super.init()

        if message.isPending {
            NSNotificationCenter.defaultCenter().addObserver(self, selector: "didUpdateProgressNotification:", name: MessageManager.SendingMessageProgressNotification, object: nil)
            NSNotificationCenter.defaultCenter().addObserver(self, selector: "didFinishSendingNotification:", name: MessageManager.FinishedSendingMessageNotification, object: nil)
        }
    }

    func stopListening() {
        NSNotificationCenter.defaultCenter().removeObserver(self)
    }

    @objc private func didUpdateProgressNotification(notification: NSNotification) {
        guard let senderMessage = notification.object as? Message, percentComplete = notification.userInfo?["percentComplete"] as? Float where senderMessage.localId == message.localId else {
            return
        }

        delegate.pendingMessageListener?(self, didUpdateProgress: percentComplete)
    }

    @objc private func didFinishSendingNotification(notification: NSNotification) {
        guard let senderMessage = notification.object as? Message where senderMessage.localId == message.localId else {
            return
        }

        NSNotificationCenter.defaultCenter().removeObserver(self)
        delegate.pendingMessageListenerDidComplete?(self)
    }
}