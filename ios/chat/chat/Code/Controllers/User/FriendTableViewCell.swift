//
//  FriendTableViewCell.swift
//  chat
//
//  Created by Andreas Binnewies on 2/1/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

class FriendTableViewCell: UITableViewCell {
    @IBOutlet weak var name: UILabel!

    @IBOutlet weak var badge: UIView!
    @IBOutlet weak var unreadMessageCountLabel: UILabel!

    var friend: Friend? {
        didSet {
            configureCell()
        }
    }

    override func awakeFromNib() {
        super.awakeFromNib()
        badge.layer.cornerRadius = badge.bounds.size.height / 2
    }

    private func configureCell() {
        guard let friend = friend else {
            return
        }

        NSNotificationCenter.defaultCenter().removeObserver(self)
        NSNotificationCenter.defaultCenter().addObserver(self, selector: "unreadMessagesBadgeUpdated:", name: MessageManager.UnreadMessageCountUpdated, object: friend)

        name.text = friend.name
        updateBadgeWithCount(MessageManager.sharedManager.unreadMessageCountForFriend(friend))
    }

    private func updateBadgeWithCount(unreadMessageCount: Int) {
        badge.hidden = unreadMessageCount == 0
        unreadMessageCountLabel.text = String(unreadMessageCount)
    }

    @objc private func unreadMessagesBadgeUpdated(notification: NSNotification) {
        if let unreadMessageCount = notification.userInfo?["unreadMessageCount"] as? Int {
            updateBadgeWithCount(unreadMessageCount)
        }
    }
}
