//
//  FriendTableViewCell.swift
//  chat
//
//  Created by Andreas Binnewies on 2/1/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import ChatCommon
import Foundation
import UIKit

class FriendTableViewCell: UITableViewCell {
    @IBOutlet weak var name: UILabel!
    @IBOutlet weak var nameLeadingConstraint: NSLayoutConstraint!

    @IBOutlet weak var badge: UIView!
    @IBOutlet weak var unreadMessageCountLabel: UILabel!

    static let cellHeight: CGFloat = 40

    private let themeListener = ThemeListener()

    var chat: Chat? {
        didSet {
            configureCell()
        }
    }

    override func awakeFromNib() {
        super.awakeFromNib()
        badge.layer.cornerRadius = badge.bounds.size.height / 2

        let selectedView = UIView()
        selectedBackgroundView = selectedView

        updateTheme(ColorTheme.currentTheme)
        themeListener.themeChangeListener = { [weak self] theme in
            self?.updateTheme(theme)
        }
    }

    override func setSelected(selected: Bool, animated: Bool) {
        super.setSelected(selected, animated: animated)
        name.textColor = selected ? UIColor.whiteColor() : UIColor.blackColor()
        name.font = selected ? UIFont.boldCustomFontOfSize(name.font.pointSize) : UIFont.customFontOfSize(name.font.pointSize)
    }

    private func configureCell() {
        guard let chat = chat else {
            return
        }

        NSNotificationCenter.defaultCenter().removeObserver(self)
        NSNotificationCenter.defaultCenter().addObserver(self, selector: "unreadMessagesBadgeUpdated:", name: MessageManager.UnreadMessageCountUpdated, object: chat)

        name.text = chat.name
        updateBadgeWithCount(MessageManager.sharedManager.unreadMessageCountForChat(chat))
    }

    private func updateBadgeWithCount(unreadMessageCount: Int) {
        badge.hidden = unreadMessageCount == 0
        unreadMessageCountLabel.text = String(unreadMessageCount)
        badge.layoutIfNeeded()
        nameLeadingConstraint.constant = unreadMessageCount == 0 ? 10 : (badge.bounds.size.width + 20)
    }

    private func updateTheme(theme: ColorTheme) {
        selectedBackgroundView?.backgroundColor = theme.buttonColor
    }

    @objc private func unreadMessagesBadgeUpdated(notification: NSNotification) {
        if let unreadMessageCount = notification.userInfo?["unreadMessageCount"] as? Int {
            updateBadgeWithCount(unreadMessageCount)
        }
    }
}
