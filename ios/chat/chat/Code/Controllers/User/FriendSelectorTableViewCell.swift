//
//  FriendSelectorTableViewCell.swift
//  chat
//
//  Created by Andreas Binnewies on 3/6/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

protocol FriendSelectorTableViewCellDelegate: class {
    func friendSelectorTableViewCell(friendSelectorTableViewCell: FriendSelectorTableViewCell, isChecked: Bool)
}

class FriendSelectorTableViewCell: UITableViewCell {
    @IBOutlet weak var name: UILabel!
    @IBOutlet weak var profilePic: ChatProfilePic!
    @IBOutlet weak var checkMark: CheckMark!

    weak var delegate: FriendSelectorTableViewCellDelegate?

    static let rowHeight: CGFloat = 60

    var friend: Friend? {
        didSet {
            configureCell()
        }
    }

    var isChecked: Bool {
        get {
            return checkMark.isChecked
        }
        set {
            checkMark.isChecked = newValue
        }
    }

    override func awakeFromNib() {
        super.awakeFromNib()
        addGestureRecognizer(UITapGestureRecognizer(target: self, action: "didTap"))
    }

    private func configureCell() {
        name.text = friend?.name ?? ""
        profilePic.sd_setImageWithURL(friend?.profilePicUrl)
    }

    @objc private func didTap() {
        checkMark.isChecked = !checkMark.isChecked
        delegate?.friendSelectorTableViewCell(self, isChecked: checkMark.isChecked)
    }
}