//
//  FriendsListHeader.swift
//  chat
//
//  Created by Andreas Binnewies on 3/5/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

enum FriendsListHeaderType {
    case Chats
    case Topics
}

protocol FriendsListHeaderDelegate: class {
    func friendsListHeaderDidTapAdd(friendsListHeader: FriendsListHeader)
}

class FriendsListHeader: UITableViewHeaderFooterView {
    @IBOutlet weak var label: UILabel!
    @IBOutlet weak var addButton: UIButton!

    weak var delegate: FriendsListHeaderDelegate?

    var headerType: FriendsListHeaderType = .Chats {
        didSet {
            switch (headerType) {
                case .Chats:
                    label.text = "CHATS"
                case .Topics:
                    label.text = "TOPICS"
            }
        }
    }

    @IBAction func didTapAdd() {
        delegate?.friendsListHeaderDidTapAdd(self)
    }
}