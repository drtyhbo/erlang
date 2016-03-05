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
    case Friends
    case GroupChats
}

class FriendsListHeader: UITableViewHeaderFooterView {
    @IBOutlet weak var label: UILabel!
    @IBOutlet weak var addButton: UIButton!

    var headerType: FriendsListHeaderType = .Friends {
        didSet {
            switch (headerType) {
                case .Friends:
                    label.text = "FRIENDS"
                    addButton.hidden = true
                case .GroupChats:
                    label.text = "GROUP CHATS"
                    addButton.hidden = false
            }
        }
    }
}