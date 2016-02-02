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

    var friend: Friend? {
        didSet {
            configureCell()
        }
    }

    private func configureCell() {
        if let friend = friend {
            name.text = friend.name
        }
    }
}
