//
//  ConversationStartTableViewCell.swift
//  chat
//
//  Created by Andreas Binnewies on 2/21/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

class ConversationStartTableViewCell: UITableViewCell {
    @IBOutlet weak var label: UILabel!

    static let rowHeight: CGFloat = 80

    var friend: Friend? {
        didSet {
            if let friend = friend {
                label.text = "This is the start of your chat history with \(friend.fullName)"
            }
        }
    }
}