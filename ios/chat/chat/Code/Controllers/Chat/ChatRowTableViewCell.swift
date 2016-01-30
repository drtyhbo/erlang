//
//  ChatRowTableViewCell.swift
//  chat
//
//  Created by Andreas Binnewies on 1/13/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import SDWebImage
import UIKit

class ChatRowTableViewCell: MessageTableViewCell {
    @IBOutlet weak var userImage: UIImageView!
    @IBOutlet weak var userName: UILabel!
    @IBOutlet weak var dateLabel: UILabel!
    @IBOutlet weak var messageLabel: UILabel!

    override var message: Message! {
        didSet {
/*            userImage.sd_setImageWithURL(NSURL(string: message.author.profilePic!.url!))
            userName.text = message.author.name!

            let timeFormatter = NSDateFormatter()
            timeFormatter.dateFormat = "H:mm"
            dateLabel.text = timeFormatter.stringFromDate(message.createdAt!)

            messageLabel.text = message.message*/
        }
    }
}