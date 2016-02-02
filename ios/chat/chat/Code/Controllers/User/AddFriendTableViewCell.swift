//
//  AddFriendTableViewCell.swift
//  chat
//
//  Created by Andreas Binnewies on 2/1/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

class AddFriendTableViewCell: UITableViewCell {
    @IBOutlet weak var nameLabel: UILabel!
    @IBOutlet weak var loadingIndicator: UIActivityIndicatorView!
    @IBOutlet weak var requestSentLabel: UILabel!

    var contact: Contact? {
        didSet {
            if let contact = contact {
                nameLabel.text = contact.name
                requestSentLabel.hidden = !contact.requestSent
            }
        }
    }

    override func awakeFromNib() {
        super.awakeFromNib()

        addGestureRecognizer(UITapGestureRecognizer(target: self, action: "didTapAddFriend"))
    }

    override func prepareForReuse() {
        super.prepareForReuse()
        loadingIndicator.hidden = true
        requestSentLabel.hidden = true
    }

    @objc private func didTapAddFriend() {
        loadingIndicator.hidden = false

        if let contact = contact {
            APIManager.addFriendWithPhoneNumber(contact.phoneNumber) {
                success in

                if let contact = self.contact where success {
                    contact.requestSent = true
                    self.requestSentLabel.hidden = false
                }
                self.loadingIndicator.hidden = true
            }
        }
    }
}
