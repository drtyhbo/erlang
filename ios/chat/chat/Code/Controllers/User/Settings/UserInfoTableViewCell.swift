//
//  UserInfoTableViewCell.swift
//  chat
//
//  Created by Andreas Binnewies on 3/15/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import ChatCommon
import Foundation
import UIKit

protocol UserInfoTableViewCellDelegate: class {
    func userInfoTableViewCellShouldUpdateProfilePic(userInfoTableViewCell: UserInfoTableViewCell)
}

class UserInfoTableViewCell: UITableViewCell {
    @IBOutlet weak var profilePicImage: UIImageView!

    @IBOutlet weak var firstName: UITextField!
    @IBOutlet weak var firstNameActivityIndicator: UIActivityIndicatorView!

    @IBOutlet weak var lastName: UITextField!
    @IBOutlet weak var lastNameActivityIndicator: UIActivityIndicatorView!

    static let cellHeight: CGFloat = 85

    weak var delegate: UserInfoTableViewCellDelegate?

    override func awakeFromNib() {
        super.awakeFromNib()

        profilePicImage.contentMode = .ScaleAspectFit
        profilePicImage.layer.cornerRadius = 5
        profilePicImage.clipsToBounds = true

        selectionStyle = .None

        configureCell()
    }

    override func prepareForReuse() {
        super.prepareForReuse()
        configureCell()
    }

    private func configureCell() {
        profilePicImage.image = User.profilePic ?? UIImage(named: "ProfilePic")
        
        profilePicImage.addGestureRecognizer(UITapGestureRecognizer(target: self, action: "didTapProfilePic"))

        firstName.text = User.firstName ?? ""
        lastName.text = User.lastName ?? ""
    }

    @objc private func didTapProfilePic() {
        delegate?.userInfoTableViewCellShouldUpdateProfilePic(self)
    }

    @IBAction func firstNameChanged() {
        let firstNameText = firstName.text ?? ""
        if firstNameText.isEmpty {
            firstName.text = User.firstName ?? ""
            return
        }

        firstNameActivityIndicator.startAnimating()
        firstNameActivityIndicator.hidden = false
        APIManager.sharedManager.updateInfoWithFirstName(firstNameText, lastName: nil) { success in
            self.firstNameActivityIndicator.stopAnimating()
            if success {
                User.firstName = firstNameText
            }
        }
    }

    @IBAction func lastNameChanged() {
        let lastNameText = lastName.text ?? ""
        if lastNameText.isEmpty {
            lastName.text = User.lastName ?? ""
            return
        }

        lastNameActivityIndicator.startAnimating()
        lastNameActivityIndicator.hidden = false
        APIManager.sharedManager.updateInfoWithFirstName(nil, lastName: lastNameText) { success in
            self.lastNameActivityIndicator.stopAnimating()
            if success {
                User.lastName = lastNameText
            }
        }
    }
}

extension UserInfoTableViewCell: UITextFieldDelegate {
    func textFieldShouldReturn(textField: UITextField) -> Bool {
        if textField == firstName {
            lastName.becomeFirstResponder()
        } else {
            textField.resignFirstResponder()
        }
        return false
    }
}