//
//  UserInfoTableViewCell.swift
//  chat
//
//  Created by Andreas Binnewies on 3/15/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

protocol UserInfoTableViewCellDelegate: class {
    func userInfoTableViewCellShouldUpdateProfilePic(userInfoTableViewCell: UserInfoTableViewCell)
}

class UserInfoTableViewCell: UITableViewCell {
    @IBOutlet weak var profilePicImage: ChatProfilePic!
    @IBOutlet weak var firstName: UITextField!
    @IBOutlet weak var lastName: UITextField!

    static let cellHeight: CGFloat = 85

    weak var delegate: UserInfoTableViewCellDelegate?

    override func awakeFromNib() {
        super.awakeFromNib()
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
}