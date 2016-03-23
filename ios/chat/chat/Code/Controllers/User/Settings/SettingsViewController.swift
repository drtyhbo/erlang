//
//  SettingsViewController.swift
//  chat
//
//  Created by Andreas Binnewies on 3/15/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import ChatCommon
import Foundation
import UIKit

class SettingsViewController: UIViewController {
    @IBOutlet weak var tableView: UITableView!

    private let userInfoCellReuseIdentifier = "UserInfoTableViewCell"
    private let themePickerCellReuseIdentifier = "ThemePickerTableViewCell"
    private let fontPickerCellReuseIdentifier = "FontPickerTableViewCell"

    private let numberOfSections = 2

    private let userInfoSection = 0
    private let userInfoRow = 0

    private let themeSection = 1
    private let themePickerRow = 0
    private let fontPickerRow = 1

    private var imagePickerController: UIImagePickerController?

    override func viewDidLoad() {
        super.viewDidLoad()

        navigationItem.title = "Settings"
        navigationItem.rightBarButtonItem = UIBarButtonItem(image: UIImage(named: "Close")!, style: .Plain, target: self, action: "didTapClose")

        tableView.registerNib(UINib(nibName: "UserInfoTableViewCell", bundle: nil), forCellReuseIdentifier: userInfoCellReuseIdentifier)
        tableView.registerNib(UINib(nibName: "ThemePickerTableViewCell", bundle: nil), forCellReuseIdentifier: themePickerCellReuseIdentifier)
        tableView.registerNib(UINib(nibName: "FontPickerTableViewCell", bundle: nil), forCellReuseIdentifier: fontPickerCellReuseIdentifier)
    }

    @objc private func didTapClose() {
        dismissViewControllerAnimated(true, completion: nil)
    }
}

extension SettingsViewController: UITableViewDelegate, UITableViewDataSource {
    func numberOfSectionsInTableView(tableView: UITableView) -> Int {
        return numberOfSections
    }

    func tableView(tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        if section == userInfoSection {
            return 1
        } else if section == themeSection {
            return 2
        }

        return 0
    }

    func tableView(tableView: UITableView, cellForRowAtIndexPath indexPath: NSIndexPath) -> UITableViewCell {
        if indexPath.section == userInfoSection {
            if indexPath.row == userInfoRow {
                let cell = tableView.dequeueReusableCellWithIdentifier(userInfoCellReuseIdentifier, forIndexPath: indexPath) as! UserInfoTableViewCell
                cell.delegate = self
                return cell
            }
        } else if indexPath.section == themeSection {
            if indexPath.row == themePickerRow {
                return tableView.dequeueReusableCellWithIdentifier(themePickerCellReuseIdentifier, forIndexPath: indexPath)
            } else if indexPath.row == fontPickerRow {
                return tableView.dequeueReusableCellWithIdentifier(fontPickerCellReuseIdentifier, forIndexPath: indexPath)
            }
        }

        return UITableViewCell()
    }

    func tableView(tableView: UITableView, heightForRowAtIndexPath indexPath: NSIndexPath) -> CGFloat {
        if indexPath.section == userInfoSection {
            return indexPath.row == userInfoRow ? UserInfoTableViewCell.cellHeight : ThemePickerTableViewCell.cellHeight
        }
        return 48
    }

    func tableView(tableView: UITableView, didSelectRowAtIndexPath indexPath: NSIndexPath) {
        if indexPath.section == themeSection && indexPath.row == fontPickerRow {
            let fontPickerViewController = FontPickerViewController()
            fontPickerViewController.delegate = self
            navigationController?.pushViewController(fontPickerViewController, animated: true)
        }
    }
}

extension SettingsViewController: UserInfoTableViewCellDelegate {
    func userInfoTableViewCellShouldUpdateProfilePic(userInfoTableViewCell: UserInfoTableViewCell) {
        let imagePickerController = UIImagePickerController()
        imagePickerController.delegate = self
        imagePickerController.sourceType = .PhotoLibrary
        imagePickerController.allowsEditing = true
        presentViewController(imagePickerController, animated: true, completion: nil)

        self.imagePickerController = imagePickerController
    }
}

extension SettingsViewController: UIImagePickerControllerDelegate, UINavigationControllerDelegate {
    func imagePickerController(picker: UIImagePickerController, didFinishPickingMediaWithInfo info: [String : AnyObject]) {
        if let image = info[UIImagePickerControllerEditedImage] as? UIImage {
            let profilePic = image.resizeToSize(Constants.profilePicSize)
            APIManager.sharedManager.uploadProfilePic(profilePic) {
                success in
                if success {
                    User.profilePic = profilePic
                    self.tableView.reloadData()
                }
            }
        }

        picker.dismissViewControllerAnimated(true, completion: nil)
        self.imagePickerController = nil
    }
}

extension SettingsViewController: FontPickerViewControllerDelegate {
    func fontPickerViewController(fontPickerViewController: FontPickerViewController, didSelectCustomFont customFont: CustomFont) {
        tableView.reloadData()
    }
}