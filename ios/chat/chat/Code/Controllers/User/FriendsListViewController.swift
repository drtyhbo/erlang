
//
//  FriendsListViewController.swift
//  chat
//
//  Created by Andreas Binnewies on 2/1/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Contacts
import Foundation
import UIKit

protocol FriendsListViewControllerDelegate: class {
    func friendsListViewController(friendsListViewController: FriendsListViewController, didSelectFriend friend: Friend)
}

class FriendsListViewController: UIViewController {
    @IBOutlet weak var friendsTable: UITableView!
    @IBOutlet weak var profilePic: ChatProfilePic!
    @IBOutlet weak var updatingProfilePicActivityIndicator: UIActivityIndicatorView!

    weak var delegate: FriendsListViewControllerDelegate?

    private let friendCellReuseIdentifier = "FriendTableViewCell"

    private var imagePickerController: UIImagePickerController?

    init() {
        super.init(nibName: "FriendsListViewController", bundle: nil)
    }

    required init?(coder aDecoder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    override func viewDidLoad() {
        super.viewDidLoad()

        friendsTable.registerNib(UINib(nibName: "FriendTableViewCell", bundle: nil), forCellReuseIdentifier: friendCellReuseIdentifier)

        profilePic.image = User.profilePic ?? UIImage(named: "ProfilePic")
        profilePic.addGestureRecognizer(UITapGestureRecognizer(target: self, action: "didTapProfilePic"))

        let contacts = ContactsHelper().getAllContacts().filter({ $0.phoneNumber.toString() != User.phoneNumber })
        FriendManager.sharedManager.loadFriendsFromContacts(contacts) {
            self.friendsTable.reloadData()
        }
    }

    private func requestContactsAccess(completionHandler: Bool->Void) {
        let authorizationStatus = CNContactStore.authorizationStatusForEntityType(CNEntityType.Contacts)
     
        switch authorizationStatus {
        case .Authorized:
            completionHandler(true)
     
        case .Denied, .NotDetermined:
            CNContactStore().requestAccessForEntityType(CNEntityType.Contacts, completionHandler: {
                access, accessError in
                completionHandler(access)
            })
     
        default:
            completionHandler(false)
        }
    }

    @objc private func didTapProfilePic() {
        let imagePickerController = UIImagePickerController()
        imagePickerController.delegate = self
        imagePickerController.sourceType = .PhotoLibrary
        imagePickerController.allowsEditing = true
        presentViewController(imagePickerController, animated: true, completion: nil)

        self.imagePickerController = imagePickerController
    }
}

extension FriendsListViewController: UITableViewDataSource, UITableViewDelegate {
    func numberOfSectionsInTableView(tableView: UITableView) -> Int {
        return 1
    }

    func tableView(tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return FriendManager.sharedManager.friends.count
    }

    func tableView(tableView: UITableView, heightForRowAtIndexPath indexPath: NSIndexPath) -> CGFloat {
        return FriendTableViewCell.cellHeight
    }

    func tableView(tableView: UITableView, cellForRowAtIndexPath indexPath: NSIndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCellWithIdentifier(friendCellReuseIdentifier, forIndexPath: indexPath) as! FriendTableViewCell
        cell.friend = FriendManager.sharedManager.friends[indexPath.row]
        return cell
    }

    func tableView(tableView: UITableView, didSelectRowAtIndexPath indexPath: NSIndexPath) {
        delegate?.friendsListViewController(self, didSelectFriend: FriendManager.sharedManager.friends[indexPath.row])
    }
}

extension FriendsListViewController: UIImagePickerControllerDelegate, UINavigationControllerDelegate {
    func imagePickerController(picker: UIImagePickerController, didFinishPickingMediaWithInfo info: [String : AnyObject]) {
        if let image = info[UIImagePickerControllerEditedImage] as? UIImage {
            let resizedImage = image.resizeToSize(Constants.profilePicSize)
            self.profilePic.alpha = 0.75

            self.updatingProfilePicActivityIndicator.hidden = false
            self.updatingProfilePicActivityIndicator.startAnimating()

            APIManager.sharedManager.uploadProfilePic(resizedImage) {
                success in
                self.profilePic.alpha = 1
                self.updatingProfilePicActivityIndicator.hidden = true

                if success {
                    self.profilePic.image = resizedImage
                    User.profilePic = resizedImage
                }
            }
        }

        dismissViewControllerAnimated(true, completion: nil)
        self.imagePickerController = nil
    }
}