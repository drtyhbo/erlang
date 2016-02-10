
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

    weak var delegate: FriendsListViewControllerDelegate?

    private let friendCellReuseIdentifier = "FriendTableViewCell"

    init() {
        super.init(nibName: "FriendsListViewController", bundle: nil)
    }

    required init?(coder aDecoder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    override func viewDidLoad() {
        super.viewDidLoad()

        friendsTable.registerNib(UINib(nibName: "FriendTableViewCell", bundle: nil), forCellReuseIdentifier: friendCellReuseIdentifier)

        let contacts = ContactsHelper().getAllContacts()
        FriendManager.sharedManager.loadFriends(contacts.map({ $0.phoneNumber })) {
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
}

extension FriendsListViewController: UITableViewDataSource, UITableViewDelegate {
    func numberOfSectionsInTableView(tableView: UITableView) -> Int {
        return 1
    }

    func tableView(tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return FriendManager.sharedManager.friends.count
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