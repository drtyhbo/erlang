
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
    private let headerReuseIdentifier = "HeaderReuseIdentifier"

    private let friendsSection = 0
    private let groupChatsSection = 1

    private let topHeaderHeight: CGFloat = 40
    private let headerHeight: CGFloat = 60

    init() {
        super.init(nibName: "FriendsListViewController", bundle: nil)
    }

    required init?(coder aDecoder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    override func viewDidLoad() {
        super.viewDidLoad()

        friendsTable.registerNib(UINib(nibName: "FriendsListHeader", bundle: nil), forHeaderFooterViewReuseIdentifier: headerReuseIdentifier)
        friendsTable.registerNib(UINib(nibName: "FriendTableViewCell", bundle: nil), forCellReuseIdentifier: friendCellReuseIdentifier)

        let contacts = ContactsHelper().getAllContacts().filter({ $0.phoneNumber.fullNumber != User.phoneNumber })
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
}

extension FriendsListViewController: UITableViewDataSource, UITableViewDelegate {
    func numberOfSectionsInTableView(tableView: UITableView) -> Int {
        return 2
    }

    func tableView(tableView: UITableView, viewForHeaderInSection section: Int) -> UIView? {
        let header = tableView.dequeueReusableHeaderFooterViewWithIdentifier(headerReuseIdentifier) as! FriendsListHeader
        header.headerType = section == friendsSection ? FriendsListHeaderType.Friends : FriendsListHeaderType.GroupChats
        return header
    }

    func tableView(tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return section == friendsSection ? FriendManager.sharedManager.friends.count : 0
    }

    func tableView(tableView: UITableView, heightForRowAtIndexPath indexPath: NSIndexPath) -> CGFloat {
        if indexPath.section == friendsSection {
            return FriendTableViewCell.cellHeight
        } else {
            return 30
        }
    }

    func tableView(tableView: UITableView, heightForHeaderInSection section: Int) -> CGFloat {
        return section == 0 ? topHeaderHeight : headerHeight
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