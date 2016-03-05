
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
    private let topicCellReuseIdentifier = "TopicTableViewCell"

    private let chatsSection = 0
    private let topicsSection = 1

    private let topHeaderHeight: CGFloat = 30
    private let headerHeight: CGFloat = 50

    private var topics: [String] = []

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
        friendsTable.registerNib(UINib(nibName: "TopicTableViewCell", bundle: nil), forCellReuseIdentifier: topicCellReuseIdentifier)

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
        header.headerType = section == chatsSection ? FriendsListHeaderType.Chats : FriendsListHeaderType.Topics
        header.delegate = section == chatsSection ? nil : self
        return header
    }

    func tableView(tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return section == chatsSection ? FriendManager.sharedManager.friends.count : topics.count
    }

    func tableView(tableView: UITableView, heightForRowAtIndexPath indexPath: NSIndexPath) -> CGFloat {
        return indexPath.section == chatsSection ? FriendTableViewCell.cellHeight : TopicTableViewCell.cellHeight
    }

    func tableView(tableView: UITableView, heightForHeaderInSection section: Int) -> CGFloat {
        return section == 0 ? topHeaderHeight : headerHeight
    }

    func tableView(tableView: UITableView, cellForRowAtIndexPath indexPath: NSIndexPath) -> UITableViewCell {
        if indexPath.section == chatsSection {
            let cell = tableView.dequeueReusableCellWithIdentifier(friendCellReuseIdentifier, forIndexPath: indexPath) as! FriendTableViewCell
            cell.friend = FriendManager.sharedManager.friends[indexPath.row]
            return cell
        } else {
            let cell = tableView.dequeueReusableCellWithIdentifier(topicCellReuseIdentifier, forIndexPath: indexPath) as! TopicTableViewCell
            cell.name = topics[indexPath.row]
            return cell
        }
    }

    func tableView(tableView: UITableView, didSelectRowAtIndexPath indexPath: NSIndexPath) {
        delegate?.friendsListViewController(self, didSelectFriend: FriendManager.sharedManager.friends[indexPath.row])
    }
}

extension FriendsListViewController: FriendsListHeaderDelegate {
    func friendsListHeaderDidTapAdd(friendsListHeader: FriendsListHeader) {
        let alertController = UIAlertController(title: "New Topic", message: "Enter a name for your topic.", preferredStyle: .Alert)
        alertController.addTextFieldWithConfigurationHandler { textField in
            textField.placeholder = "Topic name"
            textField.autocapitalizationType = .Sentences
        }

        let createAction = UIAlertAction(title: "Create", style: .Default) { action in
            let topicName = alertController.textFields![0].text ?? ""

            if !topicName.isEmpty {
                self.topics.append(topicName)
                self.friendsTable.reloadSections(NSIndexSet(index: self.topicsSection), withRowAnimation: .None)
                
                alertController.dismissViewControllerAnimated(true, completion: nil)
            }
        }
        alertController.addAction(createAction)

        let cancelAction = UIAlertAction(title: "Cancel", style: .Cancel) { action in
            alertController.dismissViewControllerAnimated(true, completion: nil)
        }
        alertController.addAction(cancelAction)

        presentViewController(alertController, animated: true, completion: nil)
    }
}