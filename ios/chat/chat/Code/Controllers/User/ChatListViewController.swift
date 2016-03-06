
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

protocol ChatListViewControllerDelegate: class {
    func chatListViewController(chatListViewController: ChatListViewController, didSelectChat chat: Chat)
}

class ChatListViewController: UIViewController {
    @IBOutlet weak var chatsTable: UITableView!

    weak var delegate: ChatListViewControllerDelegate?

    private let friendCellReuseIdentifier = "FriendTableViewCell"
    private let headerReuseIdentifier = "HeaderReuseIdentifier"
    private let topicCellReuseIdentifier = "TopicTableViewCell"

    private let chatsSection = 0
    private let topicsSection = 1

    private let topHeaderHeight: CGFloat = 30
    private let headerHeight: CGFloat = 50

    private var topics: [String] = []
    private var chats: [Chat] = []

    init() {
        super.init(nibName: "ChatListViewController", bundle: nil)
    }

    required init?(coder aDecoder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    override func viewDidLoad() {
        super.viewDidLoad()

        chatsTable.registerNib(UINib(nibName: "FriendsListHeader", bundle: nil), forHeaderFooterViewReuseIdentifier: headerReuseIdentifier)
        chatsTable.registerNib(UINib(nibName: "FriendTableViewCell", bundle: nil), forCellReuseIdentifier: friendCellReuseIdentifier)
        chatsTable.registerNib(UINib(nibName: "TopicTableViewCell", bundle: nil), forCellReuseIdentifier: topicCellReuseIdentifier)

        chatsTable.contentInset = UIEdgeInsets(top: 10, left: 0, bottom: 0, right: 0)

        let contacts = ContactsHelper().getAllContacts().filter({ $0.phoneNumber.fullNumber != User.phoneNumber })
        FriendManager.sharedManager.loadFriendsFromContacts(contacts) {
            self.loadChats()
        }
    }

    private func loadChats() {
        chats = Chat.findAll()
        chatsTable.reloadData()
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

extension ChatListViewController: UITableViewDataSource, UITableViewDelegate {
    func numberOfSectionsInTableView(tableView: UITableView) -> Int {
        return 2
    }

    func tableView(tableView: UITableView, viewForHeaderInSection section: Int) -> UIView? {
        let header = tableView.dequeueReusableHeaderFooterViewWithIdentifier(headerReuseIdentifier) as! FriendsListHeader
        header.delegate = self
        header.headerType = section == chatsSection ? FriendsListHeaderType.Chats : FriendsListHeaderType.Topics
        return header
    }

    func tableView(tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return section == chatsSection ? chats.count : topics.count
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
            cell.chat = chats[indexPath.row]
            return cell
        } else {
            let cell = tableView.dequeueReusableCellWithIdentifier(topicCellReuseIdentifier, forIndexPath: indexPath) as! TopicTableViewCell
            cell.name = topics[indexPath.row]
            return cell
        }
    }

    func tableView(tableView: UITableView, didSelectRowAtIndexPath indexPath: NSIndexPath) {
        delegate?.chatListViewController(self, didSelectChat: chats[indexPath.row])
    }

    private func createNewChat() {
        let friendSelectorViewController = FriendSelectorViewController()
        friendSelectorViewController.delegate = self
        presentViewController(UINavigationController(rootViewController: friendSelectorViewController), animated: true, completion: nil)
    }

    private func createNewTopic() {
        let alertController = UIAlertController(title: "New Topic", message: "Enter a name for your topic.", preferredStyle: .Alert)
        alertController.addTextFieldWithConfigurationHandler { textField in
            textField.placeholder = "Topic name"
            textField.autocapitalizationType = .Sentences
        }

        let createAction = UIAlertAction(title: "Create", style: .Default) { action in
            let topicName = alertController.textFields![0].text ?? ""

            if !topicName.isEmpty {
                self.topics.append(topicName)
                self.chatsTable.reloadSections(NSIndexSet(index: self.topicsSection), withRowAnimation: .None)
                
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

extension ChatListViewController: FriendsListHeaderDelegate {
    func friendsListHeaderDidTapAdd(friendsListHeader: FriendsListHeader) {
        if friendsListHeader.headerType == .Chats {
            createNewChat()
        } else if friendsListHeader.headerType == .Topics {
            createNewTopic()
        }
    }
}

extension ChatListViewController: FriendSelectorViewControllerDelegate {
    func friendSelectorViewController(friendSelectorViewController: FriendSelectorViewController, didSelectFriends friends: [Friend]) {
        friendSelectorViewController.dismissViewControllerAnimated(true, completion: nil)

        Chat.createWithParticipants(friends)
        CoreData.save()

        loadChats()
    }
}