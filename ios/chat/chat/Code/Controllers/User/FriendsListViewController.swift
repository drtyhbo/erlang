
//
//  FriendsListViewController.swift
//  chat
//
//  Created by Andreas Binnewies on 2/1/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

protocol FriendsListViewControllerDelegate: class {
    func friendsListViewController(friendsListViewController: FriendsListViewController, didSelectFriend friend: Friend)
}

class FriendsListViewController: UIViewController {
    @IBOutlet weak var friendsTable: UITableView!

    weak var delegate: FriendsListViewControllerDelegate?

    private let friendCellReuseIdentifier = "FriendTableViewCell"

    private var friends: [Friend] = []

    init() {
        super.init(nibName: "FriendsListViewController", bundle: nil)
    }

    required init?(coder aDecoder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    override func viewDidLoad() {
        super.viewDidLoad()

        friendsTable.registerNib(UINib(nibName: "FriendTableViewCell", bundle: nil), forCellReuseIdentifier: friendCellReuseIdentifier)

        FriendManager.sharedManager.loadFriends {
        
            friends in

            self.friends = friends
            self.friendsTable.reloadData()
        }
    }
}

extension FriendsListViewController: UITableViewDataSource, UITableViewDelegate {
    func numberOfSectionsInTableView(tableView: UITableView) -> Int {
        return 1
    }

    func tableView(tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return friends.count
    }

    func tableView(tableView: UITableView, cellForRowAtIndexPath indexPath: NSIndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCellWithIdentifier(friendCellReuseIdentifier, forIndexPath: indexPath) as! FriendTableViewCell
        cell.friend = friends[indexPath.row]
        return cell
    }

    func tableView(tableView: UITableView, didSelectRowAtIndexPath indexPath: NSIndexPath) {
        delegate?.friendsListViewController(self, didSelectFriend: friends[indexPath.row])
    }
}