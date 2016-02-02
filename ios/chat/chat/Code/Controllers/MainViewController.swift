//
//  MainViewController.swift
//  chat
//
//  Created by Andreas Binnewies on 2/1/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import APLSlideMenu
import Foundation
import UIKit

class MainViewController: APLSlideMenuViewController {
    init() {
        super.init(nibName: nil, bundle: nil)

        bouncing = true
        gestureSupport = .Drag

        let friendsListViewController = FriendsListViewController()
        friendsListViewController.delegate = self
        leftMenuViewController = friendsListViewController

        let chatViewController = ChatViewController()
        contentViewController = chatViewController
    }

    required init?(coder aDecoder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
}

extension MainViewController: FriendsListViewControllerDelegate {
    func friendsListViewController(friendsListViewController: FriendsListViewController, didSelectFriend friend: Friend) {
        let chatViewController = contentViewController as! ChatViewController
        chatViewController.friend = friend

        hideMenu(true)
    }
}