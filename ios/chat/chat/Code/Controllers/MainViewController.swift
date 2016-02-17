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

class MainViewController: UIViewController {
    private let currentFriendKey = "CurrentFriend"
    private let slideViewController: APLSlideMenuViewController

    private var reconnectView: ReconnectView!

    init() {
        slideViewController = APLSlideMenuViewController()

        super.init(nibName: nil, bundle: nil)

        NSNotificationCenter.defaultCenter().addObserver(self, selector: "chatClientConnecting", name: ChatClient.ChatClientConnectingNotification, object: nil)
        NSNotificationCenter.defaultCenter().addObserver(self, selector: "chatClientDidConnect", name: ChatClient.ChatClientDidConnectNotification, object: nil)
        NSNotificationCenter.defaultCenter().addObserver(self, selector: "chatClientDidDisconnect", name: ChatClient.ChatClientDidDisconnectNotification, object: nil)

    }

    required init?(coder aDecoder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    override func viewDidLoad() {
        super.viewDidLoad()

        navigationController?.setNavigationBarHidden(true, animated: true)

        slideViewController.view.frame = view.bounds
        slideViewController.bouncing = true
        slideViewController.gestureSupport = .Drag

        let friendsListViewController = FriendsListViewController()
        friendsListViewController.delegate = self
        slideViewController.leftMenuViewController = friendsListViewController

        let chatViewController = ChatViewController()
        slideViewController.contentViewController = chatViewController

        view.addSubview(slideViewController.view)
        addChildViewController(slideViewController)

        let friendId = NSUserDefaults.standardUserDefaults().integerForKey(currentFriendKey)
        if friendId > 0 {
            if let friend = Friend.findWithId(friendId) {
                chatViewController.friend = friend
            }
        }
    }

    override func viewDidLayoutSubviews() {
        super.viewDidLayoutSubviews()
        slideViewController.view.bounds.size = view.bounds.size
    }

    private func showReconnectView() {
        if reconnectView == nil {
            reconnectView = ReconnectView(frame: CGRect(x: 0, y: 0, width: view.bounds.size.width, height: 50))
        }

        view.insertSubview(reconnectView, atIndex: 0)

        UIView.animateWithDuration(0.25) {
            self.slideViewController.view.frame.origin.y = 50
        }
    }

    private func hideReconnectView() {
        UIView.animateWithDuration(0.25) {
            self.slideViewController.view.frame.origin.y = 0
        }
    }

    @objc private func chatClientConnecting() {
        showReconnectView()
    }

    @objc private func chatClientDidConnect() {
        hideReconnectView()
    }

    @objc private func chatClientDidDisconnect() {
        showReconnectView()
    }
}

extension MainViewController: FriendsListViewControllerDelegate {
    func friendsListViewController(friendsListViewController: FriendsListViewController, didSelectFriend friend: Friend) {
        NSUserDefaults.standardUserDefaults().setInteger(friend.id, forKey: currentFriendKey)
        NSUserDefaults.standardUserDefaults().synchronize()

        let chatViewController = slideViewController.contentViewController as! ChatViewController
        chatViewController.friend = friend

        slideViewController.hideMenu(true)
    }
}