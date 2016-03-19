//
//  MainViewController.swift
//  chat
//
//  Created by Andreas Binnewies on 2/1/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import APLSlideMenu
import ChatCommon
import Foundation
import UIKit

class MainViewController: UIViewController {
    private let currentChatKey = "CurrentChat"
    private let slideViewController: APLSlideMenuViewController

    private var reconnectView: ReconnectView!
    private var reconnectViewTimer: NSTimer!

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

        let chatListViewController = ChatListViewController()
        chatListViewController.delegate = self
        slideViewController.leftMenuViewController = chatListViewController

        let chatViewController = ChatViewController()
        slideViewController.contentViewController = chatViewController

        view.addSubview(slideViewController.view)
        addChildViewController(slideViewController)
    }

    override func viewWillAppear(animated: Bool) {
        super.viewWillAppear(animated)

        if let chatId = NSUserDefaults.standardUserDefaults().URLForKey(currentChatKey), chat = Chat.findWithId(chatId) {
            (slideViewController.contentViewController as! ChatViewController).chat = chat
        } else {
            slideViewController.showLeftMenu(true)
        }
    }

    override func viewDidLayoutSubviews() {
        super.viewDidLayoutSubviews()
        slideViewController.view.bounds.size = view.bounds.size
    }

    private func maybeShowReconnectView() {
        reconnectViewTimer?.invalidate()
        reconnectViewTimer = NSTimer.scheduledTimerWithTimeInterval(2, target: self, selector: "showReconnectView", userInfo: nil, repeats: false)
    }

    @objc private func showReconnectView() {
        if reconnectView == nil {
            reconnectView = ReconnectView(frame: CGRect(x: 0, y: 0, width: view.bounds.size.width, height: 50))
        }

        view.insertSubview(reconnectView, atIndex: 0)

        UIView.animateWithDuration(0.25) {
            self.slideViewController.view.frame.origin.y = 50
        }
    }

    private func hideReconnectView() {
        reconnectViewTimer?.invalidate()
        UIView.animateWithDuration(0.25) {
            self.slideViewController.view.frame.origin.y = 0
        }
    }

    @objc private func chatClientConnecting() {
        maybeShowReconnectView()
    }

    @objc private func chatClientDidConnect() {
        hideReconnectView()
    }

    @objc private func chatClientDidDisconnect() {
        maybeShowReconnectView()
    }
}

extension MainViewController: ChatListViewControllerDelegate {
    func chatListViewController(chatListViewController: ChatListViewController, didSelectChat chat: Chat) {
        let userDefaults = NSUserDefaults.sharedUserDefaults()
        userDefaults.setURL(chat.id, forKey: currentChatKey)
        userDefaults.synchronize()

        let chatViewController = slideViewController.contentViewController as! ChatViewController
        chatViewController.chat = chat

        slideViewController.hideMenu(true)
    }
}