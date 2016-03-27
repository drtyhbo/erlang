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
}

extension MainViewController: ChatListViewControllerDelegate {
    func chatListViewController(chatListViewController: ChatListViewController, didSelectChat chat: Chat) {
        NSUserDefaults.standardUserDefaults().setURL(chat.id, forKey: currentChatKey)
        NSUserDefaults.standardUserDefaults().synchronize()

        let chatViewController = slideViewController.contentViewController as! ChatViewController
        chatViewController.chat = chat

        slideViewController.hideMenu(true)
    }
}