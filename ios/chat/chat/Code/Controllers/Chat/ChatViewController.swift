//
//  ChatViewController.swift
//  chat
//
//  Created by Andreas Binnewies on 1/13/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import APLSlideMenu
import ChatCommon
import CocoaAsyncSocket
import CoreData
import MobileCoreServices
import UIKit

class ChatViewController: UIViewController {
    @IBOutlet weak var topBar: UIView!
    @IBOutlet weak var friendNameLabel: UILabel!

    @IBOutlet weak var tableView: ChatTableView!

    @IBOutlet weak var newMessageContainerBottomConstraint: NSLayoutConstraint!

    @IBOutlet weak var newMessageContainer: UIView!
    @IBOutlet weak var newMessageView: UITextView!
    @IBOutlet weak var newMessageViewHeightConstraint: NSLayoutConstraint!

    @IBOutlet weak var unreadMessagesContainer: UIView!
    @IBOutlet weak var unreadMessagesCount: UILabel!

    var chat: Chat? {
        didSet {
            if let chat = chat {
                friendNameLabel.text = chat.name

                NSNotificationCenter.defaultCenter().removeObserver(self, name: MessageManager.NewMessagesNotification, object: oldValue)
                NSNotificationCenter.defaultCenter().addObserver(self, selector: "didReceiveMessagesNotification:", name: MessageManager.NewMessagesNotification, object: chat)

                unreadMessageCount = MessageManager.sharedManager.unreadMessageCountForChat(chat)

                tableView.setupWithChat(chat, layout: ChatLayout.currentLayout())
                MessageManager.sharedManager.markMessagesForChatAsRead(chat)
            }
        }
    }

    private let keyboardNotifications = KeyboardNotifications()
    private let themeListener = ThemeListener()

    private var unreadMessageCount = 0

    private var imagePickerController: UIImagePickerController?

    deinit {
        NSNotificationCenter.defaultCenter().removeObserver(self)
        keyboardNotifications.removeNotifications()
    }

    init() {
        super.init(nibName: "ChatViewController", bundle: nil)

        ChatClient.sharedClient.maybeConnect()
    }

    required init?(coder aDecoder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    override func viewDidLoad() {
        super.viewDidLoad()

        updateTheme(ColorTheme.currentTheme)
        themeListener.themeChangeListener = { [weak self] theme in
            self?.updateTheme(theme)
        }

        tableView.chatTableViewDelegate = self
        tableView.addGestureRecognizer(UITapGestureRecognizer(target: self, action: "didTapOnMessages"))

        unreadMessagesContainer.layer.cornerRadius = unreadMessagesContainer.bounds.size.height / 2

        keyboardNotifications.addNotificationsForWillShow({
                size in
                self.keyboardWillShowWithSize(size)
            }, willHide: {
                size in
                self.keyboardWillHideWithSize(size)
            });

        navigationItem.title = "Chat"

        NSNotificationCenter.defaultCenter().addObserver(self, selector: "appDidEnterBackground", name: UIApplicationDidEnterBackgroundNotification, object: UIApplication.sharedApplication())
        NSNotificationCenter.defaultCenter().addObserver(self, selector: "unreadMessageCountUpdated:", name: MessageManager.TotalUnreadMessageCountUpdated, object: chat?.participantsArray[0])
        NSNotificationCenter.defaultCenter().addObserver(self, selector: "layoutChangedNotification", name: ChatLayout.LayoutChangedNotification, object: nil)

        registerForNotifications()
    }

    private func reloadDataWithCompletion(completion: Void->Void) {
        tableView.reloadData()

        dispatch_async(dispatch_get_main_queue()) {
            self.tableView.layoutIfNeeded()
            completion()
        }
    }

    @objc private func appDidEnterBackground() {
        newMessageView.resignFirstResponder()
    }

    private func registerForNotifications() {
        let settings = UIUserNotificationSettings(forTypes: [.Alert, .Badge, .Sound], categories: nil)
        UIApplication.sharedApplication().registerUserNotificationSettings(settings)
        UIApplication.sharedApplication().registerForRemoteNotifications()
    }

    private func sizeTextView() {
        let oldMessageViewHeight = newMessageViewHeightConstraint.constant
        newMessageViewHeightConstraint.constant = newMessageView.contentSize.height
        tableView.contentOffset.y += newMessageViewHeightConstraint.constant - oldMessageViewHeight

        newMessageView.contentOffset.y = 0
        newMessageView.layoutIfNeeded()
    }

    private func sendMessageWithText(text: String) {
        resetNewMessageView()

        if let chat = chat {
            MessageManager.sharedManager.sendMessageWithText(text, toChat: chat) {
                message in
                if let message = message {
                    self.tableView.appendMessages([message])
                }
            }
        }
    }

    private func sendMessageWithImage(image: UIImage) {
        resetNewMessageView()

        if let chat = chat {
            MessageManager.sharedManager.sendMessageWithImage(image, toChat: chat) {
                message in
                if let message = message {
                    self.tableView.appendMessages([message])
                }
            }
        }
    }

    private func sendMessageWithMediaUrl(mediaUrl: NSURL) {
        resetNewMessageView()

        if let chat = chat {
            MessageManager.sharedManager.sendMessageWithMediaUrl(mediaUrl, toChat: chat) {
                message in
                if let message = message {
                    self.tableView.appendMessages([message])
                }
            }
        }
    }

    private func resetNewMessageView() {
        newMessageView.text = ""
        sizeTextView()
    }

    private func keyboardWillShowWithSize(keyboardSize: CGSize) {
        self.tableView.contentOffset.y += keyboardSize.height

        newMessageContainerBottomConstraint.constant = keyboardSize.height
        UIView.animateWithDuration(0.1, animations: { () -> Void in
            self.view.layoutIfNeeded()
        })
    }

    private func keyboardWillHideWithSize(keyboardSize: CGSize) {
        self.tableView.contentOffset.y -= keyboardSize.height

        newMessageContainerBottomConstraint.constant = 0
        UIView.animateWithDuration(0.1, animations: { () -> Void in
            self.view.layoutIfNeeded()
        })
    }

    private func updateTheme(theme: ColorTheme) {
        newMessageView.layer.borderColor = theme.borderColor.CGColor
        newMessageView.layer.borderWidth = 1
        newMessageView.layer.cornerRadius = 8
    }

    @objc private func didReceiveMessagesNotification(notification: NSNotification) {
        if let messages = (notification.userInfo?["messages"] as? MessageManager.NewMessagesNotificationWrapper)?.messages {
            if let chat = chat {
                MessageManager.sharedManager.markMessagesForChatAsRead(chat)
            }

            self.tableView.appendMessages(messages)
        }
    }

    @objc private func unreadMessageCountUpdated(notification: NSNotification) {
        if let totalUnreadMessageCount = notification.userInfo?["unreadMessageCount"] as? Int {
            unreadMessagesContainer.hidden = totalUnreadMessageCount == 0
            unreadMessagesCount.text = "\(totalUnreadMessageCount)"
        }
    }

    @objc private func layoutChangedNotification() {
        guard let chat = chat else {
            return
        }

        tableView.setupWithChat(chat, layout: ChatLayout.currentLayout())
    }

    @objc private func didTapOnMessages() {
        newMessageView.resignFirstResponder()
    }

    @IBAction func didTapSend() {
        sendMessageWithText(newMessageView.text)
    }

    @IBAction func didTapCameraButton() {
        let imagePickerController = UIImagePickerController()
        imagePickerController.delegate = self
        imagePickerController.sourceType = .PhotoLibrary
        imagePickerController.mediaTypes = [kUTTypeImage as String, kUTTypeMovie as String]
        presentViewController(imagePickerController, animated: true, completion: nil)

        self.imagePickerController = imagePickerController
    }

    @IBAction func didTapMenu() {
        if let slideMenuViewController = parentViewController as? APLSlideMenuViewController {
            slideMenuViewController.showLeftMenu(true)
        }
    }
}

extension ChatViewController: UITextViewDelegate {
    func textViewDidChange(textView: UITextView) {
        sizeTextView()
    }
}

extension ChatViewController: UIImagePickerControllerDelegate, UINavigationControllerDelegate {
    func imagePickerController(picker: UIImagePickerController, didFinishPickingMediaWithInfo info: [String : AnyObject]) {
        if let image = info[UIImagePickerControllerOriginalImage] as? UIImage {
            sendMessageWithImage(image)
        } else if let mediaUrl = info[UIImagePickerControllerMediaURL] as? NSURL {
            sendMessageWithMediaUrl(mediaUrl)
        }

        dismissViewControllerAnimated(true, completion: nil)
        self.imagePickerController = nil
    }
}

extension ChatViewController: ChatTableViewDelegate {
    func chatTableViewDelegateDidSelectRow(chatTableView: ChatTableView) {
        newMessageView.resignFirstResponder()
    }
}