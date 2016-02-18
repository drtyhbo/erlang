//
//  ChatViewController.swift
//  chat
//
//  Created by Andreas Binnewies on 1/13/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import APLSlideMenu
import CocoaAsyncSocket
import CoreData
import UIKit

class ChatViewController: UIViewController {
    @IBOutlet weak var friendNameLabel: UILabel!

    @IBOutlet weak var tableView: UITableView!

    @IBOutlet weak var newMessageContainerBottomConstraint: NSLayoutConstraint!

    @IBOutlet weak var messageLabel: UILabel!

    @IBOutlet weak var newMessageView: UITextView!
    @IBOutlet weak var newMessageViewHeightConstraint: NSLayoutConstraint!

    @IBOutlet weak var unreadMessagesContainer: UIView!
    @IBOutlet weak var unreadMessagesCount: UILabel!

    var friend: Friend? {
        didSet {
            if let friend = friend {
                friendNameLabel.text = friend.name

                NSNotificationCenter.defaultCenter().removeObserver(self, name: MessageManager.NewMessageNotification, object: oldValue)
                NSNotificationCenter.defaultCenter().addObserver(self, selector: "didReceiveMessage:", name: MessageManager.NewMessageNotification, object: friend)

                messages = MessageManager.sharedManager.getMessagesForFriend(friend, beforeDate: nil, fetchLimit: fetchLimit)
                MessageManager.sharedManager.markMessagesForFriendAsRead(friend)

                tableView.reloadData()
                tableView.layoutIfNeeded()
                tableView.scrollToRowAtIndexPath(NSIndexPath(forRow: messages.count - 1, inSection: 0), atScrollPosition: .Bottom, animated: false)
            }
        }
    }

    private let chatRowTableViewCellReuseIdentifier = "ChatRowTableViewCell"
    private let chatRowContinuationTableViewCellReuseIdentifier = "ChatRowContinuationTableViewCell"
    private let newMessagesCellReuseIdentifier = "NewMessagesCellReuseIdentifier"
    private let imageRowTableViewCell = "ImageRowTableViewCell"

    private let fetchLimit = 15
    private let messageHelperHeight: CGFloat = 100

    private var messages: [Message] = []
    private var lastMessageDate: NSDate?
    private var isFetchingMessages = false

    private var messageHelper: MessageHelper!
    private var newMessagesRow: Int?

    private var imagePickerController: UIImagePickerController?

    init() {
        super.init(nibName: "ChatViewController", bundle: nil)

        ChatClient.sharedClient.maybeConnect()
    }

    required init?(coder aDecoder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    override func viewDidLoad() {
        super.viewDidLoad()

        tableView.dataSource = self
        tableView.delegate = self
        tableView.rowHeight = UITableViewAutomaticDimension
        tableView.estimatedRowHeight = 100
        tableView.registerNib(UINib(nibName: "ChatRowTableViewCell", bundle: nil), forCellReuseIdentifier: chatRowTableViewCellReuseIdentifier)
        tableView.registerNib(UINib(nibName: "ChatRowContinuationTableViewCell", bundle: nil), forCellReuseIdentifier: chatRowContinuationTableViewCellReuseIdentifier)
        tableView.registerNib(UINib(nibName: "NewMessagesCell", bundle: nil), forCellReuseIdentifier: newMessagesCellReuseIdentifier)
        tableView.registerNib(UINib(nibName: "ImageRowTableViewCell", bundle: nil), forCellReuseIdentifier: imageRowTableViewCell)
        tableView.contentInset = UIEdgeInsets(top: -8, left: 0, bottom: 16, right: 0)

        tableView.addGestureRecognizer(UITapGestureRecognizer(target: self, action: "didTapOnMessages"))

        unreadMessagesContainer.layer.cornerRadius = unreadMessagesContainer.bounds.size.height / 2

        NSNotificationCenter.defaultCenter().addObserver(self, selector: "keyboardWillShow:", name: UIKeyboardWillShowNotification, object: nil)
        NSNotificationCenter.defaultCenter().addObserver(self, selector: "keyboardWillHide:", name: UIKeyboardWillHideNotification, object: nil)

        navigationItem.title = "Chat"

        NSNotificationCenter.defaultCenter().addObserver(self, selector: "appDidBecomeActive", name: UIApplicationDidBecomeActiveNotification, object: UIApplication.sharedApplication())
        NSNotificationCenter.defaultCenter().addObserver(self, selector: "appDidEnterBackground", name: UIApplicationDidEnterBackgroundNotification, object: UIApplication.sharedApplication())
        NSNotificationCenter.defaultCenter().addObserver(self, selector: "unreadMessageCountUpdated:", name: MessageManager.TotalUnreadMessageCountUpdated, object: friend)


        registerForNotifications()
    }

    private func reloadDataWithCompletion(completion: Void->Void) {
        tableView.reloadData()

        dispatch_async(dispatch_get_main_queue()) {
            self.tableView.layoutIfNeeded()
            completion()
        }
    }

    @objc private func appDidBecomeActive() {

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
        newMessageViewHeightConstraint.constant = newMessageView.contentSize.height

        newMessageView.contentOffset.y = 0
        newMessageView.layoutIfNeeded()
    }

    private func sendMessageWithText(text: String) {
        resetNewMessageView()

        if let friend = friend {
            MessageManager.sharedManager.sendMessageWithText(text, to: friend) {
                message in
                if let message = message {
                    self.appendMessage(message)
                }
            }
        }
    }

    private func sendMessageWithImageURL(imageURL: NSURL, width: Int, height: Int) {
        resetNewMessageView()
/*        Message.sendImageWithURL(imageURL, width: width, height: height) {
            message in
            if let message = message {
                self.appendMessage(message)
            }
        }*/
    }

    private func sendMessageWithImage(image: UIImage) {
        resetNewMessageView()

        if let friend = friend {
            MessageManager.sharedManager.sendMessageWithImage(image, to: friend) {
                message in
                if let message = message {
                    self.appendMessage(message)
                }
            }
        }
/*        Message.sendImageFile(PFFile(data: UIImageJPEGRepresentation(image, 0.8)!)!, width: Int(image.size.width), height: Int(image.size.height)) {
            message in
            if let message = message {
                self.appendMessage(message)
            }
        }*/
    }

    private func resetNewMessageView() {
        newMessageView.text = ""
        sizeTextView()
    }

    private func appendMessage(message: Message) {
        self.messages.append(message)

        UIView.performWithoutAnimation {
            self.tableView.insertRowsAtIndexPaths([NSIndexPath(forRow: self.messages.count - 1, inSection: 0)], withRowAnimation: .None)
            self.tableView.scrollToRowAtIndexPath(NSIndexPath(forRow: self.messages.count - 1, inSection: 0), atScrollPosition: .Bottom, animated: false)
        }
    }

    private func fetchMoreMessages() {
        guard let friend = friend else {
            return
        }

        if isFetchingMessages {
            return
        }
        isFetchingMessages = true

        let newMessages = MessageManager.sharedManager.getMessagesForFriend(friend, beforeDate: messages[0].date, fetchLimit: fetchLimit)
        if newMessages.count > 0 {
            messages = newMessages + messages
            tableView.reloadData()

            tableView.scrollToRowAtIndexPath(NSIndexPath(forRow: newMessages.count, inSection: 0), atScrollPosition: .Top, animated: false)
        }

        isFetchingMessages = false
    }

    private func doesRowAtIndexPathHaveHeader(indexPath: NSIndexPath) -> Bool {
        let message = messages[indexPath.row]
        if let previousMessage: Message = indexPath.row > 0 ? messages[indexPath.row - 1] : nil {
            return indexPath.row == 0 || abs(indexPath.row - messages.count) % fetchLimit == 0 || message.date.timeIntervalSinceDate(previousMessage.date) > 600 || message.from != previousMessage.from
        } else {
            return indexPath.row == 0
        }
    }

    @objc private func keyboardWillShow(notification: NSNotification) {
        let info = notification.userInfo!
        let keyboardFrame: CGRect = (info[UIKeyboardFrameEndUserInfoKey] as! NSValue).CGRectValue()

        let tableViewOverhang = tableView.contentSize.height - (tableView.bounds.size.height - keyboardFrame.size.height)
        if tableViewOverhang > 0 {
            self.tableView.contentOffset.y = max(0, self.tableView.contentOffset.y + tableViewOverhang)
        }

        newMessageContainerBottomConstraint.constant = keyboardFrame.size.height
        UIView.animateWithDuration(0.1, animations: { () -> Void in
            self.view.layoutIfNeeded()
        })
    }

    @objc private func keyboardWillHide(notification: NSNotification) {
        newMessageContainerBottomConstraint.constant = 0
        UIView.animateWithDuration(0.1, animations: { () -> Void in
            self.view.layoutIfNeeded()
        })
    }

    @objc private func didReceiveMessage(notification: NSNotification) {
        if let message = (notification.userInfo?["message"] as? MessageManager.NewMessageNotificationWrapper)?.message {
            appendMessage(message)

            if let friend = friend {
                MessageManager.sharedManager.markMessagesForFriendAsRead(friend)
            }
        }
    }

    @objc private func unreadMessageCountUpdated(notification: NSNotification) {
        if let totalUnreadMessageCount = notification.userInfo?["unreadMessageCount"] as? Int {
            unreadMessagesContainer.hidden = totalUnreadMessageCount == 0
            unreadMessagesCount.text = "\(totalUnreadMessageCount)"
        }
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
        presentViewController(imagePickerController, animated: true, completion: nil)

        self.imagePickerController = imagePickerController
    }

    @IBAction func didTapMenu() {
        if let slideMenuViewController = parentViewController as? APLSlideMenuViewController {
            slideMenuViewController.showLeftMenu(true)
        }
    }
}

extension ChatViewController: UITableViewDataSource {
    func numberOfSectionsInTableView(tableView: UITableView) -> Int {
        return 1
    }

    func tableView(tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return messages.count + (newMessagesRow != nil ? 1 : 0)
    }

    func tableView(tableView: UITableView, cellForRowAtIndexPath indexPath: NSIndexPath) -> UITableViewCell {
        let message = messages[indexPath.row]

        var cell: MessageTableViewCell
        if message.imageInfo != nil {
            cell = tableView.dequeueReusableCellWithIdentifier(imageRowTableViewCell, forIndexPath: indexPath) as! ImageRowTableViewCell
        } else {
            cell = tableView.dequeueReusableCellWithIdentifier(chatRowTableViewCellReuseIdentifier, forIndexPath: indexPath) as! MessageTableViewCell
        }

        cell.message = message
        cell.hasHeader = doesRowAtIndexPathHaveHeader(indexPath)

        return cell
    }

    func tableView(tableView: UITableView, estimatedHeightForRowAtIndexPath indexPath: NSIndexPath) -> CGFloat {
        let message = messages[indexPath.row]

        if message.imageInfo != nil {
            return ImageRowTableViewCell.estimatedHeightForMessage(message, hasHeader: doesRowAtIndexPathHaveHeader(indexPath))
        } else {
            let height = ChatRowTableViewCell.estimatedHeightForMessage(message, hasHeader: doesRowAtIndexPathHaveHeader(indexPath))
            return height
        }
    }
}

extension ChatViewController: UITableViewDelegate {
    func tableView(tableView: UITableView, didSelectRowAtIndexPath indexPath: NSIndexPath) {
        tableView.deselectRowAtIndexPath(indexPath, animated: false)
        newMessageView.resignFirstResponder()
    }
}

extension ChatViewController: UIScrollViewDelegate {
    func scrollViewDidEndDecelerating(scrollView: UIScrollView) {
        if scrollView.contentOffset.y < 200 {
            fetchMoreMessages()
        }
    }
}

extension ChatViewController: UITextViewDelegate {
    func textViewDidChange(textView: UITextView) {
        sizeTextView()
    }
}

extension ChatViewController: GIFSelectorDelegate {
    func gifSelector(gifSelector: GIFSelector, didSelectImageWithURL url: NSURL, width: Int, height: Int) {
        sendMessageWithImageURL(url, width: width, height: height)
    }
}

extension ChatViewController: UIImagePickerControllerDelegate, UINavigationControllerDelegate {
    func imagePickerController(picker: UIImagePickerController, didFinishPickingMediaWithInfo info: [String : AnyObject]) {
        if let image = info[UIImagePickerControllerOriginalImage] as? UIImage {
            sendMessageWithImage(image)
            dismissViewControllerAnimated(true, completion: nil)
            self.imagePickerController = nil
        }
    }
}