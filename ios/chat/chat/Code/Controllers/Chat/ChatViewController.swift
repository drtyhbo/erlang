//
//  ChatViewController.swift
//  chat
//
//  Created by Andreas Binnewies on 1/13/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

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

    @IBOutlet weak var messageHelperContainer: UIView!
    @IBOutlet weak var messageHelperContainerHeightConstraint: NSLayoutConstraint!

    var friend: Friend? {
        didSet {
            if let friend = friend {
                friendNameLabel.text = friend.name

                NSNotificationCenter.defaultCenter().removeObserver(self, name: MessageManager.NewMessageNotification, object: oldValue)
                NSNotificationCenter.defaultCenter().addObserver(self, selector: "didReceiveMessage:", name: MessageManager.NewMessageNotification, object: friend)

                messages = MessageManager.sharedManager.getMessagesForFriend(friend)
                MessageManager.sharedManager.markMessagesForFriendAsRead(friend)

                tableView.reloadData()
            }
        }
    }

    private let chatRowTableViewCellReuseIdentifier = "ChatRowTableViewCell"
    private let chatRowContinuationTableViewCellReuseIdentifier = "ChatRowContinuationTableViewCell"
    private let newMessagesCellReuseIdentifier = "NewMessagesCellReuseIdentifier"

    private let messageHelperHeight: CGFloat = 100

    private var messages: [Message] = []
    private var lastMessageDate: NSDate?

    private var messageHelper: MessageHelper!
    private var newMessagesRow: Int?

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

        tableView.addGestureRecognizer(UITapGestureRecognizer(target: self, action: "didTapOnMessages"))

        NSNotificationCenter.defaultCenter().addObserver(self, selector: "keyboardWillShow:", name: UIKeyboardWillShowNotification, object: nil)
        NSNotificationCenter.defaultCenter().addObserver(self, selector: "keyboardWillHide:", name: UIKeyboardWillHideNotification, object: nil)

        navigationItem.title = "Chat"

        NSNotificationCenter.defaultCenter().addObserver(self, selector: "appDidBecomeActive", name: UIApplicationDidBecomeActiveNotification, object: UIApplication.sharedApplication())
        NSNotificationCenter.defaultCenter().addObserver(self, selector: "appDidEnterBackground", name: UIApplicationDidEnterBackgroundNotification, object: UIApplication.sharedApplication())

        registerForNotifications()
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
            MessageManager.sharedManager.sendMessageWithText(text, to: friend)
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
/*        Message.sendImageFile(PFFile(data: UIImageJPEGRepresentation(image, 0.8)!)!, width: Int(image.size.width), height: Int(image.size.height)) {
            message in
            if let message = message {
                self.appendMessage(message)
            }
        }*/
    }

    private func resetNewMessageView() {
        clearMessageHelper()
        newMessageView.text = ""
        sizeTextView()
    }

    private func setMessageHelper(messageHelper: MessageHelper, withTitle title: String) {
        newMessageView.text = ""

        self.messageHelper = messageHelper

        messageHelper.frame = CGRect(x: 0, y: 0, width: view.bounds.size.width, height: messageHelperHeight)
        messageHelperContainer.addSubview(messageHelper)
        messageHelperContainerHeightConstraint.constant = messageHelperHeight
        UIView.animateWithDuration(0.25) {
            self.view.layoutIfNeeded()
        }

        messageLabel.text = "\(title.uppercaseString):"
    }

    private func clearMessageHelper() {
        if messageHelper == nil {
            return
        }

        messageHelper.removeFromSuperview()
        messageHelper = nil

        messageHelperContainerHeightConstraint.constant = 0
        UIView.animateWithDuration(0.25) {
            self.view.layoutIfNeeded()
        }

        messageLabel.text = ""
    }

    private func appendMessage(message: Message) {
        self.messages.append(message)
        CATransaction.begin()
        CATransaction.setCompletionBlock {
            self.tableView.scrollToRowAtIndexPath(NSIndexPath(forRow: self.messages.count - 1, inSection: 0), atScrollPosition: .Bottom, animated: false)
        }

        self.tableView.beginUpdates()
        self.tableView.insertRowsAtIndexPaths([NSIndexPath(forRow: self.messages.count - 1, inSection: 0)], withRowAnimation: .None)
        self.tableView.endUpdates()

        CATransaction.commit()
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
        let cell = tableView.dequeueReusableCellWithIdentifier(chatRowTableViewCellReuseIdentifier, forIndexPath: indexPath) as! MessageTableViewCell
        cell.message = messages[indexPath.row]
        return cell
    }
}

extension ChatViewController: UITableViewDelegate {
    func tableView(tableView: UITableView, didSelectRowAtIndexPath indexPath: NSIndexPath) {
        tableView.deselectRowAtIndexPath(indexPath, animated: false)
        newMessageView.resignFirstResponder()
    }
}

extension ChatViewController: UITextViewDelegate {
    func textViewDidChange(textView: UITextView) {
        if textView.text.lowercaseString.hasPrefix("gif") && messageHelper == nil {
            let gifSelector = GIFSelector()
            gifSelector.delegate = self
            setMessageHelper(gifSelector, withTitle: "gif")
        } else if messageHelper != nil {
            messageHelper.searchQuery = textView.text
        }
        sizeTextView()
    }

    func textView(textView: UITextView, shouldChangeTextInRange range: NSRange, replacementText text: String) -> Bool {
        if text == "" && range.location == 0 && range.length == 0 && messageHelper != nil {
            clearMessageHelper()
            return false
        }
        return true
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
        }
    }
}