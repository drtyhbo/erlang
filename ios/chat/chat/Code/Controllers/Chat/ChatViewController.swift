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
    enum RowType {
        case Message(AnyObject, Int)
        case Date(NSDate)
    }

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

                rows = []
                messages = []

                loadMessages()
                MessageManager.sharedManager.markMessagesForFriendAsRead(friend)
            }
        }
    }

    private let chatRowTableViewCellReuseIdentifier = "ChatRowTableViewCell"
    private let chatRowContinuationTableViewCellReuseIdentifier = "ChatRowContinuationTableViewCell"
    private let newMessagesCellReuseIdentifier = "NewMessagesCellReuseIdentifier"
    private let imageRowTableViewCell = "ImageRowTableViewCell"
    private let dayTableViewCellReuseIdentifier = "DayTableViewCell"

    private let fetchLimit = 30
    private let messageHelperHeight: CGFloat = 100

    private var rows: [RowType] = []
    private var messages: [Message] = []
    private var lastMessageDate: NSDate?

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
        tableView.registerNib(UINib(nibName: "DayTableViewCell", bundle: nil), forCellReuseIdentifier: dayTableViewCellReuseIdentifier)
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
        messages.append(message)

        let previousRowCount = rows.count
        calculateRows()

        UIView.performWithoutAnimation {
            let newRowCount = self.rows.count - previousRowCount

            var indexPaths: [NSIndexPath] = []
            for i in 0..<newRowCount {
                indexPaths.append(NSIndexPath(forRow: self.rows.count - 1 - i, inSection: 0))
            }

            self.tableView.insertRowsAtIndexPaths(indexPaths, withRowAnimation: .None)
            self.tableView.scrollToRowAtIndexPath(NSIndexPath(forRow: self.rows.count - 1, inSection: 0), atScrollPosition: .Bottom, animated: false)
        }
    }

    private func loadMessages() {
        guard let friend = friend else {
            return
        }

        let firstBatch = messages.count == 0
        let newMessages = MessageManager.sharedManager.getMessagesForFriend(friend, beforeDate: messages.count == 0 ? nil : messages[0].date, fetchLimit: fetchLimit)
        if newMessages.count > 0 {
            messages = newMessages + messages

            let previousRowCount = rows.count
            calculateRows()

            tableView.reloadData()
            tableView.layoutIfNeeded()
            tableView.scrollToRowAtIndexPath(NSIndexPath(forRow: (rows.count - previousRowCount) - (firstBatch ? 1 : 0), inSection: 0), atScrollPosition: firstBatch ? .Bottom : .Top, animated: false)
        }
    }

    private func calculateRows() {
        rows = []
        if messages.count == 0 {
            return
        }

        guard let gregorian = NSCalendar(calendarIdentifier: NSCalendarIdentifierGregorian) else {
            return
        }

        var previousDayOfYear = gregorian.ordinalityOfUnit(.Day, inUnit: .Year, forDate: messages[0].date)
        rows = [.Message(messages[0], 0)]
        for i in 1..<messages.count {
            let message = messages[i]

            let dayOfYear = gregorian.ordinalityOfUnit(.Day, inUnit: .Year, forDate: message.date)
            if dayOfYear != previousDayOfYear {
                rows.append(.Date(message.date))
            }
            previousDayOfYear = dayOfYear

            rows.append(.Message(message, i))
        }
    }

    private func headerTypeForRowAtIndexPath(indexPath: NSIndexPath) -> MessageTableViewCell.HeaderType {
        if indexPath.row == 0 {
            return .Full
        }

        switch(rows[indexPath.row - 1]) {
            case .Date(_):
                return .FullNoPadding
            default:
                break
        }

        let row = rows[indexPath.row]
        switch(row) {
            case .Message(let object, let messageIndex):
                let message = object as! Message
                if let previousMessage: Message = messageIndex > 0 ? messages[messageIndex - 1] : nil {
                    return (messageIndex == 0 || abs(messageIndex - messages.count) % fetchLimit == 0 || message.date.timeIntervalSinceDate(previousMessage.date) > 600 || message.from != previousMessage.from) ? .Full : .PaddingOnly
                } else {
                    return messageIndex == 0 ? .Full : .PaddingOnly
                }
            default:
                return .PaddingOnly
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
        return rows.count
    }

    func tableView(tableView: UITableView, cellForRowAtIndexPath indexPath: NSIndexPath) -> UITableViewCell {
        let row = rows[indexPath.row]

        switch(row) {
        case .Message(let object, _):
            let message = object as! Message

            var cell: MessageTableViewCell
            if message.imageInfo != nil {
                cell = tableView.dequeueReusableCellWithIdentifier(imageRowTableViewCell, forIndexPath: indexPath) as! ImageRowTableViewCell
            } else {
                cell = tableView.dequeueReusableCellWithIdentifier(chatRowTableViewCellReuseIdentifier, forIndexPath: indexPath) as! MessageTableViewCell
            }

            cell.message = message
            cell.headerType = headerTypeForRowAtIndexPath(indexPath)

            return cell
        case .Date(let date):
            let cell = tableView.dequeueReusableCellWithIdentifier(dayTableViewCellReuseIdentifier) as! DayTableViewCell
            cell.date = date
            return cell
        }
    }

    func tableView(tableView: UITableView, estimatedHeightForRowAtIndexPath indexPath: NSIndexPath) -> CGFloat {
        switch(rows[indexPath.row]) {
        case .Message(let object, _):
            let message = object as! Message
            if message.imageInfo != nil {
                return ImageRowTableViewCell.estimatedHeightForMessage(message, headerType: headerTypeForRowAtIndexPath(indexPath))
            } else {
                let height = ChatRowTableViewCell.estimatedHeightForMessage(message, headerType: headerTypeForRowAtIndexPath(indexPath))
                return height
            }
        case .Date(_):
            return DayTableViewCell.estimatedRowHeight
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
            loadMessages()
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