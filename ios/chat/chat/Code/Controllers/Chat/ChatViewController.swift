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
import MobileCoreServices
import UIKit

class ChatViewController: UIViewController {
    enum RowType {
        case ConversationStart
        case Message(AnyObject, Int)
        case Date(NSDate)
        case NewMessages
    }

    @IBOutlet weak var topBar: UIView!
    @IBOutlet weak var friendNameLabel: UILabel!

    @IBOutlet weak var tableView: UITableView!

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

                isAtTop = false
                rows = []
                messages = []
                unreadMessageCount = MessageManager.sharedManager.unreadMessageCountForChat(chat)

                loadMessages()
                MessageManager.sharedManager.markMessagesForChatAsRead(chat)
            }
        }
    }

    private let chatRowCellReuseIdentifier = "ChatRowTableViewCell"
    private let mediaRowCellReuseIdentifier = "MediaRowTableViewCell"
    private let dayCellReuseIdentifier = "DayTableViewCell"
    private let newMessagesCellReuseIdentifier = "NewMessagesTableViewCell"
    private let conversationStartCellReuseIdentifier = "ConversationStartTableViewCell"

    private let fetchLimit = 30
    private let keyboardNotifications = KeyboardNotifications()

    private var isAtTop = false
    private var rows: [RowType] = []
    private var messages: [Message] = []
    private var unreadMessageCount = 0

    private var imagePickerController: UIImagePickerController?

    deinit {
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

        topBar.backgroundColor = UIColor.currentTheme.lightBackgroundColor
        newMessageContainer.backgroundColor = UIColor.currentTheme.lightBackgroundColor
        newMessageView.layer.borderColor = UIColor.currentTheme.borderColor.CGColor
        newMessageView.layer.borderWidth = 1
        newMessageView.layer.cornerRadius = 8

        tableView.dataSource = self
        tableView.delegate = self
        tableView.rowHeight = UITableViewAutomaticDimension
        tableView.estimatedRowHeight = 100
        tableView.registerNib(UINib(nibName: "ChatRowTableViewCell", bundle: nil), forCellReuseIdentifier: chatRowCellReuseIdentifier)
        tableView.registerNib(UINib(nibName: "NewMessagesCell", bundle: nil), forCellReuseIdentifier: newMessagesCellReuseIdentifier)
        tableView.registerNib(UINib(nibName: "MediaRowTableViewCell", bundle: nil), forCellReuseIdentifier: mediaRowCellReuseIdentifier)
        tableView.registerNib(UINib(nibName: "DayTableViewCell", bundle: nil), forCellReuseIdentifier: dayCellReuseIdentifier)
        tableView.registerNib(UINib(nibName: "NewMessagesTableViewCell", bundle: nil), forCellReuseIdentifier: newMessagesCellReuseIdentifier)
        tableView.registerNib(UINib(nibName: "ConversationStartTableViewCell", bundle: nil), forCellReuseIdentifier: conversationStartCellReuseIdentifier)

        tableView.contentInset = UIEdgeInsets(top: 0, left: 0, bottom: 16, right: 0)

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

        NSNotificationCenter.defaultCenter().addObserver(self, selector: "appDidBecomeActive", name: UIApplicationDidBecomeActiveNotification, object: UIApplication.sharedApplication())
        NSNotificationCenter.defaultCenter().addObserver(self, selector: "appDidEnterBackground", name: UIApplicationDidEnterBackgroundNotification, object: UIApplication.sharedApplication())
        NSNotificationCenter.defaultCenter().addObserver(self, selector: "unreadMessageCountUpdated:", name: MessageManager.TotalUnreadMessageCountUpdated, object: chat?.participantsArray[0])

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

        if let chat = chat {
            MessageManager.sharedManager.sendMessageWithText(text, toChat: chat) {
                message in
                if let message = message {
                    self.appendMessage(message)
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
                    self.appendMessage(message)
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
                    self.appendMessage(message)
                }
            }
        }
    }

    private func resetNewMessageView() {
        newMessageView.text = ""
        sizeTextView()
    }

    private func appendMessage(message: Message) {
        appendMessages([message])
    }

    private func appendMessages(newMessages: [Message]) {
        messages = messages + newMessages

        if unreadMessageCount > 0 {
            unreadMessageCount += newMessages.count
        }

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
        guard let chat = chat else {
            return
        }

        let isFirstBatch = messages.count == 0
        let newMessages = MessageManager.sharedManager.getMessagesForChat(chat, beforeDate: messages.count == 0 ? nil : messages[0].date, fetchLimit: fetchLimit)
        isAtTop = newMessages.count < fetchLimit
        messages = newMessages + messages

        let previousRowCount = rows.count
        calculateRows()

        if isFirstBatch || newMessages.count > 0 {
            tableView.reloadData()
            tableView.layoutIfNeeded()

            if rows.count > 0 {
                tableView.scrollToRowAtIndexPath(NSIndexPath(forRow: (rows.count - previousRowCount) - (isFirstBatch ? 1 : 0), inSection: 0), atScrollPosition: isFirstBatch ? .Bottom : .Top, animated: false)
            }
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

        if isAtTop {
            rows.insert(.ConversationStart, atIndex: 0)
        }

        if unreadMessageCount > 0 {
            rows.insert(.NewMessages, atIndex: rows.count - unreadMessageCount)
        }
    }

    private func headerTypeForRowAtIndexPath(indexPath: NSIndexPath) -> MessageTableViewCell.HeaderType {
        if indexPath.row == 0 {
            return .Full
        }

        var previousRowIsMedia = false

        switch(rows[indexPath.row - 1]) {
            case .ConversationStart:
                return .FullNoPadding
            case .Date(_):
                return .FullNoPadding
            case .NewMessages:
                return .FullNoPadding
            case .Message(let object, _):
                let message = object as! Message
                previousRowIsMedia = message.thumbnailInfo != nil
        }

        let row = rows[indexPath.row]
        switch(row) {
            case .Message(let object, let messageIndex):
                let message = object as! Message
                if message.thumbnailInfo != nil && previousRowIsMedia {
                    return .NoPadding
                } else if let previousMessage: Message = messageIndex > 0 ? messages[messageIndex - 1] : nil {
                    return (messageIndex == 0 || abs(messageIndex - messages.count) % fetchLimit == 0 || message.date.timeIntervalSinceDate(previousMessage.date) > 600 || message.from != previousMessage.from) ? .Full : .PaddingOnly
                } else {
                    return messageIndex == 0 ? .Full : .PaddingOnly
                }
            default:
                return .PaddingOnly
        }
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

    @objc private func didReceiveMessagesNotification(notification: NSNotification) {
        if let messages = (notification.userInfo?["messages"] as? MessageManager.NewMessagesNotificationWrapper)?.messages {
            if let chat = chat {
                MessageManager.sharedManager.markMessagesForChatAsRead(chat)
            }

            appendMessages(messages)
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
        case .ConversationStart:
            let cell = tableView.dequeueReusableCellWithIdentifier(conversationStartCellReuseIdentifier, forIndexPath: indexPath) as! ConversationStartTableViewCell
            cell.friend = chat?.participantsArray[0]
            return cell
        case .Message(let object, _):
            let message = object as! Message

            var cell: MessageTableViewCell
            if message.type == .Text {
                cell = tableView.dequeueReusableCellWithIdentifier(chatRowCellReuseIdentifier, forIndexPath: indexPath) as! MessageTableViewCell
            } else {
                cell = tableView.dequeueReusableCellWithIdentifier(mediaRowCellReuseIdentifier, forIndexPath: indexPath) as! MediaRowTableViewCell
            }

            cell.message = message
            cell.headerType = headerTypeForRowAtIndexPath(indexPath)

            return cell
        case .Date(let date):
            let cell = tableView.dequeueReusableCellWithIdentifier(dayCellReuseIdentifier, forIndexPath: indexPath) as! DayTableViewCell
            cell.date = date
            return cell
        case .NewMessages:
            return tableView.dequeueReusableCellWithIdentifier(newMessagesCellReuseIdentifier, forIndexPath: indexPath)
        }
    }

    func tableView(tableView: UITableView, estimatedHeightForRowAtIndexPath indexPath: NSIndexPath) -> CGFloat {
        switch(rows[indexPath.row]) {
        case .ConversationStart:
            return ConversationStartTableViewCell.rowHeight
        case .Message(let object, _):
            let message = object as! Message
            if message.type == .Text {
                return ChatRowTableViewCell.estimatedHeightForMessage(message, headerType: headerTypeForRowAtIndexPath(indexPath))

            } else {
                return MediaRowTableViewCell.estimatedHeightForMessage(message, headerType: headerTypeForRowAtIndexPath(indexPath))
            }
        case .Date(_):
            return DayTableViewCell.estimatedRowHeight
        case .NewMessages:
            return NewMessagesTableViewCell.rowHeight
        }
    }

    func tableView(tableView: UITableView, heightForRowAtIndexPath indexPath: NSIndexPath) -> CGFloat {
        switch(rows[indexPath.row]) {
        case .ConversationStart:
            return ConversationStartTableViewCell.rowHeight
        case .NewMessages:
            return NewMessagesTableViewCell.rowHeight
        default:
            return UITableViewAutomaticDimension
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