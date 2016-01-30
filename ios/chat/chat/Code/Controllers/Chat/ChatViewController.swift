//
//  ChatViewController.swift
//  chat
//
//  Created by Andreas Binnewies on 1/13/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import CocoaAsyncSocket
import UIKit

class ChatViewController: UIViewController {
    @IBOutlet weak var tableView: UITableView!

    @IBOutlet weak var newMessageContainerBottomConstraint: NSLayoutConstraint!

    @IBOutlet weak var messageLabel: UILabel!

    @IBOutlet weak var newMessageView: UITextView!
    @IBOutlet weak var newMessageViewHeightConstraint: NSLayoutConstraint!

    @IBOutlet weak var messageHelperContainer: UIView!
    @IBOutlet weak var messageHelperContainerHeightConstraint: NSLayoutConstraint!

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

        ChatClient.sharedClient.connect()
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

        NSNotificationCenter.defaultCenter().addObserver(self, selector: "keyboardWillShow:", name: UIKeyboardWillShowNotification, object: nil)
        NSNotificationCenter.defaultCenter().addObserver(self, selector: "keyboardWillHide:", name: UIKeyboardWillHideNotification, object: nil)

        navigationItem.title = "Chat"

        NSNotificationCenter.defaultCenter().addObserver(self, selector: "appDidBecomeActive", name: UIApplicationDidBecomeActiveNotification, object: UIApplication.sharedApplication())
        NSNotificationCenter.defaultCenter().addObserver(self, selector: "appDidEnterBackground", name: UIApplicationDidEnterBackgroundNotification, object: UIApplication.sharedApplication())

    }

    @objc private func appDidBecomeActive() {

    }

    @objc private func appDidEnterBackground() {
        newMessageView.resignFirstResponder()
    }

    private func sizeTextView() {
        newMessageViewHeightConstraint.constant = newMessageView.contentSize.height

        newMessageView.contentOffset.y = 0
        newMessageView.layoutIfNeeded()
    }

    private func sendMessageWithText(text: String) {
        resetNewMessageView()
/*        Message.sendMessage(text) {
            message in
            if let message = message {
                self.appendMessage(message)
            }
        }*/
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

        newMessageContainerBottomConstraint.constant = keyboardFrame.size.height
        UIView.animateWithDuration(0.1, animations: { () -> Void in
            self.view.layoutIfNeeded()
        })

        self.tableView.contentOffset.y = self.tableView.contentOffset.y + keyboardFrame.size.height
    }

    @objc private func keyboardWillHide(notification: NSNotification) {
        newMessageContainerBottomConstraint.constant = 0
        UIView.animateWithDuration(0.1, animations: { () -> Void in
            self.view.layoutIfNeeded()
        })
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
        if newMessagesRow != nil && indexPath.row == newMessagesRow! {
            return tableView.dequeueReusableCellWithIdentifier(newMessagesCellReuseIdentifier)!
        } else {
            let messageIndex = (newMessagesRow != nil && indexPath.row > newMessagesRow!) ? indexPath.row - 1 : indexPath.row
            var cellReuseIdentifier = chatRowTableViewCellReuseIdentifier
/*            if messageIndex != 0 && messages[messageIndex].author.objectId == messages[messageIndex - 1].author.objectId {
                cellReuseIdentifier = chatRowContinuationTableViewCellReuseIdentifier
            }*/

            let cell = tableView.dequeueReusableCellWithIdentifier(cellReuseIdentifier, forIndexPath: indexPath) as! MessageTableViewCell
            cell.message = messages[messageIndex]
            return cell
        }
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