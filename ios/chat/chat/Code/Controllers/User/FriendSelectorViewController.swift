//
//  FriendSelectorViewController.swift
//  chat
//
//  Created by Andreas Binnewies on 3/6/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

protocol FriendSelectorViewControllerDelegate: class {
    func friendSelectorViewController(friendSelectorViewController: FriendSelectorViewController, didSelectFriends friends: [Friend])
}

class FriendSelectorViewController: UIViewController {
    @IBOutlet weak var searchTextField: UITextField!
    @IBOutlet weak var tableView: UITableView!

    @IBOutlet weak var nextContainer: UIView!
    @IBOutlet weak var nextContainerHeightConstraint: NSLayoutConstraint!
    @IBOutlet weak var nextContainerBottomConstraint: NSLayoutConstraint!
    @IBOutlet weak var nextLabel: UILabel!

    weak var delegate: FriendSelectorViewControllerDelegate?

    private let friendSelectorCellReuseIdentifier = "FriendSelectorTableViewCell"
    private let friends: [Friend]

    private let keyboardNotifications = KeyboardNotifications()

    private var filteredFriends: [Friend]?
    private var selectedFriends: [Friend] = []

    deinit {
        keyboardNotifications.removeNotifications()
    }

    init() {
        friends = FriendManager.sharedManager.friends

        super.init(nibName: "FriendSelectorViewController", bundle: nil)

        edgesForExtendedLayout = .None
        automaticallyAdjustsScrollViewInsets = true
    }

    required init?(coder aDecoder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    override func viewDidLoad() {
        super.viewDidLoad()

        tableView.backgroundColor = UIColor.whiteColor()

        tableView.registerNib(UINib(nibName: "FriendSelectorTableViewCell", bundle: nil), forCellReuseIdentifier: friendSelectorCellReuseIdentifier)

        navigationItem.title = "New Chat"
        navigationItem.rightBarButtonItem = UIBarButtonItem(image: UIImage(named: "Close")!, style: .Plain, target: self, action: "didTapClose")
        navigationItem.rightBarButtonItem?.tintColor = UIColor.currentTheme.buttonColor

        keyboardNotifications.addNotificationsForWillShow({
                size in
                self.keyboardWillShowWithSize(size)
            }, willHide: {
                size in
                self.keyboardWillHideWithSize(size)
            });

        searchTextField.addTarget(self, action: "searchQueryDidChange:", forControlEvents: .EditingChanged)
    }

    private func keyboardWillShowWithSize(keyboardSize: CGSize) {
        nextContainerBottomConstraint.constant = keyboardSize.height
        UIView.animateWithDuration(0.1, animations: { () -> Void in
            self.view.layoutIfNeeded()
        })
    }

    private func keyboardWillHideWithSize(keyboardSize: CGSize) {
        nextContainerBottomConstraint.constant = 0
        UIView.animateWithDuration(0.1, animations: { () -> Void in
            self.view.layoutIfNeeded()
        })
    }

    private func updateNextContainer() {
        nextContainerHeightConstraint.constant = selectedFriends.count > 0 ? 55 : 0
        nextLabel.text = selectedFriends.map({ $0.firstName }).joinWithSeparator(", ")
    }

    @objc private func searchQueryDidChange(textField: UITextField) {
        let searchQuery = (textField.text ?? "").uppercaseString
        filteredFriends = searchQuery.isEmpty ? nil : friends.filter({ $0.fullName.uppercaseString.containsString(searchQuery) })
        tableView.reloadData()
    }

    @IBAction func didTapNext() {
        if selectedFriends.count == 0 {
            return
        }

        delegate?.friendSelectorViewController(self, didSelectFriends: selectedFriends)
    }

    @IBAction func didTapClose() {
        dismissViewControllerAnimated(true, completion: nil)
    }
}

extension FriendSelectorViewController: UITableViewDataSource, UITableViewDelegate {
    func numberOfSectionsInTableView(tableView: UITableView) -> Int {
        return 1
    }

    func tableView(tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return filteredFriends?.count ?? friends.count
    }

    func tableView(tableView: UITableView, heightForRowAtIndexPath indexPath: NSIndexPath) -> CGFloat {
        return FriendSelectorTableViewCell.rowHeight
    }

    func tableView(tableView: UITableView, cellForRowAtIndexPath indexPath: NSIndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCellWithIdentifier(friendSelectorCellReuseIdentifier, forIndexPath: indexPath) as! FriendSelectorTableViewCell
        cell.friend = filteredFriends?[indexPath.row] ?? friends[indexPath.row]
        cell.delegate = self
        cell.isChecked = false

        for i in 0..<selectedFriends.count {
            if selectedFriends[i] == cell.friend {
                cell.isChecked = true
                break
            }
        }
    
        return cell
    }
}

extension FriendSelectorViewController: FriendSelectorTableViewCellDelegate {
    func friendSelectorTableViewCell(friendSelectorTableViewCell: FriendSelectorTableViewCell, isChecked: Bool) {
        guard let friend = friendSelectorTableViewCell.friend else {
            return
        }

        if isChecked {
            selectedFriends.append(friend)
        } else {
            for i in 0..<selectedFriends.count {
                if selectedFriends[i] == friend {
                    selectedFriends.removeAtIndex(i)
                    break
                }
            }
        }

        updateNextContainer()
    }
}

extension FriendSelectorViewController: UITextFieldDelegate {
    func textFieldShouldReturn(textField: UITextField) -> Bool {
        textField.resignFirstResponder()
        return true
    }
}