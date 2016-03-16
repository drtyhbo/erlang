//
//  ConfirmCodeViewController.swift
//  chat
//
//  Created by Andreas Binnewies on 1/30/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

class ConfirmCodeViewController: UIViewController {
    @IBOutlet weak var instructionalLabel: UILabel!
    @IBOutlet weak var code: UITextField!
    @IBOutlet weak var confirmCodeContainer: UIView!
    @IBOutlet weak var confirmCodeVerticalConstraint: NSLayoutConstraint!

    private let keyboardNotifications = KeyboardNotifications()
    private let phoneNumber: PhoneNumber!

    private var isConfirming = false
    private var preKeys: [PreKey] = []

    init(phoneNumber: PhoneNumber) {
        self.phoneNumber = phoneNumber
        super.init(nibName: "ConfirmCodeViewController", bundle: nil)
    }

    required init?(coder aDecoder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    override func viewDidLoad() {
        super.viewDidLoad()

        view.backgroundColor = UIColor.currentTheme.lightBackgroundColor

        navigationItem.title = PhoneNumberFormatter().formatPhoneNumber(phoneNumber.phoneNumber)
        navigationItem.backBarButtonItem = UIBarButtonItem(title: "Back", style: .Plain, target: nil, action: nil)

        keyboardNotifications.addNotificationsForWillShow({
                size in
                self.keyboardWillShowWithSize(size)
            });

        instructionalLabel.text = String(format: instructionalLabel.text!, PhoneNumberFormatter().formatPhoneNumber(phoneNumber.phoneNumber))

        code.addTarget(self, action: "codeDidChange:", forControlEvents: .EditingChanged)
        code.becomeFirstResponder()
    }

    override func viewWillAppear(animated: Bool) {
        super.viewWillAppear(animated)

        setupNextButton()

        dispatch_async(dispatch_get_main_queue()) {
            self.preKeys = PreKeyCache.sharedCache.generateInitialCache()
        }
    }

    private func setupNextButton() {
        let nextButton = UIBarButtonItem(title: "Next", style: .Plain, target: self, action: "confirmCode")
        navigationItem.rightBarButtonItem = nextButton
    }

    private func setupActivityIndicator() {
        let activityIndicator = UIActivityIndicatorView(activityIndicatorStyle: .Gray)
        navigationItem.rightBarButtonItem = UIBarButtonItem(customView: activityIndicator)
        activityIndicator.startAnimating()
    }

    private func keyboardWillShowWithSize(keyboardSize: CGSize) {
        confirmCodeVerticalConstraint.constant = (view.bounds.size.height - keyboardSize.height) - (view.bounds.size.height / 2 + confirmCodeContainer.bounds.size.height / 2)
    }

    @objc private func confirmCode() {
        setupActivityIndicator()
        isConfirming = true

        APIManager.sharedManager.confirmPhoneNumber(phoneNumber, withCode: code.text ?? "", preKeys: preKeys) {
            userIdString, sessionToken, firstName, lastName, error in

            self.setupNextButton()
            self.isConfirming = false

            if let userIdString = userIdString, userId = Int(userIdString), sessionToken = sessionToken {
                User.userId = userId
                User.sessionToken = sessionToken
                User.firstName = firstName
                User.lastName = lastName

                self.navigationController?.pushViewController(firstName?.isEmpty == false ? MainViewController() : UserInfoViewController(), animated: true)
            }
        }
    }

    @IBAction func tapConfirm() {
        confirmCode()
    }

    @objc private func codeDidChange(textField: UITextField) {
        if textField.text?.characters.count == 6 {
            confirmCode()
        }
    }
}

extension ConfirmCodeViewController: UITextFieldDelegate {
    func textField(textField: UITextField, shouldChangeCharactersInRange range: NSRange, replacementString string: String) -> Bool {
        return !isConfirming
    }

    func textFieldShouldReturn(textField: UITextField) -> Bool {
        confirmCode()
        return true
    }
}