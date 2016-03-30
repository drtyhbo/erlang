//
//  PhoneNumberViewController.swift
//  chat
//
//  Created by Andreas Binnewies on 1/30/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import ChatCommon
import Foundation
import UIKit

class PhoneNumberViewController: UIViewController {
    @IBOutlet weak var phoneNumberTextField: UITextField!
    @IBOutlet weak var phoneNumberContainer: UIView!
    @IBOutlet weak var phoneNumberContainerVerticalConstraint: NSLayoutConstraint!

    private let keyboardNotifications = KeyboardNotifications()

    private var currentPhoneNumber: String {
        let notDigits = NSCharacterSet.decimalDigitCharacterSet().invertedSet
        return (phoneNumberTextField.text ?? "").componentsSeparatedByCharactersInSet(notDigits).joinWithSeparator("")
    }

    deinit {
        keyboardNotifications.removeNotifications()
    }

    init() {
        super.init(nibName: "PhoneNumberViewController", bundle: nil)
    }

    required init?(coder aDecoder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    override func viewDidLoad() {
        super.viewDidLoad()

        view.backgroundColor = UIColor.currentTheme.lightBackgroundColor

        setupNextButton()

        navigationItem.title = "Phone Number"
        navigationItem.backBarButtonItem = ThemedBarButtonItem(title: "Back", style: .Plain, target: nil, action: nil)

        keyboardNotifications.addNotificationsForWillShow({
                size in
                self.keyboardWillShowWithSize(size)
            });

        phoneNumberTextField.addTarget(self, action: "phoneNumberDidChange:", forControlEvents: .EditingChanged)
        phoneNumberTextField.becomeFirstResponder()
    }

    @objc private func confirmPhoneNumber() {
        setupActivityIndicator()

        let phoneNumber = PhoneNumber(phoneNumber: currentPhoneNumber)
        APIManager.sharedManager.registerPhoneNumber(phoneNumber, deviceUUID: User.deviceUUID) {
            result in
            if result {
                User.phoneNumber = phoneNumber.fullNumber

                let confirmCodeViewController = ConfirmCodeViewController(phoneNumber: phoneNumber)
                self.navigationController?.pushViewController(confirmCodeViewController, animated: true)

                self.setupNextButton()
            }
        }
    }

/*    private func createThemedBarButtonItemWithTitle(title: String, target: AnyObject?, action: Selector) -> UIBarButtonItem {
        let button = UIBarButtonItem(title: title, style: .Plain, target: target, action: action)
        button.setTitleTextAttributes([NSForegroundColorAttributeName: UIColor.currentTheme.buttonColor], forState: .Normal)
        button.tintColor = UIColor.currentTheme.buttonColor
        return button
    }*/

    private func setupNextButton() {
        let nextButton = ThemedBarButtonItem(title: "Next", style: .Plain, target: self, action: "confirmPhoneNumber")
        nextButton.enabled = (phoneNumberTextField.text ?? "").characters.count == 10
        navigationItem.rightBarButtonItem = nextButton
    }

    private func setupActivityIndicator() {
        let activityIndicator = UIActivityIndicatorView(activityIndicatorStyle: .Gray)
        navigationItem.rightBarButtonItem = UIBarButtonItem(customView: activityIndicator)
        activityIndicator.startAnimating()
    }

    private func keyboardWillShowWithSize(keyboardSize: CGSize) {
        phoneNumberContainerVerticalConstraint.constant = (view.bounds.size.height - keyboardSize.height) - (view.bounds.size.height / 2 + phoneNumberContainer.bounds.size.height / 2)
    }

    @objc private func phoneNumberDidChange(textField: UITextField) {
        var phoneNumber = currentPhoneNumber
        if phoneNumber.characters.count > 10 {
            phoneNumber = phoneNumber.substringWithRange(phoneNumber.startIndex..<phoneNumber.startIndex.advancedBy(10))
        }

        navigationItem.rightBarButtonItem?.enabled = phoneNumber.characters.count == 10
        textField.text = PhoneNumberFormatter().formatPhoneNumber(phoneNumber)
    }
}

extension PhoneNumberViewController: UITextFieldDelegate {
    func textFieldShouldReturn(textField: UITextField) -> Bool {
        confirmPhoneNumber()
        return true
    }
}