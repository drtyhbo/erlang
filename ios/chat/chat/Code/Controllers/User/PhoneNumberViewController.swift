//
//  PhoneNumberViewController.swift
//  chat
//
//  Created by Andreas Binnewies on 1/30/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

class PhoneNumberViewController: UIViewController {
    @IBOutlet weak var phoneNumberTextField: UITextField!

    private var currentPhoneNumber: String {
        let notDigits = NSCharacterSet.decimalDigitCharacterSet().invertedSet
        return (phoneNumberTextField.text ?? "").componentsSeparatedByCharactersInSet(notDigits).joinWithSeparator("")
    }

    init() {
        super.init(nibName: "PhoneNumberViewController", bundle: nil)
    }

    required init?(coder aDecoder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    override func viewDidLoad() {
        super.viewDidLoad()

        setupNextButton()

        navigationItem.title = "Phone Number"
        navigationItem.backBarButtonItem = UIBarButtonItem(title: "Back", style: .Plain, target: nil, action: nil)

        phoneNumberTextField.addTarget(self, action: "phoneNumberDidChange:", forControlEvents: .EditingChanged)
        phoneNumberTextField.becomeFirstResponder()
    }

    @objc private func confirmPhoneNumber() {
        setupActivityIndicator()

        let phoneNumber = PhoneNumber(phoneNumber: currentPhoneNumber)
        APIManager.sharedManager.registerPhoneNumber(phoneNumber) {
            result in
            if result {
                User.phoneNumber = phoneNumber.fullNumber

                let confirmCodeViewController = ConfirmCodeViewController(phoneNumber: phoneNumber)
                self.navigationController?.pushViewController(confirmCodeViewController, animated: true)

                self.setupNextButton()
            }
        }
    }

    private func setupNextButton() {
        let nextButton = UIBarButtonItem(title: "Next", style: .Plain, target: self, action: "confirmPhoneNumber")
        navigationItem.rightBarButtonItem = nextButton
    }

    private func setupActivityIndicator() {
        let activityIndicator = UIActivityIndicatorView(activityIndicatorStyle: .Gray)
        navigationItem.rightBarButtonItem = UIBarButtonItem(customView: activityIndicator)
        activityIndicator.startAnimating()
    }

    @objc private func phoneNumberDidChange(textField: UITextField) {
        var phoneNumber = currentPhoneNumber
        if phoneNumber.characters.count > 10 {
            phoneNumber = phoneNumber.substringWithRange(phoneNumber.startIndex..<phoneNumber.startIndex.advancedBy(10))
        }

        textField.text = PhoneNumberFormatter().formatPhoneNumber(phoneNumber)
    }
}

extension PhoneNumberViewController: UITextFieldDelegate {
    func textFieldShouldReturn(textField: UITextField) -> Bool {
        confirmPhoneNumber()
        return true
    }
}