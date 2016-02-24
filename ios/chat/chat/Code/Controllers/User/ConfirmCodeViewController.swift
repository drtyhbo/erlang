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

    private let phoneNumber: PhoneNumber!

    init(phoneNumber: PhoneNumber) {
        self.phoneNumber = phoneNumber
        super.init(nibName: "ConfirmCodeViewController", bundle: nil)
    }

    required init?(coder aDecoder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    override func viewDidLoad() {
        super.viewDidLoad()

        setupNextButton()
        
        navigationItem.title = PhoneNumberFormatter().formatPhoneNumber(phoneNumber.phoneNumber)
        navigationItem.backBarButtonItem = UIBarButtonItem(title: "Back", style: .Plain, target: nil, action: nil)

        instructionalLabel.text = String(format: instructionalLabel.text!, PhoneNumberFormatter().formatPhoneNumber(phoneNumber.phoneNumber))

        code.addTarget(self, action: "codeDidChange:", forControlEvents: .EditingChanged)
        code.becomeFirstResponder()
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

    @objc private func confirmCode() {
        code.enabled = false
        APIManager.sharedManager.confirmPhoneNumber(phoneNumber, withCode: code.text ?? "", key: SecurityHelper.sharedHelper.publicKey!) {
            userId, sessionToken, firstName, lastName, error in
            self.code.enabled = true
            if let userId = userId, sessionToken = sessionToken {
                User.userId = userId
                User.sessionToken = sessionToken
                User.firstName = firstName
                User.lastName = lastName

                self.navigationController?.pushViewController(UserInfoViewController(), animated: true)
            } else if let error = error {
                switch(error.error) {
                case "mismatch":
                    print ("mismatch")
                default:
                    print ("error")
                }
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
    func textFieldShouldReturn(textField: UITextField) -> Bool {
        confirmCode()
        return true
    }
}