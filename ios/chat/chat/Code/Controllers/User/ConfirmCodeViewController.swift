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

        let nextButton = UIBarButtonItem(barButtonSystemItem: .Done, target: self, action: "confirmCode")
        navigationItem.rightBarButtonItem = nextButton
    }

    @objc private func confirmCode() {
        APIManager.sharedManager.confirmPhoneNumber(phoneNumber, withCode: code.text ?? "", key: SecurityHelper.sharedHelper.publicKey!) {
            userId, sessionToken, error in
            if let userId = userId, sessionToken = sessionToken {
                User.userId = userId
                User.sessionToken = sessionToken

                self.navigationController?.pushViewController(MainViewController(), animated: true)
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
}

extension ConfirmCodeViewController: UITextFieldDelegate {
    func textFieldShouldReturn(textField: UITextField) -> Bool {
        confirmCode()
        return true
    }
}