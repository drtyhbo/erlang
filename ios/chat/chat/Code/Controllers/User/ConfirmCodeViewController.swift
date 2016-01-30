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
    @IBOutlet weak var confirmButton: UIButton!
    @IBOutlet weak var activityIndicator: UIActivityIndicatorView!

    private let phoneNumber: String!

    init(phoneNumber: String) {
        self.phoneNumber = phoneNumber
        super.init(nibName: "ConfirmCodeViewController", bundle: nil)
    }

    required init?(coder aDecoder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    func confirmCode() {
        confirmButton.hidden = true
        activityIndicator.hidden = false

        APIManager.confirmPhoneNumber(phoneNumber, withCode: code.text ?? "") {
            sessionToken in
            if let sessionToken = sessionToken {
                User.username = self.phoneNumber
                User.sessionToken = sessionToken

                self.navigationController?.pushViewController(ChatViewController(), animated: true)
            }
            self.confirmButton.hidden = true
            self.activityIndicator.hidden = false
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