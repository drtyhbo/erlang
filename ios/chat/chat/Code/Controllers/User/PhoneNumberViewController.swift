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
    @IBOutlet weak var phoneNumber: UITextField!
    @IBOutlet weak var confirmButton: UIButton!
    @IBOutlet weak var activityIndicator: UIActivityIndicatorView!

    init() {
        super.init(nibName: "PhoneNumberViewController", bundle: nil)
    }

    required init?(coder aDecoder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    func confirmPhoneNumber() {
        confirmButton.hidden = true
        activityIndicator.hidden = false

        let phoneNumber = self.phoneNumber.text ?? ""
        APIManager.registerPhoneNumber(phoneNumber) {
            result in
            if result {
                let confirmCodeViewController = ConfirmCodeViewController(phoneNumber: phoneNumber)
                self.navigationController?.pushViewController(confirmCodeViewController, animated: true)
            }

            self.confirmButton.hidden = false
            self.activityIndicator.hidden = true
        }
    }

    @IBAction func tapConfirm() {
        confirmPhoneNumber()
    }
}

extension PhoneNumberViewController: UITextFieldDelegate {
    func textFieldShouldReturn(textField: UITextField) -> Bool {
        confirmPhoneNumber()
        return true
    }
}