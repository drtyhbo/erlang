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

    init() {
        super.init(nibName: "PhoneNumberViewController", bundle: nil)
    }

    required init?(coder aDecoder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    override func viewDidLoad() {
        super.viewDidLoad()

        setupConfirmButton()
    }

    @objc private func confirmPhoneNumber() {
        let activityIndicator = UIActivityIndicatorView(activityIndicatorStyle: .Gray)
        navigationItem.rightBarButtonItem = UIBarButtonItem(customView: activityIndicator)
        activityIndicator.startAnimating()

        let phoneNumber = PhoneNumber(phoneNumber: self.phoneNumber.text ?? "")
        APIManager.sharedManager.registerPhoneNumber(phoneNumber) {
            result in
            if result {
                User.phoneNumber = phoneNumber.toString()

                let confirmCodeViewController = ConfirmCodeViewController(phoneNumber: phoneNumber)
                self.navigationController?.pushViewController(confirmCodeViewController, animated: true)

                self.setupConfirmButton()
            }
        }
    }

    private func setupConfirmButton() {
        let nextButton = UIBarButtonItem(barButtonSystemItem: .Done, target: self, action: "confirmPhoneNumber")
        navigationItem.rightBarButtonItem = nextButton
    }
}

extension PhoneNumberViewController: UITextFieldDelegate {
    func textFieldShouldReturn(textField: UITextField) -> Bool {
        confirmPhoneNumber()
        return true
    }
}