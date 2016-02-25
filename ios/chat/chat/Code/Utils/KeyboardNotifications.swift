//
//  KeyboardNotifications.swift
//  chat
//
//  Created by Andreas Binnewies on 2/25/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

class KeyboardNotifications {
    private var willShowCallback: (CGSize->Void)?
    private var willHideCallback: (CGSize->Void)?

    func addNotificationsForWillShow(willShow: CGSize->Void, willHide: CGSize->Void) {
        willShowCallback = willShow
        willHideCallback = willHide

        NSNotificationCenter.defaultCenter().addObserver(self, selector: "keyboardWillShow:", name: UIKeyboardWillShowNotification, object: nil)
        NSNotificationCenter.defaultCenter().addObserver(self, selector: "keyboardWillHide:", name: UIKeyboardWillHideNotification, object: nil)
    }

    func removeNotifications() {
        NSNotificationCenter.defaultCenter().removeObserver(self)
    }

    @objc private func keyboardWillShow(notification: NSNotification) {
        let info = notification.userInfo!
        let keyboardFrame: CGRect = (info[UIKeyboardFrameEndUserInfoKey] as! NSValue).CGRectValue()

        willShowCallback?(keyboardFrame.size)
    }

    @objc private func keyboardWillHide(notification: NSNotification) {
        let info = notification.userInfo!
        let keyboardFrame: CGRect = (info[UIKeyboardFrameEndUserInfoKey] as! NSValue).CGRectValue()

        willHideCallback?(keyboardFrame.size)
    }
}