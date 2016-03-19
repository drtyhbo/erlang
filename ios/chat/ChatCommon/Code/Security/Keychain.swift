//
//  Keychain.swift
//  chat
//
//  Created by Andreas Binnewies on 2/29/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import PDKeychainBindingsController

class Keychain {
    private let keychainBindings: PDKeychainBindings

    init() {
        self.keychainBindings = PDKeychainBindings.sharedKeychainBindings()
    }

    func setString(string: String, forKey key: String) {
        keychainBindings.setString(string, forKey: key)
    }

    func stringForKey(key: String) -> String? {
        return keychainBindings.stringForKey(key)
    }

    func removeObjectForKey(key: String) {
        keychainBindings.removeObjectForKey(key)
    }
}