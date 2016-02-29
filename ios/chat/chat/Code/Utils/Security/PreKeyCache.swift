//
//  PreKeyCache.swift
//  chat
//
//  Created by Andreas Binnewies on 2/29/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import Sodium

class PreKey {
    let keyPair: KeyPair
    let index: Int

    init (keyPair: KeyPair, index: Int) {
        self.keyPair = keyPair
        self.index = index
    }
}

class PreKeyCache {
    static let sharedCache = PreKeyCache()

    private let initialPreKeyCount = 100
    private let indexOfLastResort = 0xFFFF
    private let hasGeneratedPreKeysKey = "GeneratedPreKeys"

    func generateInitialCache() -> [PreKey] {
        var preKeys: [PreKey] = []
        for i in 0..<initialPreKeyCount {
            if let preKey = generateKeyPairForIndex(i) {
                preKeys.append(preKey)
            }
        }
        if let preKey = generateKeyPairForIndex(indexOfLastResort) {
            preKeys.append(preKey)
        }

        return preKeys
    }

    private func generateKeyPairForIndex(index: Int) -> PreKey? {
        if let preKey = preKeyForIndex(index) {
            return preKey
        }

        let sodium = Sodium()!
        guard let keyPair = KeyPair.keyPair(), hexPublicKey = sodium.utils.bin2hex(keyPair.publicKey), hexSecretKey = sodium.utils.bin2hex(keyPair.secretKey) else {
            return nil
        }

        let keychain = Keychain()
        keychain.setString(hexPublicKey, forKey: "pk\(index)")
        keychain.setString(hexSecretKey, forKey: "sk\(index)")

        return PreKey(keyPair: keyPair, index: index)
    }

    private func preKeyForIndex(index: Int) -> PreKey? {
        let sodium = Sodium()!
        let keychain = Keychain()

        guard let hexPublicKey = keychain.stringForKey("pk\(index)"), hexSecretKey = keychain.stringForKey("sk\(index)"), publicKey = sodium.utils.hex2bin(hexPublicKey), secretKey = sodium.utils.hex2bin(hexSecretKey) else {
            return nil
        }

        return PreKey(keyPair: KeyPair(publicKey: publicKey, secretKey: secretKey), index: index)
    }
}