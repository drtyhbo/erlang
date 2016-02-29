//
//  KeyPair.swift
//  chat
//
//  Created by Andreas Binnewies on 2/29/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import Sodium

class KeyPair {
    typealias PublicKey = NSData
    typealias SecretKey = NSData

    static private let PublicKeyBytes = Int(crypto_box_publickeybytes())
    static private let SecretKeyBytes = Int(crypto_box_secretkeybytes())

    let publicKey: PublicKey
    let secretKey: SecretKey

    init(publicKey: PublicKey, secretKey: SecretKey) {
        self.publicKey = publicKey
        self.secretKey = secretKey
    }

    static func keyPair() -> KeyPair? {
        guard let publicKey = NSMutableData(length: PublicKeyBytes), secretKey = NSMutableData(length: SecretKeyBytes) else {
            return nil
        }
        if crypto_box_keypair(publicKey.mutableUInt8Bytes, secretKey.mutableUInt8Bytes) != 0 {
            return nil
        }
        return KeyPair(publicKey: PublicKey(data: publicKey), secretKey: SecretKey(data: secretKey))
    }
}