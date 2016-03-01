//
//  MessageCrypter.swift
//  chat
//
//  Created by Andreas Binnewies on 3/1/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import Sodium
import SwiftyJSON

class MessageCrypter {
    static let sharedCrypter = MessageCrypter();

    private typealias SharedSecret = NSData

    private let SharedRootKeyBytes = Int(crypto_scalarmult_bytes())

    func encryptMessage(message: JSON, forFriend friend: Friend, callback: String?->Void) {
        let conversation = Conversation.getOrCreateWithFriend(friend)
        CoreData.save()

        if conversation.publicKey == nil {
            APIManager.sharedManager.getPrekeyForFriend(friend) { keyIndex, publicKey in
                guard let keyIndex = keyIndex, publicKey = publicKey else {
                    callback(nil)
                    return
                }

                let conversation = Conversation.getOrCreateWithFriend(friend)
                conversation.preKeyIndex = keyIndex
                conversation.publicKey = publicKey
                CoreData.save()

                self.handleEncryption(message, conversation: conversation, callback: callback)
            }
        } else {
            handleEncryption(message, conversation: conversation, callback: callback)
        }
    }

    func decryptMessage(message: JSON, fromFriend friend: Friend) {

    }

    // TODO: Call this in a synchronous messaging queue to prevent this from being run more than
    // once at a time.
    private func handleEncryption(message: JSON, conversation: Conversation, callback: String?->Void) {
        var keyPair: KeyPair
        if !conversation.isRatcheting {
            keyPair = KeyPair.keyPair()!
        } else {
            keyPair = KeyPair.fromKeychainWithKey("\(conversation.friend.id)")!
        }
        conversation.isRatcheting = false

        guard let sharedSecret = generateSharedSecretForConversation(conversation, withKeyPair: keyPair) else {
            callback(nil)
            return
        }

        let hash = Hash()
        hash.update([sharedSecret, "message\(conversation.messageNumber)".utf8Data])
        guard let messageKey = hash.final() else {
            callback(nil)
            return
        }

        let sodium = Sodium()!
        guard let encryptedMessage: NSData = try! sodium.secretBox.seal(message.rawData(), secretKey: messageKey) else {
            callback(nil)
            return
        }

        var json = JSON([
            "k": sodium.utils.bin2hex(keyPair.publicKey)!,
            "c": conversation.messageNumber,
            "m": sodium.utils.bin2hex(encryptedMessage)!])
        if conversation.preKeyIndex < 0 {
            json["pki"] = JSON(conversation.preKeyIndex)
        }

        conversation.messageNumber++
        CoreData.save()

        callback(try! json.rawData().base64)
    }

    private func generateSharedSecretForConversation(conversation: Conversation, withKeyPair keyPair: KeyPair) -> SharedSecret? {
        guard let sharedSecret = NSMutableData(length: SharedRootKeyBytes) else {
            return nil
        }

        if crypto_scalarmult(sharedSecret.mutableUInt8Bytes, keyPair.secretKey.UInt8Bytes, conversation.publicKey!.UInt8Bytes) != 0 {
            return nil
        }

        let hash = Hash()
        if User.userId < conversation.friend.id {
            hash.update([sharedSecret, keyPair.publicKey, conversation.publicKey!])
        } else {
            hash.update([sharedSecret, conversation.publicKey!, keyPair.publicKey])
        }
        return hash.final()
    }
}