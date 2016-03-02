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

    typealias SharedSecret = NSData

    private let SharedRootKeyBytes = Int(crypto_scalarmult_bytes())

    func sharedSecret() -> SharedSecret {
        return Sodium()!.randomBytes.buf(32)!
    }

    func encryptData(data: NSData, withSharedSecret sharedSecret: SharedSecret) -> NSData? {
        return Sodium()!.secretBox.seal(data, secretKey: sharedSecret)
    }

    func decryptData(data: NSData, withSharedSecret sharedSecret: SharedSecret) -> NSData? {
        return Sodium()!.secretBox.open(data, secretKey: sharedSecret)
    }

    func encryptMessage(message: JSON, forFriend friend: Friend, callback: String?->Void) {
        let conversation = Conversation.getOrCreateWithFriend(friend)
        CoreData.save()

        if conversation.publicKey == nil {
            APIManager.sharedManager.getPrekeyForFriend(friend) { keyIndex, publicKey in
                guard let keyIndex = keyIndex, publicKey = publicKey else {
                    callback(nil)
                    return
                }

                conversation.preKeyIndex = keyIndex
                conversation.publicKey = publicKey
                CoreData.save()

                self.handleEncryption(message, conversation: conversation, callback: callback)
            }
        } else {
            handleEncryption(message, conversation: conversation, callback: callback)
        }
    }

    func decryptMessage(message: String, forFriend friend: Friend) -> JSON? {
        let conversation = Conversation.getOrCreateWithFriend(friend)
        CoreData.save()

        return handleDecryption(message, conversation: conversation)
    }

    // TODO: Call this in a synchronous messaging queue to prevent this from being run more than
    // once at a time.
    private func handleEncryption(message: JSON, conversation: Conversation, callback: String?->Void) {
        var keyPair: KeyPair
        if !conversation.isRatcheting {
            keyPair = KeyPair.keyPair()!
        } else {
            keyPair = KeyPair.fromKeychainWithKey(conversation.friend.id)!
        }
        keyPair.saveToKeychainWithKey(conversation.friend.id)

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

        guard let encryptedMessage = encryptData(try! message.rawData(), withSharedSecret: messageKey) else {
            callback(nil)
            return
        }

        var json = JSON([
            "k": keyPair.publicKey.base64,
            "c": conversation.messageNumber,
            "m": encryptedMessage.base64])
        if conversation.preKeyIndex >= 0 {
            json["pki"] = JSON(conversation.preKeyIndex)
        }

        conversation.isRatcheting = true
        conversation.messageNumber++
        CoreData.save()

        callback(try! json.rawData().base64)
    }

    private func handleDecryption(message: String, conversation: Conversation) -> JSON? {
        guard let messageData = NSData.fromBase64(message) else {
            return nil
        }

        let sodium = Sodium()!
        let json = JSON(data: messageData)
        guard let otherPublicKeyBase64 = json["k"].string, otherPublicKey = NSData.fromBase64(otherPublicKeyBase64), messageNumber = json["c"].int, encryptedMessageBase64 = json["m"].string, encryptedMessage = NSData.fromBase64(encryptedMessageBase64) else {
            return nil
        }

        conversation.publicKey = otherPublicKey
        conversation.preKeyIndex = -1
        conversation.isRatcheting = false
        CoreData.save()

        let keyPair: KeyPair
        if let preKeyIndex = json["pki"].int {
            guard let preKey = PreKeyCache.sharedCache.preKeyForIndex(preKeyIndex) else {
                return nil
            }
            keyPair = preKey.keyPair
        } else {
            guard let keychainKeyPair = KeyPair.fromKeychainWithKey(conversation.friend.id) else {
                return nil
            }
            keyPair = keychainKeyPair
        }

        guard let sharedSecret = generateSharedSecretForConversation(conversation, withKeyPair: keyPair) else {
            return nil
        }

        let hash = Hash()
        hash.update([sharedSecret, "message\(messageNumber)".utf8Data])
        guard let messageKey = hash.final(), decryptedMessage: NSData = sodium.secretBox.open(encryptedMessage, secretKey: messageKey) else {
            return nil
        }

        return JSON(data: decryptedMessage)
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