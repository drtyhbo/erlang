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

public class MessageCrypter {
    public static let sharedCrypter = MessageCrypter();

    public typealias SharedSecret = NSData

    private let SharedRootKeyBytes = Int(crypto_scalarmult_bytes())

    func sharedSecret() -> SharedSecret {
        let hash = Hash()
        hash.update(Sodium()!.randomBytes.buf(32)!)
        return hash.final()!
    }

    func encryptData(data: NSData, withSharedSecret sharedSecret: SharedSecret) -> NSData? {
        return Sodium()!.secretBox.seal(data, secretKey: sharedSecret)
    }

    public func decryptData(data: NSData, withSharedSecret sharedSecret: SharedSecret) -> NSData? {
        return Sodium()!.secretBox.open(data, secretKey: sharedSecret)
    }

    func encryptData(unencryptedData: NSData, forFriend friend: Friend, callback: [String:AnyObject]?->Void) {
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

                self.handleEncryption(unencryptedData, conversation: conversation, callback: callback)
            }
        } else {
            handleEncryption(unencryptedData, conversation: conversation, callback: callback)
        }
    }

    func decryptMessage(message: [String:AnyObject], forFriend friend: Friend) -> NSData? {
        let conversation = Conversation.getOrCreateWithFriend(friend)
        CoreData.save()

        return handleDecryption(message, conversation: conversation)
    }

    // TODO: Call this in a synchronous messaging queue to prevent this from being run more than
    // once at a time.
    private func handleEncryption(unencryptedData: NSData, conversation: Conversation, callback: [String:AnyObject]?->Void) {
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

        guard let encryptedMessage = encryptData(unencryptedData, withSharedSecret: messageKey) else {
            callback(nil)
            return
        }

        var json: [String:AnyObject] = [
            "pk": keyPair.publicKey.base64,
            "c": conversation.messageNumber,
            "m": encryptedMessage.base64]
        if conversation.preKeyIndex >= 0 {
            json["pki"] = conversation.preKeyIndex
        }

        conversation.isRatcheting = true
        conversation.messageNumber++
        CoreData.save()

        callback(json)
    }

    private func handleDecryption(json: [String:AnyObject], conversation: Conversation) -> NSData? {
        let sodium = Sodium()!
        guard let otherPublicKeyBase64 = json["pk"] as? String, otherPublicKey = NSData.fromBase64(otherPublicKeyBase64), messageNumber = json["c"] as? Int, encryptedMessageBase64 = json["m"] as? String, encryptedMessage = NSData.fromBase64(encryptedMessageBase64) else {
            return nil
        }

        conversation.publicKey = otherPublicKey
        conversation.preKeyIndex = -1
        conversation.isRatcheting = false
        CoreData.save()

        let keyPair: KeyPair
        if let preKeyIndex = json["pki"] as? Int {
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

        return decryptedMessage
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