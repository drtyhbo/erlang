//
//  Conversation.swift
//  chat
//
//  Created by Andreas Binnewies on 3/1/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import CoreData
import Foundation

@objc(Conversation)
class Conversation: NSManagedObject {
    @NSManaged var isRatcheting: Bool
    @NSManaged var messageNumber: Int
    @NSManaged var preKeyIndex: Int
    @NSManaged var publicKey: NSData?
    @NSManaged var device: Device

    static func createWithDevice(device: Device) -> Conversation {
        let conversation = Conversation.MR_createEntity()!
        conversation.device = device
        conversation.isRatcheting = false
        return conversation
    }

    static func getOrCreateWithDevice(device: Device) -> Conversation {
        if let conversation = Conversation.findWithDevice(device) {
            return conversation
        }
        return createWithDevice(device)
    }

    static func findWithDevice(device: Device) -> Conversation? {
        return Conversation.MR_findFirstByAttribute("device", withValue: device)
    }
}