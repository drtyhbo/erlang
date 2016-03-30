//
//  Device.swift
//  chat
//
//  Created by Andreas Binnewies on 3/28/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import CoreData
import Foundation
import MagicalRecord

@objc(Device)
public class Device: NSManagedObject {
    @NSManaged var id: Int
    @NSManaged var lastActive: NSDate
    @NSManaged var owner: Friend?

    public static func createWithId(id: Int, owner: Friend? = nil) -> Device {
        if let device = findWithId(id) {
            device.lastActive = NSDate()
            return device
        }

        let device = Device.MR_createEntity()!
        device.id = id
        device.owner = owner
        device.lastActive = NSDate()

        return device
    }

    static func findWithId(id: Int) -> Device? {
        return Device.MR_findFirstByAttribute("id", withValue: id)
    }

    static func activeDeviceForFriend(friend: Friend) -> Device? {
        return Device.MR_findFirstByAttribute("owner", withValue: friend)
    }
}