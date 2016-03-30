//
//  DeviceManager.swift
//  chat
//
//  Created by Andreas Binnewies on 3/28/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import CoreData
import Foundation
import MagicalRecord

public class DeviceManager {
    public static let sharedManager = DeviceManager()

    public func activeDevicesForFriends(var friends: [Friend], completion: [Device]->Void) {
        var devices: [Device] = []
        for var i = friends.count - 1; i >= 0; i-- {
            if let device = Device.activeDeviceForFriend(friends[i]) {
                devices.append(device)
                friends.removeAtIndex(i)
            }
        }

        if friends.isEmpty {
            completion(devices)
        } else {
            APIManager.sharedManager.activeDevicesForFriends(friends) { deviceIds in
                for i in 0..<deviceIds.count {
                    devices.append(Device.createWithId(deviceIds[i], owner: friends[i]))
                }
                CoreData.save()

                completion(devices)
            }
        }
    }
}