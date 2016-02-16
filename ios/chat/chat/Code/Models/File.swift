//
//  File.swift
//  chat
//
//  Created by Andreas Binnewies on 2/12/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import CoreData
import Foundation

@objc(File)
class File: NSManagedObject {
    @NSManaged var id: Int
    @NSManaged var data: NSData
    @NSManaged var thumbData: NSData?

    static func createWithId(id: Int, data: NSData) -> File {
        let file = File.MR_createEntity()!
        file.id = id
        file.data = data
        return file
    }

    static func findWithId(id: Int) -> File? {
        return File.MR_findFirstByAttribute("id", withValue: id)
    }
}