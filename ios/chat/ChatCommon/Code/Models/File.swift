//
//  File.swift
//  chat
//
//  Created by Andreas Binnewies on 2/12/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import CoreData
import Foundation
import UIKit

@objc(File)
public class File: NSManagedObject {
    @NSManaged var id: String
    @NSManaged public var contentType: String
    @NSManaged public var data: NSData
    @NSManaged public var localPath: String?

    public var image: UIImage? {
        return UIImage(data: data)
    }

    public static func createWithId(id: String, data: NSData, contentType: String) -> File {
        let file = File.MR_createEntity()!
        file.id = id
        file.data = data
        file.contentType = contentType
        return file
    }

    public static func findWithId(id: String) -> File? {
        return File.MR_findFirstByAttribute("id", withValue: id)
    }
}