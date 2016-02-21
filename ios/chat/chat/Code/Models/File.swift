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
class File: NSManagedObject {
    @NSManaged var id: Int
    @NSManaged var contentType: String
    @NSManaged var data: NSData
    @NSManaged var localPath: String?

    var image: UIImage? {
        return UIImage(data: data)
    }

    static func createWithId(id: Int, data: NSData, contentType: String) -> File {
        let file = File.MR_createEntity()!
        file.id = id
        file.data = data
        file.contentType = contentType
        return file
    }

    static func findWithId(id: Int) -> File? {
        return File.MR_findFirstByAttribute("id", withValue: id)
    }
}