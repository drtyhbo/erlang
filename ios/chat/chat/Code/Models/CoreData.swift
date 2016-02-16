//
//  CoreData.swift
//  chat
//
//  Created by Andreas Binnewies on 2/15/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import CoreData
import Foundation
import MagicalRecord

class CoreData {
    static func save() {
        NSManagedObjectContext.MR_defaultContext().MR_saveToPersistentStoreAndWait()
    }
}