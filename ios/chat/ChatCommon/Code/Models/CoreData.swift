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

public class CoreData {
    public static func save() {
        NSManagedObjectContext.MR_defaultContext().MR_saveToPersistentStoreAndWait()
    }

    public static func setup() {
        let storeUrl = NSFileManager.defaultManager().containerURLForSecurityApplicationGroupIdentifier("group.drtyhbo.Chat")
        MagicalRecord.setupCoreDataStackWithAutoMigratingSqliteStoreAtURL(storeUrl!.URLByAppendingPathComponent("db.sqlite"))
    }
}