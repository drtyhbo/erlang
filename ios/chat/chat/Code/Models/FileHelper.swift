//
//  FileHelper.swift
//  chat
//
//  Created by Andreas Binnewies on 2/15/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation

class FileHelper {
    static func getFileWithId(id: Int, completion: File?->Void) {
        if let file = File.findWithId(id) {
            completion(file)
            return
        }

        APIManager.sharedManager.getUrlForFileWithId(id) {
            url in
            guard let url = url else {
                completion(nil)
                return
            }

            APIManager.sharedManager.downloadFileWithUrl(url) {
                data, contentType in

                var file: File?
/*                if let encryptedData = data, contentType = contentType, decryptedData = SecurityHelper.sharedHelper.decrypt(encryptedData) {
                    file = File.createWithId(id, data: decryptedData, contentType: contentType)
                    CoreData.save()
                }*/

                completion(file)
            }
        }
    }

    static func saveFileToTemporaryLocation(file: File) -> NSURL {
        if let localPath = file.localPath {
            if NSFileManager.defaultManager().fileExistsAtPath(localPath) {
                return NSURL(fileURLWithPath: localPath)
            }
        }

        let temporaryUrl = NSURL(fileURLWithPath: NSTemporaryDirectory()).URLByAppendingPathComponent("\(NSUUID().UUIDString)\(extensionForFile(file))")
        file.data.writeToURL(temporaryUrl, atomically: true)
        file.localPath = temporaryUrl.path
        CoreData.save()

        return temporaryUrl
    }

    private static func extensionForFile(file: File) -> String {
        switch (file.contentType) {
        case "video/mp4":
            return ".mp4"
        default:
            return ""
        }
    }
}