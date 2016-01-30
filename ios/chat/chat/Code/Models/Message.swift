//
//  Message.swift
//  chat
//
//  Created by Andreas Binnewies on 1/13/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation

class Message {
/*    var imageInfo: (NSURL, Int, Int)? {
        if let imageURL = image?.url, width = extra["width"] as? Int, height = extra["height"] as? Int {
            return (NSURL(string: imageURL)!, width, height)
        }
        return nil
    }

    static func parseClassName() -> String {
        return "Message"
    }

    static func sendMessage(message: String, completion: Message?->Void) {
        PFCloud.callFunctionInBackground("sendMessage", withParameters: ["message": message]) {
            message, error in
            if error != nil {
                completion(nil)
            } else {
                completion(message as? Message)
            }
        }
    }

    static func sendImageWithURL(imageURL: NSURL, width: Int, height: Int, completion: Message?->Void) {
        let parameters: [NSObject: AnyObject] = [
            "imageURL": imageURL.absoluteString,
            "width": width,
            "height": height
        ]
        PFCloud.callFunctionInBackground("sendMessage", withParameters: parameters) {
            message, error in
            if error != nil {
                completion(nil)
            } else {
                completion(message as? Message)
            }
        }
    }

    static func sendImageFile(imageFile: PFFile, width: Int, height: Int, completion: Message?->Void) {
        imageFile.saveInBackgroundWithBlock {
            success, error in
            if !success {
                return
            }

            let parameters: [NSObject: AnyObject] = [
                "imageFile": imageFile,
                "width": width,
                "height": height
            ]
            PFCloud.callFunctionInBackground("sendMessage", withParameters: parameters) {
                message, error in
                if error != nil {
                    completion(nil)
                } else {
                    completion(message as? Message)
                }
            }
        }
    }

    static func loadMessagesFromDate(date: NSDate?, completion: [Message]?->Void) {
        var parameters: [NSObject: AnyObject] = [:]
        if let date = date {
            parameters["date"] = date
        }
        PFCloud.callFunctionInBackground("loadMessages", withParameters: parameters) {
            messages, error in
            if error != nil {
                completion(nil)
            } else {
                completion(messages as? [Message])
            }
        }
    }*/
}