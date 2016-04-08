//
//  Constants.swift
//  chat
//
//  Created by Andreas Binnewies on 2/6/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

class Constants {
/*    static let host = "chat1.drtyhbo.com"
    static let webPort = "80"*/
    static let host = "127.0.0.1"
    static let webPort = "8080"

    static let profilePicSize = CGSize(width: 144, height: 144)
    static let profilePicBaseUrl = NSURL(string: "https://s3-us-west-1.amazonaws.com/drtyhbo-chat-users/")!

    struct Connection {
        static var reconnectionTimeInterval: NSTimeInterval {
            return drand48() * 5
        }
    }
}