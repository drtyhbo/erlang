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
    static let host = "chat1.drtyhbo.com"
    static let webPort = "80"
/*    static let host = "192.168.1.133"
    static let webPort = "8080"*/

    static let profilePicSize = CGSize(width: 144, height: 144)
    static let profilePicBaseUrl = NSURL(string: "https://s3-us-west-1.amazonaws.com/drtyhbo-chat-users/")!

    static let themes: [ColorTheme] = [
        ColorTheme(lightBackgroundColor: UIColor(0xFFF8FB), darkBackgroundColor: UIColor(0x30021C), borderColor: UIColor(0xFCE4EC), buttonColor: UIColor(0xF06292)),
        ColorTheme(lightBackgroundColor: UIColor(0xEFF0F9), darkBackgroundColor: UIColor(0x0C0C31), borderColor: UIColor(0xC5CAE9), buttonColor: UIColor(0x7986CB)),
        ColorTheme(lightBackgroundColor: UIColor(0xECF2F2), darkBackgroundColor: UIColor(0x004D40), borderColor: UIColor(0xE0F2F1), buttonColor: UIColor(0x009688)),
        ColorTheme(lightBackgroundColor: UIColor(0xEFEFEF), darkBackgroundColor: UIColor(0x000000), borderColor: UIColor(0xEEEEEE), buttonColor: UIColor(0x555555))]
}