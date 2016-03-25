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
    static let fetchLimit = 30

    static let profilePicSize = CGSize(width: 144, height: 144)

    static let themes: [ColorTheme] = [
        ColorTheme(lightBackgroundColor: UIColor(0xFFF8FB), darkBackgroundColor: UIColor(0x30021C), borderColor: UIColor(0xFCE4EC), buttonColor: UIColor(0xF06292)),
        ColorTheme(lightBackgroundColor: UIColor(0xEFF0F9), darkBackgroundColor: UIColor(0x0C0C31), borderColor: UIColor(0xC5CAE9), buttonColor: UIColor(0x7986CB)),
        ColorTheme(lightBackgroundColor: UIColor(0xECF2F2), darkBackgroundColor: UIColor(0x004D40), borderColor: UIColor(0xB2DFDB), buttonColor: UIColor(0x009688)),
        ColorTheme(lightBackgroundColor: UIColor(0xFAFAFA), darkBackgroundColor: UIColor(0x000000), borderColor: UIColor(0xEEEEEE), buttonColor: UIColor(0x555555))]

    static let otherChatBubbleColor = UIColor(0xE5E5EA)

    struct BubbleLayout {
        static let minPadding: CGFloat = 10
        static let maxPadding: CGFloat = 70
    }
}