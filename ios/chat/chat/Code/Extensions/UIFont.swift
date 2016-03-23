//
//  UIFont.swift
//  chat
//
//  Created by Andreas Binnewies on 3/23/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

extension UIFont {
    static func customFontOfSize(size: CGFloat) -> UIFont {
        return UIFont(name: CustomFont.currentFontName + "-Regular", size: size) ?? UIFont.systemFontOfSize(size)
    }

    static func boldCustomFontOfSize(size: CGFloat) -> UIFont {
        return UIFont(name: CustomFont.currentFontName + "-Bold", size: size) ?? UIFont.boldSystemFontOfSize(size)
    }
}