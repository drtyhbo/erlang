//
//  UIImage.swift
//  chat
//
//  Created by Andreas Binnewies on 2/15/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

extension UIImage {
    // Taken from: https://stackoverflow.com/questions/990976/how-to-create-a-colored-1x1-uiimage-on-the-iphone-dynamically
    static func imageWithColor(color: UIColor) -> UIImage {
        let rect = CGRectMake(0.0, 0.0, 1.0, 1.0)
        UIGraphicsBeginImageContext(rect.size)

        let context = UIGraphicsGetCurrentContext()
        CGContextSetFillColorWithColor(context, color.CGColor)
        CGContextFillRect(context, rect)

        let image = UIGraphicsGetImageFromCurrentImageContext()
        UIGraphicsEndImageContext()

        return image
    }

    func resizeToSize(size: CGSize) -> UIImage {
        UIGraphicsBeginImageContextWithOptions(size, false, 1)
        drawInRect(CGRect(origin: CGPoint(x: 0, y: 0), size: size))
        let newImage = UIGraphicsGetImageFromCurrentImageContext()
        UIGraphicsEndImageContext()

        return newImage
    }

    func resizeToPercentage(percentage: CGFloat) -> UIImage {
        return resizeToSize(CGSize(width: size.width * percentage, height: size.height * percentage))
    }

    func imageMaskedWithColor(color: UIColor) -> UIImage {
        let imageRect = CGRect(x: 0, y: 0, width: size.width, height: size.height)

        UIGraphicsBeginImageContextWithOptions(imageRect.size, false, scale)

        let context = UIGraphicsGetCurrentContext()

        CGContextScaleCTM(context, 1.0, -1.0)
        CGContextTranslateCTM(context, 0.0, -imageRect.size.height)

        if imageOrientation == .UpMirrored {
            CGContextScaleCTM(context, -1.0, 1.0)
            CGContextTranslateCTM(context, -imageRect.size.width, 0.0)
        }

        CGContextClipToMask(context, imageRect, CGImage)
        CGContextSetFillColorWithColor(context, color.CGColor)
        CGContextFillRect(context, imageRect)

        let newImage = UIGraphicsGetImageFromCurrentImageContext()
        UIGraphicsEndImageContext()
    
        return newImage
    }
}