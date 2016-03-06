//
//  TintedImage.swift
//  chat
//
//  Created by Andreas Binnewies on 3/6/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

class TintedImage: UIImageView {
    override func awakeFromNib() {
        super.awakeFromNib()
        image = image?.imageWithRenderingMode(.AlwaysTemplate)
    }
}