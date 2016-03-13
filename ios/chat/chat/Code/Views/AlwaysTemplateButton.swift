//
//  AlwaysTemplateButton.swift
//  chat
//
//  Created by Andreas Binnewies on 3/13/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

class AlwaysTemplateButton: UIButton {
    override func awakeFromNib() {
        super.awakeFromNib()
        setImage(imageForState(.Normal)?.imageWithRenderingMode(.AlwaysTemplate), forState: .Normal)
    }
}