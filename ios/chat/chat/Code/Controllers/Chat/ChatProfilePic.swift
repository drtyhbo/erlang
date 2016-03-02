//
//  ChatProfilePic.swift
//  chat
//
//  Created by Andreas Binnewies on 2/15/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

class ChatProfilePic: UIImageView {
    override func awakeFromNib() {
        super.awakeFromNib()
        contentMode = .ScaleAspectFill
        layer.cornerRadius = 5
        clipsToBounds = true
    }
}