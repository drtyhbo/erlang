//
//  ChatProfilePic.swift
//  chat
//
//  Created by Andreas Binnewies on 2/15/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import ChatCommon
import Foundation
import UIKit

class ChatProfilePic: UIImageView {
    private struct ProfilePicMask {
        let name: String
        let imageName: String?
    }

    private let masks: [ProfilePicMask] = [
        ProfilePicMask(name: "none", imageName: nil),
        ProfilePicMask(name: "heart", imageName: "Heart"),
        ProfilePicMask(name: "star", imageName: "Star"),
        ProfilePicMask(name: "badge", imageName: "Badge")]

    var friend: Friend? {
        didSet {
            if let url = friend?.profilePicUrl {
                sd_setImageWithURL(url)
            } else if let profilePic = User.profilePic where friend == nil {
                image = profilePic
            } else {
                image = UIImage(named: "ProfilePic")
            }

            currentMaskIndex = 0
            let maskName = NSUserDefaults.standardUserDefaults().stringForKey(maskKey) ?? "none"
            for i in 0..<masks.count {
                if masks[i].name == maskName {
                    currentMaskIndex = i
                    updateMask(masks[i])
                    break
                }
            }
        }
    }

    private var currentMaskIndex: Int = 0
    private var maskKey: String {
        if friend == nil {
            return "mask:me"
        } else if let id = friend?.id {
            return "mask:\(id)"
        } else {
            return "mask:error"
        }
    }

    override func awakeFromNib() {
        super.awakeFromNib()

        contentMode = .ScaleAspectFill
        layer.cornerRadius = 5
        clipsToBounds = true
        userInteractionEnabled = true

        addGestureRecognizer(UITapGestureRecognizer(target: self, action: "didTap"))
    }

    private func updateMask(mask: ProfilePicMask) {
        if let maskImageName = mask.imageName {
            let maskLayer = CALayer()
            maskLayer.contents = UIImage(named: maskImageName)!.CGImage
            maskLayer.frame = bounds
            layer.mask = maskLayer
            layer.masksToBounds = true
        } else {
            layer.mask = nil
        }
    }

    @objc private func didTap() {
        currentMaskIndex = (currentMaskIndex + 1) % masks.count

        updateMask(masks[currentMaskIndex])

        NSUserDefaults.standardUserDefaults().setObject(masks[currentMaskIndex].name, forKey: maskKey)
        NSUserDefaults.standardUserDefaults().synchronize()
    }
}