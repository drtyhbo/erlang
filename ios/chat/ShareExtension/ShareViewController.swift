//
//  ShareViewController.swift
//  ShareExtension
//
//  Created by Andreas Binnewies on 3/18/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import ChatCommon
import MobileCoreServices
import UIKit
import Social

class ShareViewController: SLComposeServiceViewController {

    required init?(coder aDecoder: NSCoder) {
        super.init(coder: aDecoder)
        CoreData.setup()
    }

    override func isContentValid() -> Bool {
        return true
    }

    override func didSelectPost() {
        let friends = FriendManager.sharedManager.friends
        let chat = Chat.createWithParticipants([friends.first!])
        CoreData.save()

        if let extensionContext = extensionContext {
            for item: NSExtensionItem in extensionContext.inputItems as? [NSExtensionItem] ?? [] {
                for attachment: NSItemProvider in item.attachments as? [NSItemProvider] ?? [] {
                    if !attachment.hasItemConformingToTypeIdentifier(kUTTypeImage as String) {
                        continue
                    }
                    attachment.loadItemForTypeIdentifier(kUTTypeImage as String, options: nil) { url, error in
                        guard let url = url as? NSURL, imageData = NSData(contentsOfURL: url), image = UIImage(data: imageData) else {
                            return
                        }

                        MessageManager.sharedManager.sendMessageWithImage(image, toChat: chat, callback: { message in
                            self.extensionContext!.completeRequestReturningItems([], completionHandler: nil)
                        })
                    }
                }
            }
        }
    }

    override func configurationItems() -> [AnyObject]! {
        let friends = FriendManager.sharedManager.friends

        let shareWithConfigurationItem = SLComposeSheetConfigurationItem()
        shareWithConfigurationItem.title = "Share with..."
        shareWithConfigurationItem.value = friends.first?.fullName ?? "nil"

        return [shareWithConfigurationItem]
    }

}
