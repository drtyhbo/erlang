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
        guard let extensionContext = extensionContext else {
            return false
        }

        for item: NSExtensionItem in extensionContext.inputItems as? [NSExtensionItem] ?? [] {
            for attachment: NSItemProvider in item.attachments as? [NSItemProvider] ?? [] {
                if !attachment.hasItemConformingToTypeIdentifier(kUTTypeImage as String) &&
                    !attachment.hasItemConformingToTypeIdentifier(kUTTypeMovie as String) {
                    return false
                }
            }
        }

        return true
    }

    override func didSelectPost() {
        guard let extensionContext = extensionContext else {
            return
        }

        let friends = FriendManager.sharedManager.friends
        let chat = Chat.createWithParticipants([friends.first!])
        CoreData.save()

        let dispatchGroup = dispatch_group_create()

        for item: NSExtensionItem in extensionContext.inputItems as? [NSExtensionItem] ?? [] {
            for attachment: NSItemProvider in item.attachments as? [NSItemProvider] ?? [] {
                if attachment.hasItemConformingToTypeIdentifier(kUTTypeImage as String) {
                    shareImageFromAttachment(attachment, toChat: chat, dispatchGroup: dispatchGroup)
                } else if attachment.hasItemConformingToTypeIdentifier(kUTTypeMovie as String) {
                    shareMovieFromAttachment(attachment, toChat: chat, dispatchGroup: dispatchGroup)
                }
            }
        }

        dispatch_group_notify(dispatchGroup, dispatch_get_main_queue()) {
            extensionContext.completeRequestReturningItems([], completionHandler: nil)
        }
    }

    private func shareImageFromAttachment(attachment: NSItemProvider, toChat chat: Chat, dispatchGroup: dispatch_group_t) {
        dispatch_group_enter(dispatchGroup)
        attachment.loadItemForTypeIdentifier(kUTTypeImage as String, options: nil) { url, error in
            guard let url = url as? NSURL, imageData = NSData(contentsOfURL: url), image = UIImage(data: imageData) else {
                dispatch_group_leave(dispatchGroup)
                return
            }

            MessageManager.sharedManager.sendMessageWithImage(image, toChat: chat, callback: { message in
                dispatch_group_leave(dispatchGroup)
            })
        }
    }

    private func shareMovieFromAttachment(attachment: NSItemProvider, toChat chat: Chat, dispatchGroup: dispatch_group_t) {
        dispatch_group_enter(dispatchGroup)
        attachment.loadItemForTypeIdentifier(kUTTypeMovie as String, options: nil) { url, error in
            guard let url = url as? NSURL else {
                dispatch_group_leave(dispatchGroup)
                return
            }
 
            MessageManager.sharedManager.sendMessageWithMediaUrl(url, toChat: chat, callback: { message in
                dispatch_group_leave(dispatchGroup)
            })
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
