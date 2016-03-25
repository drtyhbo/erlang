//
//  ChatMessageManager.swift
//  chat
//
//  Created by Andreas Binnewies on 3/21/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import ChatCommon
import Foundation

protocol ChatMessageManagerDelegate: class {
    func chatMessageManager(chatMessageManager: ChatMessageManager, didPrependMessages messages: [Message])
    func chatMessageManager(chatMessageManager: ChatMessageManager, didAppendMessages messages: [Message])
}

class ChatMessageManager {
    private(set) var messages: [Message] = []
    private(set) var hasMoreMessages = true

    private let chat: Chat
    private let delegate: ChatMessageManagerDelegate

    init(chat: Chat, delegate: ChatMessageManagerDelegate) {
        self.chat = chat
        self.delegate = delegate
    }

    func loadMessages() {
        let quantityToFetch = Constants.fetchLimit + 1
        let newMessages = MessageManager.sharedManager.getMessagesForChat(chat, beforeDate: messages.count == 0 ? nil : messages[0].date, fetchLimit: quantityToFetch)
        hasMoreMessages = newMessages.count == quantityToFetch
        messages = newMessages[0..<min(newMessages.count, Constants.fetchLimit)] + messages

        delegate.chatMessageManager(self, didPrependMessages: newMessages)
    }

    func appendMessages(messages: [Message]) {
        self.messages += messages
        delegate.chatMessageManager(self, didAppendMessages: messages)
    }
}

