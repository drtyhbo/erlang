//
//  ChatTableDataSource.swift
//  chat
//
//  Created by Andreas Binnewies on 3/21/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import ChatCommon
import Foundation
import UIKit

class ChatTableDataSource: NSObject {
    let chat: Chat
    private(set) var messageManager: ChatMessageManager!

    init(chat: Chat, tableView: UITableView) {
        self.chat = chat

        super.init()

        messageManager = ChatMessageManager(chat: chat, delegate: self as! ChatMessageManagerDelegate)
        tableView.dataSource = self as? UITableViewDataSource
    }

    func appendMessages(messages: [Message]) {
        messageManager.appendMessages(messages)
    }

    func loadMessages() {
        messageManager.loadMessages()
    }

    func heightForRowAtIndexPath(indexPath: NSIndexPath) -> CGFloat {
        return 0
    }
}