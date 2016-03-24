//
//  ChatTableView.swift
//  chat
//
//  Created by Andreas Binnewies on 3/21/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import ChatCommon
import Foundation
import UIKit

enum ChatUIStyle {
    case Normal
    case Bubble
}

class ChatTableView: UITableView {
    private var chat: Chat!
    private var uiStyle: ChatUIStyle = .Normal

    func setupWithChat(chat: Chat, style: ChatUIStyle) {
        switch style {
        case .Normal:
            dataSource = NormalChatTableDataSource(chat: chat, tableView: self)
        case .Bubble:
            dataSource = BubbleChatTableDataSource(chat: chat, tableView: self)
        }
        loadMessages()
    }

    func appendMessages(messages: [Message]) {
        let previousRowCount = dataSource!.tableView(self, numberOfRowsInSection: 0)
        (dataSource as! ChatTableDataSource).appendMessages(messages)
        let newRowCount = dataSource!.tableView(self, numberOfRowsInSection: 0)

        UIView.performWithoutAnimation {
            let insertedRowCount = newRowCount - previousRowCount

            var indexPaths: [NSIndexPath] = []
            for i in 0..<insertedRowCount {
                indexPaths.append(NSIndexPath(forRow: newRowCount - 1 - i, inSection: 0))
            }

            self.insertRowsAtIndexPaths(indexPaths, withRowAnimation: .None)
            // Reload the previous row in case the way it looks depends on the new rows.
            self.reloadRowsAtIndexPaths([NSIndexPath(forRow: previousRowCount - 1, inSection: 0)], withRowAnimation: .None)
            self.scrollToRowAtIndexPath(NSIndexPath(forRow: newRowCount - 1, inSection: 0), atScrollPosition: .Bottom, animated: false)
        }

    }

    private func loadMessages() {
        let previousRowCount = dataSource!.tableView(self, numberOfRowsInSection: 0)
        (dataSource as? ChatTableDataSource)?.loadMessages()
        let insertedRowCount = dataSource!.tableView(self, numberOfRowsInSection: 0) - previousRowCount

        if previousRowCount == 0 || insertedRowCount > 0 {
            reloadData()
            layoutIfNeeded()

            if insertedRowCount > 0 {
                scrollToRowAtIndexPath(NSIndexPath(forRow: insertedRowCount - (previousRowCount == 0 ? 1 : 0), inSection: 0), atScrollPosition: previousRowCount == 0 ? .Bottom : .Top, animated: false)
            }
        }

    }
}

extension ChatTableView: UIScrollViewDelegate {
    func scrollViewDidEndDecelerating(scrollView: UIScrollView) {
        if scrollView.contentOffset.y < 200 {
            loadMessages()
        }
    }
}
