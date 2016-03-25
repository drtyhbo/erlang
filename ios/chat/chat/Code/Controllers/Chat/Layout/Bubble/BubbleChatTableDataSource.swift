//
//  BubbleChatTableDataSource.swift
//  chat
//
//  Created by Andreas Binnewies on 3/21/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import ChatCommon
import Foundation

class BubbleChatTableDataSource: ChatTableDataSource {
    private enum RowType {
        case MessageRow(AnyObject)
    }

    private let leftMessageRowCellReuseIdentifier = "LeftBubbleMessageRowTableViewCell"
    private let rightMessageRowCellReuseIdentifier = "RightBubbleMessageRowTableViewCell"
    private let leftMediaRowCellReuseIdentifier = "LeftBubbleMediaRowTableViewCell"
    private let rightMediaRowCellReuseIdentifier = "RightBubbleMediaRowTableViewCell"

    private var rows: [RowType] = []

    override init(chat: Chat, tableView: UITableView) {
        super.init(chat: chat, tableView: tableView)

        tableView.estimatedRowHeight = 100
        tableView.registerNib(UINib(nibName: "LeftBubbleMessageRowTableViewCell", bundle: nil), forCellReuseIdentifier: leftMessageRowCellReuseIdentifier)
        tableView.registerNib(UINib(nibName: "RightBubbleMessageRowTableViewCell", bundle: nil), forCellReuseIdentifier: rightMessageRowCellReuseIdentifier)
        tableView.registerNib(UINib(nibName: "LeftBubbleMediaRowTableViewCell", bundle: nil), forCellReuseIdentifier: leftMediaRowCellReuseIdentifier)
        tableView.registerNib(UINib(nibName: "RightBubbleMediaRowTableViewCell", bundle: nil), forCellReuseIdentifier: rightMediaRowCellReuseIdentifier)
    }

    private func calculateRowsFromMessages(messages: [Message]) -> [RowType] {
        if messages.count == 0 {
            return []
        }

        var rows: [RowType] = []
        for i in 0..<messages.count {
            rows.append(.MessageRow(messages[i]))
        }

        return rows
    }

    private func priorMessageForRowAtIndex(rowIndex: Int) -> Message? {
        return nextMessageForRowAtIndex(rowIndex, byIncrementing: -1)
    }

    private func nextMessageForRowAtIndex(rowIndex: Int) -> Message? {
        return nextMessageForRowAtIndex(rowIndex, byIncrementing: 1)
    }

    private func nextMessageForRowAtIndex(var rowIndex: Int, byIncrementing increment: Int) -> Message? {
        rowIndex += increment

        while rowIndex >= 0 && rowIndex < rows.count {
            switch (rows[rowIndex]) {
            case .MessageRow(let messageObject):
                return messageObject as? Message
            }
            rowIndex += increment
        }

        return nil
    }
}

extension BubbleChatTableDataSource: UITableViewDataSource {
    func numberOfSectionsInTableView(tableView: UITableView) -> Int {
        return 1
    }

    func tableView(tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return rows.count
    }

    func tableView(tableView: UITableView, cellForRowAtIndexPath indexPath: NSIndexPath) -> UITableViewCell {
        let row = rows[indexPath.row]

        switch(row) {
        case .MessageRow(let object):
            let nextMessage = nextMessageForRowAtIndex(indexPath.row)
            let message = object as! Message
            let isFromCurrentUser = message.from == nil

            var cell: BubbleTableViewCell
            if message.type == .Text {
                cell = tableView.dequeueReusableCellWithIdentifier(isFromCurrentUser ? rightMessageRowCellReuseIdentifier : leftMessageRowCellReuseIdentifier, forIndexPath: indexPath) as! BubbleMessageRowTableViewCell
            } else {
                cell = tableView.dequeueReusableCellWithIdentifier(isFromCurrentUser ?rightMediaRowCellReuseIdentifier : leftMediaRowCellReuseIdentifier, forIndexPath: indexPath) as! BubbleMediaRowTableViewCell
            }
            cell.footerType = nextMessage != nil && nextMessage!.from == message.from ? .Small : .Large
            cell.updateWithMessage(message, hasTail: nextMessage == nil || nextMessage?.from != message.from)

            return cell
        }
    }

    func tableView(tableView: UITableView, estimatedHeightForRowAtIndexPath indexPath: NSIndexPath) -> CGFloat {
        switch(rows[indexPath.row]) {
        case .MessageRow(let object):
            let message = object as! Message
            if message.type == .Text {
                return BubbleMessageRowTableViewCell.estimatedHeightForMessage(message)
            } else {
                return BubbleMediaRowTableViewCell.estimatedHeightForMessage(message)
            }
        }
    }
}

extension BubbleChatTableDataSource: ChatMessageManagerDelegate {
    func chatMessageManager(chatMessageManager: ChatMessageManager, didAppendMessages messages: [Message]) {
        rows += calculateRowsFromMessages(messages)
    }

    func chatMessageManager(chatMessageManager: ChatMessageManager, didPrependMessages messages: [Message]) {
        rows = calculateRowsFromMessages(messages) + rows
    }
}