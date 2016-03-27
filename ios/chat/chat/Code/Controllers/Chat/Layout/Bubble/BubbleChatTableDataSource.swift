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
        case DateRow(NSDate)
        case MessageRow(Message)
    }

    private let leftMessageRowCellReuseIdentifier = "LeftBubbleMessageRowTableViewCell"
    private let rightMessageRowCellReuseIdentifier = "RightBubbleMessageRowTableViewCell"
    private let leftMediaRowCellReuseIdentifier = "LeftBubbleMediaRowTableViewCell"
    private let rightMediaRowCellReuseIdentifier = "RightBubbleMediaRowTableViewCell"
    private let dateCellReuseIdentifier = "DateTableViewCell"

    private var rows: [RowType] = []

    override init(chat: Chat, tableView: UITableView) {
        super.init(chat: chat, tableView: tableView)

        tableView.registerNib(UINib(nibName: "LeftBubbleMessageRowTableViewCell", bundle: nil), forCellReuseIdentifier: leftMessageRowCellReuseIdentifier)
        tableView.registerNib(UINib(nibName: "RightBubbleMessageRowTableViewCell", bundle: nil), forCellReuseIdentifier: rightMessageRowCellReuseIdentifier)
        tableView.registerNib(UINib(nibName: "LeftBubbleMediaRowTableViewCell", bundle: nil), forCellReuseIdentifier: leftMediaRowCellReuseIdentifier)
        tableView.registerNib(UINib(nibName: "RightBubbleMediaRowTableViewCell", bundle: nil), forCellReuseIdentifier: rightMediaRowCellReuseIdentifier)
        tableView.registerNib(UINib(nibName: "BubbleDateTableViewCell", bundle: nil), forCellReuseIdentifier: dateCellReuseIdentifier)
    }

    override func heightForRowAtIndexPath(indexPath: NSIndexPath) -> CGFloat {
        let nextMessage = nextMessageForRowAtIndex(indexPath.row)
        switch(rows[indexPath.row]) {
        case .DateRow(_):
            return BubbleDateTableViewCell.rowHeight
        case .MessageRow(let message):
            let footerType: BubbleMessageRowTableViewCell.FooterType = nextMessage != nil && nextMessage!.from == message.from ? .Small : .Large
            if message.type == .Text {
                return BubbleMessageRowTableViewCell.heightForMessage(message, footerType: footerType)
            } else {
                return BubbleMediaRowTableViewCell.heightForMessage(message, footerType: footerType)
            }
        }
    }

    private func calculateRowsFromMessages(messages: [Message], var previousMessage: Message?) -> [RowType] {
        if messages.count == 0 {
            return []
        }

        var rows: [RowType] = []
        for i in 0..<messages.count {
            let message = messages[i]
            if let previousMessage = previousMessage where message.date.timeIntervalSinceDate(previousMessage.date) >= Constants.BubbleLayout.timeIntervalBetweenDateRows {
                rows.append(.DateRow(message.date))
            }
            rows.append(.MessageRow(messages[i]))

            previousMessage = message
        }

        return rows
    }

    private func lastMessage() -> Message? {
        return nextMessageForRowAtIndex(rows.count, byIncrementing: -1)
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
            case .MessageRow(let message):
                return message
            default:
                break
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
        case .DateRow(let date):
            let cell = tableView.dequeueReusableCellWithIdentifier(dateCellReuseIdentifier, forIndexPath: indexPath) as! BubbleDateTableViewCell
            cell.date = date
            return cell
        case .MessageRow(let message):
            let nextMessage = nextMessageForRowAtIndex(indexPath.row)
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
}

extension BubbleChatTableDataSource: ChatMessageManagerDelegate {
    func chatMessageManager(chatMessageManager: ChatMessageManager, didAppendMessages messages: [Message]) {
        rows += calculateRowsFromMessages(messages, previousMessage: lastMessage())
    }

    func chatMessageManager(chatMessageManager: ChatMessageManager, didPrependMessages messages: [Message]) {
        rows = calculateRowsFromMessages(messages, previousMessage: nil) + rows
    }
}