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

    private let messageRowCellReuseIdentifier = "BubbleMessageRowTableViewCell"
    private let mediaRowCellReuseIdentifier = "BubbleMediaRowTableViewCell"

    private var rows: [RowType] = []

    override init(chat: Chat, tableView: UITableView) {
        super.init(chat: chat, tableView: tableView)

        tableView.estimatedRowHeight = 100
        tableView.registerNib(UINib(nibName: "BubbleMessageRowTableViewCell", bundle: nil), forCellReuseIdentifier: messageRowCellReuseIdentifier)
        tableView.registerNib(UINib(nibName: "BubbleMediaRowTableViewCell", bundle: nil), forCellReuseIdentifier: mediaRowCellReuseIdentifier)
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

    private func priorMessageForRowAtIndex(var rowIndex: Int) -> Message? {
        rowIndex--

        while rowIndex >= 0 {
            switch (rows[rowIndex]) {
            case .MessageRow(let messageObject):
                return messageObject as? Message
            }
            rowIndex--
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
            let message = object as! Message

            var cell: BubbleTableViewCell
            if message.type == .Text {
                cell = tableView.dequeueReusableCellWithIdentifier(messageRowCellReuseIdentifier, forIndexPath: indexPath) as! BubbleMessageRowTableViewCell
            } else {
                cell = tableView.dequeueReusableCellWithIdentifier(mediaRowCellReuseIdentifier, forIndexPath: indexPath) as! BubbleMediaRowTableViewCell
            }
            cell.headerType = priorMessageForRowAtIndex(indexPath.row)?.from == message.from ? .Small : .Large
            cell.message = message

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