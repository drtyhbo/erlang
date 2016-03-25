//
//  NormalChatTableDataSource.swift
//  chat
//
//  Created by Andreas Binnewies on 3/21/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import ChatCommon
import Foundation

class NormalChatTableDataSource: ChatTableDataSource {
    private enum RowType {
        case ConversationStart
        case MessageRow(Message, MessageTableViewCell.HeaderType)
        case Date(NSDate)
        case NewMessages

        func headerTypeForMessage(message: Message) -> MessageTableViewCell.HeaderType {
            switch(self) {
                case .MessageRow(let previousMessage, _):
                    if message.thumbnailInfo != nil && previousMessage.thumbnailInfo != nil {
                        return .NoPadding
                    } else {
                        return (message.date.timeIntervalSinceDate(previousMessage.date) > 600 || message.from != previousMessage.from) ? .Full : .PaddingOnly
                    }
                default:
                    return .FullNoPadding
            }
        }
    }

    private let chatRowCellReuseIdentifier = "ChatRowTableViewCell"
    private let mediaRowCellReuseIdentifier = "MediaRowTableViewCell"
    private let dayCellReuseIdentifier = "DayTableViewCell"
    private let newMessagesCellReuseIdentifier = "NewMessagesTableViewCell"
    private let conversationStartCellReuseIdentifier = "ConversationStartTableViewCell"

    private var rows: [RowType] = []

    override init(chat: Chat, tableView: UITableView) {
        super.init(chat: chat, tableView: tableView)


        tableView.estimatedRowHeight = 100
        tableView.registerNib(UINib(nibName: "ChatRowTableViewCell", bundle: nil), forCellReuseIdentifier: chatRowCellReuseIdentifier)
        tableView.registerNib(UINib(nibName: "NewMessagesCell", bundle: nil), forCellReuseIdentifier: newMessagesCellReuseIdentifier)
        tableView.registerNib(UINib(nibName: "MediaRowTableViewCell", bundle: nil), forCellReuseIdentifier: mediaRowCellReuseIdentifier)
        tableView.registerNib(UINib(nibName: "DayTableViewCell", bundle: nil), forCellReuseIdentifier: dayCellReuseIdentifier)
        tableView.registerNib(UINib(nibName: "NewMessagesTableViewCell", bundle: nil), forCellReuseIdentifier: newMessagesCellReuseIdentifier)
        tableView.registerNib(UINib(nibName: "ConversationStartTableViewCell", bundle: nil), forCellReuseIdentifier: conversationStartCellReuseIdentifier)
    }

    private func calculateRowsFromMessages(messages: [Message], previousRow: RowType?) -> [RowType] {
        var rows: [RowType] = []

        if messages.count == 0 {
            return rows
        }

        guard let gregorian = NSCalendar(calendarIdentifier: NSCalendarIdentifierGregorian) else {
            return rows
        }

        var previousDayOfYear = gregorian.ordinalityOfUnit(.Day, inUnit: .Year, forDate: messages[0].date)
        rows = [.MessageRow(messages[0], previousRow?.headerTypeForMessage(messages[0]) ?? .Full)]
        for i in 1..<messages.count {
            let message = messages[i]

            let dayOfYear = gregorian.ordinalityOfUnit(.Day, inUnit: .Year, forDate: message.date)
            if dayOfYear != previousDayOfYear {
                rows.append(.Date(message.date))
                previousDayOfYear = dayOfYear
            }

            rows.append(.MessageRow(message, rows.last!.headerTypeForMessage(message)))
        }

/*        if unreadMessageCount > 0 {
            rows.insert(.NewMessages, atIndex: rows.count - unreadMessageCount)
        }*/

        return rows
    }
}

extension NormalChatTableDataSource: UITableViewDataSource {
    func numberOfSectionsInTableView(tableView: UITableView) -> Int {
        return 1
    }

    func tableView(tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return rows.count
    }

    func tableView(tableView: UITableView, cellForRowAtIndexPath indexPath: NSIndexPath) -> UITableViewCell {
        let row = rows[indexPath.row]

        switch(row) {
        case .ConversationStart:
            let cell = tableView.dequeueReusableCellWithIdentifier(conversationStartCellReuseIdentifier, forIndexPath: indexPath) as! ConversationStartTableViewCell
            cell.friend = chat.participantsArray[0]
            return cell
        case .MessageRow(let message, let headerType):

            var cell: MessageTableViewCell
            if message.type == .Text {
                cell = tableView.dequeueReusableCellWithIdentifier(chatRowCellReuseIdentifier, forIndexPath: indexPath) as! MessageTableViewCell
            } else {
                cell = tableView.dequeueReusableCellWithIdentifier(mediaRowCellReuseIdentifier, forIndexPath: indexPath) as! MediaRowTableViewCell
            }

            cell.message = message
            cell.headerType = headerType

            return cell
        case .Date(let date):
            let cell = tableView.dequeueReusableCellWithIdentifier(dayCellReuseIdentifier, forIndexPath: indexPath) as! DayTableViewCell
            cell.date = date
            return cell
        case .NewMessages:
            return tableView.dequeueReusableCellWithIdentifier(newMessagesCellReuseIdentifier, forIndexPath: indexPath)
        }
    }

    func tableView(tableView: UITableView, estimatedHeightForRowAtIndexPath indexPath: NSIndexPath) -> CGFloat {
        switch(rows[indexPath.row]) {
        case .ConversationStart:
            return ConversationStartTableViewCell.rowHeight
        case .MessageRow(let message, let headerType):
            if message.type == .Text {
                return ChatRowTableViewCell.estimatedHeightForMessage(message, headerType: headerType)

            } else {
                return MediaRowTableViewCell.estimatedHeightForMessage(message, headerType: headerType)
            }
        case .Date(_):
            return DayTableViewCell.estimatedRowHeight
        case .NewMessages:
            return NewMessagesTableViewCell.rowHeight
        }
    }

    func tableView(tableView: UITableView, heightForRowAtIndexPath indexPath: NSIndexPath) -> CGFloat {
        switch(rows[indexPath.row]) {
        case .ConversationStart:
            return ConversationStartTableViewCell.rowHeight
        case .NewMessages:
            return NewMessagesTableViewCell.rowHeight
        default:
            return UITableViewAutomaticDimension
        }
    }
}

extension NormalChatTableDataSource: ChatMessageManagerDelegate {
    func chatMessageManager(chatMessageManager: ChatMessageManager, didAppendMessages messages: [Message]) {
        rows += calculateRowsFromMessages(messages, previousRow: rows.last)
    }

    func chatMessageManager(chatMessageManager: ChatMessageManager, didPrependMessages messages: [Message]) {
        if messages.count > 0 {
            rows = calculateRowsFromMessages(messages, previousRow: nil) + rows
            if !messageManager.hasMoreMessages {
                rows.insert(.ConversationStart, atIndex: 0)
            }
        }
    }
}