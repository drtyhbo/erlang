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
        case Message(AnyObject, AnyObject?)
        case Date(NSDate)
        case NewMessages
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

    private func calculateRowsFromMessages(messages: [Message]) -> [RowType] {
        var rows: [RowType] = []

        if messages.count == 0 {
            return rows
        }

        guard let gregorian = NSCalendar(calendarIdentifier: NSCalendarIdentifierGregorian) else {
            return rows
        }

        var previousDayOfYear = gregorian.ordinalityOfUnit(.Day, inUnit: .Year, forDate: messages[0].date)
        rows = [.Message(messages[0], nil)]
        for i in 1..<messages.count {
            let message = messages[i]

            let dayOfYear = gregorian.ordinalityOfUnit(.Day, inUnit: .Year, forDate: message.date)
            if dayOfYear != previousDayOfYear {
                rows.append(.Date(message.date))
            }
            previousDayOfYear = dayOfYear

            rows.append(.Message(message, messages[i - 1]))
        }

        if !messageManager.hasMoreMessages {
            rows.insert(.ConversationStart, atIndex: 0)
        }

/*        if unreadMessageCount > 0 {
            rows.insert(.NewMessages, atIndex: rows.count - unreadMessageCount)
        }*/

        return rows
    }

    private func headerTypeForRowAtIndexPath(indexPath: NSIndexPath) -> MessageTableViewCell.HeaderType {
        if indexPath.row == 0 {
            return .Full
        }

        var previousRowIsMedia = false

        switch(rows[indexPath.row - 1]) {
            case .ConversationStart:
                return .FullNoPadding
            case .Date(_):
                return .FullNoPadding
            case .NewMessages:
                return .FullNoPadding
            case .Message(let object, _):
                let message = object as! Message
                previousRowIsMedia = message.thumbnailInfo != nil
        }

        let row = rows[indexPath.row]
        switch(row) {
            case .Message(let messageObject, let previousMessageObject):
                let message = messageObject as! Message
                let previousMessage = previousMessageObject as? Message
                if message.thumbnailInfo != nil && previousRowIsMedia {
                    return .NoPadding
                } else if let previousMessage = previousMessage {
                    return (message.date.timeIntervalSinceDate(previousMessage.date) > 600 || message.from != previousMessage.from) ? .Full : .PaddingOnly
                } else {
                    return previousMessage == nil && !messageManager.hasMoreMessages ? .Full : .PaddingOnly
                }
            default:
                return .PaddingOnly
        }
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
        case .Message(let object, _):
            let message = object as! Message

            var cell: MessageTableViewCell
            if message.type == .Text {
                cell = tableView.dequeueReusableCellWithIdentifier(chatRowCellReuseIdentifier, forIndexPath: indexPath) as! MessageTableViewCell
            } else {
                cell = tableView.dequeueReusableCellWithIdentifier(mediaRowCellReuseIdentifier, forIndexPath: indexPath) as! MediaRowTableViewCell
            }

            cell.message = message
            cell.headerType = headerTypeForRowAtIndexPath(indexPath)

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
        case .Message(let object, _):
            let message = object as! Message
            if message.type == .Text {
                return ChatRowTableViewCell.estimatedHeightForMessage(message, headerType: headerTypeForRowAtIndexPath(indexPath))

            } else {
                return MediaRowTableViewCell.estimatedHeightForMessage(message, headerType: headerTypeForRowAtIndexPath(indexPath))
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
        rows += calculateRowsFromMessages(messages)
    }

    func chatMessageManager(chatMessageManager: ChatMessageManager, didPrependMessages messages: [Message]) {
        rows = calculateRowsFromMessages(messages) + rows
    }
}