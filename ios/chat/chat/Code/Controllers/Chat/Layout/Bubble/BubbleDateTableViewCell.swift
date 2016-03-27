//
//  BubbleDateTableViewCell.swift
//  chat
//
//  Created by Andreas Binnewies on 3/26/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

class BubbleDateTableViewCell: UITableViewCell {
    @IBOutlet weak var dayLabel: UILabel!
    @IBOutlet weak var timeLabel: UILabel!

    static let rowHeight: CGFloat = 50

    var date: NSDate = NSDate() {
        didSet {
            updateDate(date)
            updateTimeFromDate(date)
        }
    }

    private func updateDate(date: NSDate) {
        guard let gregorian = NSCalendar(calendarIdentifier: NSCalendarIdentifierGregorian) else {
            return
        }

        let components = gregorian.components(.Day, fromDate: date, toDate: NSDate(), options: NSCalendarOptions(rawValue: 0))
        if components.day == 0 {
            dayLabel.text = "Today"
        } else if components.day == 1 {
            dayLabel.text = "Yesterday"
        } else {
            let dateFormatter = NSDateFormatter()
            dateFormatter.dateStyle = .MediumStyle
            dayLabel.text = dateFormatter.stringFromDate(date)
        }
    }

    private func updateTimeFromDate(date: NSDate) {
        let timeFormatter = NSDateFormatter()
        timeFormatter.timeStyle = .ShortStyle
        timeLabel.text = timeFormatter.stringFromDate(date)
    }
}