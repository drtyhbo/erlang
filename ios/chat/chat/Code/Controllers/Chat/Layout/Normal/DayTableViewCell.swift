//
//  DayTableViewCell.swift
//  chat
//
//  Created by Andreas Binnewies on 2/20/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

class DayTableViewCell: UITableViewCell {
    @IBOutlet weak var dayLabel: UILabel!

    static let estimatedRowHeight: CGFloat = 60

    var date: NSDate = NSDate() {
        didSet {
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
    }
}
