//
//  FontPickerViewController.swift
//  chat
//
//  Created by Andreas Binnewies on 3/23/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

protocol FontPickerViewControllerDelegate: class {
    func fontPickerViewController(fontPickerViewController: FontPickerViewController, didSelectCustomFont customFont: CustomFont)
}

class FontPickerViewController: UIViewController {
    weak var delegate: FontPickerViewControllerDelegate?

    private let cellReuseIdentifier = "FontTableViewCell"

    init() {
        super.init(nibName: "FontPickerViewController", bundle: nil)
    }

    required init?(coder aDecoder: NSCoder) {
        super.init(coder: aDecoder)
    }
}

extension FontPickerViewController: UITableViewDataSource, UITableViewDelegate {
    func numberOfSectionsInTableView(tableView: UITableView) -> Int {
        return 1
    }

    func tableView(tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return CustomFont.fonts.count
    }

    func tableView(tableView: UITableView, cellForRowAtIndexPath indexPath: NSIndexPath) -> UITableViewCell {
        let font = CustomFont.fonts[indexPath.row]

        var tableViewCell: UITableViewCell! = tableView.dequeueReusableCellWithIdentifier(cellReuseIdentifier)
        if tableViewCell == nil {
            tableViewCell = UITableViewCell(style: .Default, reuseIdentifier: cellReuseIdentifier)
        }
        tableViewCell.textLabel?.font = UIFont(name: font.fontName + "-Regular", size: 16)
        tableViewCell.textLabel?.text = font.displayName
        tableViewCell.tintColor = ColorTheme.currentTheme.buttonColor
        tableViewCell.accessoryType = CustomFont.currentFont == font ? .Checkmark : .None

        return tableViewCell
    }

    func tableView(tableView: UITableView, didSelectRowAtIndexPath indexPath: NSIndexPath) {
        CustomFont.currentFont = CustomFont.fonts[indexPath.row]
        delegate?.fontPickerViewController(self, didSelectCustomFont: CustomFont.fonts[indexPath.row])
        navigationController?.popViewControllerAnimated(true)
    }
}
