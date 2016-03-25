//
//  LayoutPickerViewController.swift
//  chat
//
//  Created by Andreas Binnewies on 3/23/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

protocol LayoutPickerViewControllerDelegate: class {
    func layoutPickerViewController(layoutPickerViewController: LayoutPickerViewController, didSelectLayout layout: ChatLayout)
}

class LayoutPickerViewController: UIViewController {
    weak var delegate: LayoutPickerViewControllerDelegate?

    private let cellReuseIdentifier = "LayoutTableViewCell"
    private let layouts: [ChatLayout] = [
        .Normal,
        .Bubble]

    init() {
        super.init(nibName: "LayoutPickerViewController", bundle: nil)
    }

    required init?(coder aDecoder: NSCoder) {
        super.init(coder: aDecoder)
    }
}

extension LayoutPickerViewController: UITableViewDataSource, UITableViewDelegate {
    func numberOfSectionsInTableView(tableView: UITableView) -> Int {
        return 1
    }

    func tableView(tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return layouts.count
    }

    func tableView(tableView: UITableView, cellForRowAtIndexPath indexPath: NSIndexPath) -> UITableViewCell {
        let layout = layouts[indexPath.row]

        var tableViewCell: UITableViewCell! = tableView.dequeueReusableCellWithIdentifier(cellReuseIdentifier)
        if tableViewCell == nil {
            tableViewCell = UITableViewCell(style: .Default, reuseIdentifier: cellReuseIdentifier)
        }
        tableViewCell.textLabel?.text = layout.displayName
        tableViewCell.tintColor = ColorTheme.currentTheme.buttonColor
        tableViewCell.accessoryType = ChatLayout.currentLayout() == layout ? .Checkmark : .None

        return tableViewCell
    }

    func tableView(tableView: UITableView, didSelectRowAtIndexPath indexPath: NSIndexPath) {
        ChatLayout.setCurrentLayout(layouts[indexPath.row])
        delegate?.layoutPickerViewController(self, didSelectLayout: layouts[indexPath.row])
        navigationController?.popViewControllerAnimated(true)
    }
}
