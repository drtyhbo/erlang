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
        return CustomFont.fontNames.count
    }

    func tableView(tableView: UITableView, cellForRowAtIndexPath indexPath: NSIndexPath) -> UITableViewCell {
        var tableViewCell = tableView.dequeueReusableCellWithIdentifier(cellReuseIdentifier)
        if tableViewCell == nil {
            tableViewCell = UITableViewCell(style: .Default, reuseIdentifier: cellReuseIdentifier)
        }
        tableViewCell!.textLabel?.font = UIFont(name: CustomFont.fontNames[indexPath.row].fontName + "-Regular", size: 16)
        tableViewCell!.textLabel?.text = CustomFont.fontNames[indexPath.row].displayName

        return tableViewCell!
    }

    func tableView(tableView: UITableView, didSelectRowAtIndexPath indexPath: NSIndexPath) {
        CustomFont.currentFontName = CustomFont.fontNames[indexPath.row].fontName
        delegate?.fontPickerViewController(self, didSelectCustomFont: CustomFont.fontNames[indexPath.row])
        navigationController?.popViewControllerAnimated(true)
    }
}
