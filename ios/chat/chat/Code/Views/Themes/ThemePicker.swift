//
//  ThemePicker.swift
//  chat
//
//  Created by Andreas Binnewies on 3/15/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

class ThemePicker: UICollectionView {
    private let themeCellReuseIdentifier = "ThemeCollectionViewCell"

    required init?(coder aDecoder: NSCoder) {
        super.init(coder: aDecoder)

        delegate = self
        dataSource = self

        registerNib(UINib(nibName: "ThemeCollectionViewCell", bundle: nil), forCellWithReuseIdentifier: themeCellReuseIdentifier)

        selectItemAtIndexPath(NSIndexPath(forRow: ColorTheme.currentThemeType.rawValue, inSection: 0), animated: false, scrollPosition: .None)
    }
}

extension ThemePicker: UICollectionViewDelegate, UICollectionViewDataSource, UICollectionViewDelegateFlowLayout {
    func collectionView(collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        return Constants.themes.count
    }

    func collectionView(collectionView: UICollectionView, cellForItemAtIndexPath indexPath: NSIndexPath) -> UICollectionViewCell {
        let cell = collectionView.dequeueReusableCellWithReuseIdentifier(themeCellReuseIdentifier, forIndexPath: indexPath) as! ThemeCollectionViewCell
        cell.themeColor = Constants.themes[indexPath.row].buttonColor
        return cell
    }

    func collectionView(collectionView: UICollectionView, didSelectItemAtIndexPath indexPath: NSIndexPath) {
        ColorTheme.currentThemeType = ColorTheme.ThemeType(rawValue: indexPath.row)!
    }

    func collectionView(collectionView: UICollectionView, layout collectionViewLayout: UICollectionViewLayout, sizeForItemAtIndexPath indexPath: NSIndexPath) -> CGSize {
        return CGSize(width: 40, height: 40)
    }

    func collectionView(collectionView: UICollectionView, layout collectionViewLayout: UICollectionViewLayout, minimumInteritemSpacingForSectionAtIndex section: Int) -> CGFloat {
        return 0
    }
}