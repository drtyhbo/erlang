//
//  GIFSelector.swift
//  chat
//
//  Created by Andreas Binnewies on 1/17/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Alamofire
import Foundation
import SwiftyJSON
import TWTToast
import UIKit

class MessageHelper: TWTNibBackedView {
    var searchQuery: String = ""
}

protocol GIFSelectorDelegate: class {
    func gifSelector(gifSelector: GIFSelector, didSelectImageWithURL url: NSURL, width: Int, height: Int)
}

class GIFSelector: MessageHelper {
    private class GIF {
        private let smallWidth: Int
        private let smallHeight: Int
        private let smallGIFURL: NSURL
        private let originalWidth: Int
        private let originalHeight: Int
        private let originalURL: NSURL

        init(smallWidth: Int, smallHeight: Int, smallGIFURL: NSURL, originalWidth: Int, originalHeight: Int, originalURL: NSURL) {
            self.smallWidth = smallWidth
            self.smallHeight = smallHeight
            self.smallGIFURL = smallGIFURL
            self.originalWidth = originalWidth
            self.originalHeight = originalHeight
            self.originalURL = originalURL
        }
    }

    @IBOutlet weak var collectionView: UICollectionView!

    weak var delegate: GIFSelectorDelegate?

    override var searchQuery: String {
        didSet {
            maybeSearch()
        }
    }

    private let gifSelectorCellReuseIdentifier = "GIFSelector"

    private var searchTimer: NSTimer?
    private var gifs: [GIF] = []

    override static func nibName() -> String {
        return "GIFSelector"
    }

    init() {
        super.init(frame: CGRectZero)
        collectionView.registerNib(UINib(nibName: "GIFSelectorCell", bundle: nil), forCellWithReuseIdentifier: gifSelectorCellReuseIdentifier)
    }

    required init?(coder aDecoder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private func maybeSearch() {
        searchTimer?.invalidate()
        searchTimer = NSTimer.scheduledTimerWithTimeInterval(0.5, target: self, selector: "search", userInfo: nil, repeats: false)
    }

    @objc private func search() {
        Alamofire.request(.GET, "https://api.giphy.com/v1/gifs/search", parameters: [
            "q": searchQuery,
            "api_key": "dc6zaTOxFJmzC"
        ]).responseJSON {
            response in
            guard let data = response.data else {
                return
            }

            self.gifs = []

            let json = JSON(data: data)
            if let gifsJson = json["data"].array {
                for gifJson in gifsJson {
                    let imagesJson = gifJson["images"]
                    let fixedHeightSmall = imagesJson["fixed_height_small"]
                    let original = imagesJson["original"]
                    self.gifs.append(GIF(
                        smallWidth: Int(fixedHeightSmall["width"].string!)!,
                        smallHeight: Int(fixedHeightSmall["height"].string!)!,
                        smallGIFURL: NSURL(string: fixedHeightSmall["url"].string!)!,
                        originalWidth: Int(original["width"].string!)!,
                        originalHeight: Int(original["height"].string!)!,
                        originalURL: NSURL(string: original["url"].string!)!))
                }
            }

            self.collectionView.reloadData()
        }
    }
}

extension GIFSelector: UICollectionViewDataSource {
    func numberOfSectionsInCollectionView(collectionView: UICollectionView) -> Int {
        return 1
    }

    func collectionView(collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        return gifs.count
    }

    func collectionView(collectionView: UICollectionView, cellForItemAtIndexPath indexPath: NSIndexPath) -> UICollectionViewCell {
        let cell = collectionView.dequeueReusableCellWithReuseIdentifier(gifSelectorCellReuseIdentifier, forIndexPath: indexPath) as! GIFSelectorCell
        cell.imageView.sd_setImageWithURL(gifs[indexPath.row].smallGIFURL)
        return cell
    }

    func collectionView(collectionView: UICollectionView, didSelectItemAtIndexPath indexPath: NSIndexPath) {
        let gif = gifs[indexPath.row]
        delegate?.gifSelector(self, didSelectImageWithURL: gif.originalURL, width: gif.originalWidth, height: gif.originalHeight)
    }
}

extension GIFSelector: UICollectionViewDelegateFlowLayout {
    func collectionView(collectionView: UICollectionView, layout collectionViewLayout: UICollectionViewLayout, sizeForItemAtIndexPath indexPath: NSIndexPath) -> CGSize {
        return CGSize(width: gifs[indexPath.row].smallWidth, height: gifs[indexPath.row].smallHeight)
    }

    func collectionView(collectionView: UICollectionView, layout collectionViewLayout: UICollectionViewLayout, minimumInteritemSpacingForSectionAtIndex section: Int) -> CGFloat {
        return 1
    }
}