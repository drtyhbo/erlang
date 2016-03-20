//
//  PreKey.swift
//  chat
//
//  Created by Andreas Binnewies on 3/18/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation

public class PreKey {
    let keyPair: KeyPair
    let index: Int

    init (keyPair: KeyPair, index: Int) {
        self.keyPair = keyPair
        self.index = index
    }
}