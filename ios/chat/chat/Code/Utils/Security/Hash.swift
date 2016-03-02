//
//  Hash.swift
//  chat
//
//  Created by Andreas Binnewies on 3/1/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import Sodium

class Hash {
    private let HashBytes = Int(crypto_generichash_bytes())

    private var genericHashState: crypto_generichash_state = crypto_generichash_state()

    init() {
        crypto_generichash_init(&genericHashState, nil, 0, HashBytes)
    }

    func update(data: NSData) {
        update([data])
    }

    func update(datas: [NSData]) {
        for data in datas {
            crypto_generichash_update(&genericHashState, data.UInt8Bytes, UInt64(data.length))
        }
    }

    func final() -> NSData? {
        guard let hash = NSMutableData(length: HashBytes) else {
            return nil
        }

        crypto_generichash_final(&genericHashState, hash.mutableUInt8Bytes, hash.length)

        return hash
    }
}