//
//  Friend.swift
//  chat
//
//  Created by Andreas Binnewies on 2/1/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation

class Friend: Hashable {
    var hashValue: Int {
        return Int(id)!
    }

    private(set) var id: String
    private(set) var name: String

    init(id: String, name: String) {
        self.id = id
        self.name = name
    }
}

func ==(lhs: Friend, rhs: Friend) -> Bool {
    return lhs.id == rhs.id
}