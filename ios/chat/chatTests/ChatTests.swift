//
//  ChatTests.swift
//  chatTests
//
//  Created by Andreas Binnewies on 3/5/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import XCTest

@testable import chat

class ChatTests: XCTestCase {
    private var friends: [Friend] = []

    override func setUp() {
        super.setUp()

        Chat.MR_truncateAll()

        for i in 0..<3 {
            var friend: Friend? = Friend.findWithId(i)
            if friend == nil {
                friend = Friend.createWithId(i, name: "TestUser\(i)")
            }
            friends.append(friend!)
        }

        Chat.createWithParticipants(friends)
        CoreData.save()
    }
    
    func testFindOneFriend() {
        let groupChat = Chat.findWithFriends([friends[0]])
        assert(groupChat == nil)
    }

    func testFindAllFriends() {
        let groupChat = Chat.findWithFriends(friends)
        assert(groupChat != nil)
    }
}
