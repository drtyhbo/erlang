//
//  ChatConnection.swift
//  chat
//
//  Created by Andreas Binnewies on 1/30/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import CocoaAsyncSocket
import Foundation
import SwiftyJSON

protocol ChatConnectionDelegate: class {
    func chatConnectionOnConnect(chatConnection: ChatConnection)
    func chatConnectionOnDisconnect(chatConnection: ChatConnection)
    func chatConnection(chatConnection: ChatConnection, didReceiveJSON json: JSON)
}

class ChatConnection {
    weak var delegate: ChatConnectionDelegate?

    var isConnected: Bool {
        return s != nil && s.connectedHost() != nil
    }

    private let host: String
    private let port: UInt16

    private(set) var isConnecting = false

    private var s: AsyncSocket!
    private var jsonToSend: [JSON] = []

    init(host: String, port: UInt16) {
        self.host = host
        self.port = port
    }

    func connect() {
        if isConnecting {
            return
        }

        isConnecting = true

        if s == nil {
            s = AsyncSocket(delegate: self)
        }

        do {
            try s.connectToHost(host, onPort: port)
        } catch (let e) {
            print ("Error connecting: \(e)")
            isConnecting = false
        }
    }

    func sendJson(json: JSON) {
        jsonToSend.append(json)
        if isConnected {
            sendQueuedJson()
        }
    }

    private func readData() {
        s.readDataToData("\n".dataUsingEncoding(NSUTF8StringEncoding), withTimeout: -1, tag: 0)
    }

    private func sendQueuedJson() {
        for json in jsonToSend {
            if let jsonString = json.rawString(NSUTF8StringEncoding, options: NSJSONWritingOptions(rawValue: 0)) {
                s.writeData(jsonString.dataUsingEncoding(NSUTF8StringEncoding), withTimeout: 0, tag: 0)
            }
        }
        jsonToSend = []
    }
}

extension ChatConnection: AsyncSocketDelegate {
    @objc func onSocket(sock: AsyncSocket!, didReadData data: NSData!, withTag tag: Int) {
        delegate?.chatConnection(self, didReceiveJSON: JSON(data: data))
        readData()
    }

    @objc func onSocketDidDisconnect(sock: AsyncSocket!) {
        isConnecting = false

        delegate?.chatConnectionOnDisconnect(self)
    }

    @objc func onSocket(sock: AsyncSocket!, didConnectToHost host: String!, port: UInt16) {
        isConnecting = false

        let options: [String: AnyObject] = [
            kCFStreamSSLValidatesCertificateChain as String: false
        ]
        sock.startTLS(options)
    }

    @objc func onSocketDidSecure(sock: AsyncSocket!) {
        delegate?.chatConnectionOnConnect(self)
        sendQueuedJson()
        readData()
    }
}