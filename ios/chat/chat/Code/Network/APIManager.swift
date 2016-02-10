//
//  APIManager.swift
//  chat
//
//  Created by Andreas Binnewies on 1/30/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Alamofire
import Foundation
import SwiftyJSON

class APIManager {
    class Error {
        let error: String

        init(_ error: String) {
            self.error = error
        }
    }

    static let domain = Constants.host

    static func registerPhoneNumber(phoneNumber: String, callback: Bool->Void) {
        sendRequestToUrl("register/", parameters: [
            "phone": phoneNumber
        ]) {
            json in
            callback(errorFromJson(json) == nil)
        }
    }

    static func confirmPhoneNumber(phoneNumber: String, withCode code: String, callback: (String?, String?, Error?)->Void) {
        sendRequestToUrl("confirm/", parameters: [
            "phone": phoneNumber,
            "code": code
        ]) {
            json in
            callback(json?["id"].string, json?["sessionToken"].string, errorFromJson(json))
        }
    }

    struct FriendData {
        let id: Int
        let name: String
    }

    static func getFriendsWithPhoneNumbers(phoneNumber: [String], callback: [FriendData]->Void) {
        sendUserRequestToUrl("friend/check/", parameters: [
            "phone": [phoneNumber],
        ]) {
            json in

            var friendsData: [FriendData] = []
            if let friendsJson = json?["friends"].array {
                for i in 0..<friendsJson.count {
                    if friendsJson[i].null == nil {
                        let friendJson = friendsJson[i]
                        friendsData.append(FriendData(id: Int(friendJson["id"].string!)!, name: friendJson["name"].string!))
                    }
                }
            }
            callback(friendsData)
        }
    }

    private static func errorFromJson(json: JSON?) -> Error? {
        if let json = json {
            return json["status"] == "ok" ? nil : Error(json["status"].string ?? "invalid")
        } else {
            return Error("invalid")
        }
    }

    private static func sendRequestToUrl(url: String, parameters: [String:AnyObject], callback: JSON?->Void) {
        Alamofire.request(.POST, "http://\(domain)/api/\(url)", parameters: parameters)
            .responseJSON {
                response in
                if let json = response.result.value {
                    callback(JSON(json))
                } else {
                    callback(nil)
                }
            }
    }

    private static func sendUserRequestToUrl(url: String, callback: JSON?->Void) {
        sendUserRequestToUrl(url, parameters: [:], callback: callback)
    }

    private static func sendUserRequestToUrl(url: String, var parameters: [String:AnyObject], callback: JSON?->Void) {
        parameters["id"] = User.userId
        parameters["session"] = User.sessionToken

        sendRequestToUrl("user/\(url)", parameters: parameters, callback: callback)
    }
}