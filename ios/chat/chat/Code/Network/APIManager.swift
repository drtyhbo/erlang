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
    static let webPort = Constants.webPort == "80" ? "" : ":\(Constants.webPort)"

    static func registerPhoneNumber(phoneNumber: String, callback: Bool->Void) {
        sendRequestToUrl("register/", parameters: [
            "phone": phoneNumber
        ]) {
            json in
            callback(errorFromJson(json) == nil)
        }
    }

    static func confirmPhoneNumber(phoneNumber: String, withCode code: String, key: String, callback: (String?, String?, Error?)->Void) {
        sendRequestToUrl("confirm/", parameters: [
            "phone": phoneNumber,
            "code": code,
            "key": key,
        ]) {
            json in
            callback(json?["id"].string, json?["sessionToken"].string, errorFromJson(json))
        }
    }

    struct FriendData {
        let id: Int
        let name: String
        let base64Key: String
    }

    static func getFriendsWithPhoneNumbers(phoneNumber: [String], callback: [FriendData]->Void) {
        sendUserRequestToUrl("friend/check/", parameters: [
            "phone": [phoneNumber],
        ]) {
            json in

            var friendsData: [FriendData] = []
            if let friendsJson = json?["friends"].array {
                for i in 0..<friendsJson.count {
                    if friendsJson[i].null != nil {
                        continue
                    }

                    let friendJson = friendsJson[i]
                    if let stringId = friendJson["id"].string, id = Int(stringId), name = friendJson["name"].string, base64Key = friendJson["key"].string {
                        friendsData.append(FriendData(id: id, name: name, base64Key: base64Key))
                    }
                }
            }
            callback(friendsData)
        }
    }

    static func registerDeviceToken(deviceToken: String, callback: Bool->Void) {
        sendUserRequestToUrl("pns/register/", parameters: [
            "token": deviceToken,
            "type": "ios"
        ]) {
            json in
            callback(json != nil && json!["status"].string == "ok")
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
        Alamofire.request(.POST, "http://\(domain)\(webPort)/api/\(url)", parameters: parameters)
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