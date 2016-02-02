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

    static let domain = "http://192.168.1.109:3000"

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

    static func addFriendWithPhoneNumber(phoneNumber: String, callback: Bool->Void) {
        sendUserRequestToUrl("friend/add/", parameters: [
            "phone": phoneNumber,
        ]) {
            json in
            callback(errorFromJson(json) == nil)
        }
    }

    static func usersExistForPhoneNumbers(phoneNumber: [String], callback: [Bool]->Void) {
        sendUserRequestToUrl("friend/check/", parameters: [
            "phone": [phoneNumber],
        ]) {
            json in
            var exists: [Bool] = []
            if let existsJson = json?["exists"].array {
                exists = existsJson.map({ $0.bool! })
            }
            callback(exists)
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
        Alamofire.request(.POST, "\(domain)/api/\(url)", parameters: parameters)
            .responseJSON {
                response in
                if let json = response.result.value {
                    callback(JSON(json))
                } else {
                    callback(nil)
                }
            }
    }

    private static func sendUserRequestToUrl(url: String, var parameters: [String:AnyObject], callback: JSON?->Void) {
        parameters["id"] = User.userId
        parameters["session"] = User.sessionToken

        sendRequestToUrl("user/\(url)", parameters: parameters, callback: callback)
    }
}