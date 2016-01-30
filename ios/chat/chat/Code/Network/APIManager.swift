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
    static let domain = "http://127.0.0.1:3000"

    static func registerPhoneNumber(phoneNumber: String, callback: Bool->Void) {
        sendRequestToUrl("register/", parameters: [
            "phone": phoneNumber
        ]) {
            json in
            callback(isStatusOkJson(json))
        }
    }

    static func confirmPhoneNumber(phoneNumber: String, withCode code: String, callback: String?->Void) {
        sendRequestToUrl("confirm/", parameters: [
            "phone": phoneNumber,
            "code": code
        ]) {
            json in
            callback(json?["sessionToken"].string)
        }
    }

    private static func isStatusOkJson(json: JSON?) -> Bool {
        return json != nil && json!["status"].string == "ok"
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
}