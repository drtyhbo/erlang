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

class APIManager: NSObject {
    class Error {
        let error: String

        init(_ error: String) {
            self.error = error
        }
    }

    static let sharedManager = APIManager()

    let domain = Constants.host
    let webPort = Constants.webPort == "80" ? "" : ":\(Constants.webPort)"

    func registerPhoneNumber(phoneNumber: PhoneNumber, callback: Bool->Void) {
        sendRequestToUrl("register/", parameters: [
            "phone": phoneNumber.toString()
        ]) {
            json in
            callback(self.errorFromJson(json) == nil)
        }
    }

    func confirmPhoneNumber(phoneNumber: PhoneNumber, withCode code: String, key: String, callback: (String?, String?, Error?)->Void) {
        sendRequestToUrl("confirm/", parameters: [
            "phone": phoneNumber.toString(),
            "code": code,
            "key": key,
        ]) {
            json in
            callback(json?["id"].string, json?["sessionToken"].string, self.errorFromJson(json))
        }
    }

    struct FriendData {
        let id: Int
        let name: String
        let base64Key: String
    }

    func getFriendsWithPhoneNumbers(phoneNumbers: [PhoneNumber], callback: [FriendData]->Void) {
        sendUserRequestToUrl("friend/check/", parameters: [
            "phone": [phoneNumbers.map({ $0.toString() })],
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

    func registerDeviceToken(deviceToken: String, callback: Bool->Void) {
        sendUserRequestToUrl("pns/register/", parameters: [
            "token": deviceToken,
            "type": "ios"
        ]) {
            json in
            callback(json != nil && json!["status"].string == "ok")
        }
    }

    func createFileForFriend(friend: Friend, numFiles: Int, callback: Int?->Void) {
        sendUserRequestToUrl("file/create/", parameters: [
            "friendId": friend.id,
            "numIds": numFiles]) {
            json in
            if let json = json, fileId = json["fileId"].int {
                callback(fileId)
            } else {
                callback(nil)
            }
        }
    }

    func getUrlForFileWithId(fileId: Int, callback: NSURL?->Void) {
        getUrlForFileWithId(fileId, method: "GET", contentType: "", callback: callback)
    }

    func getUrlForFileWithId(fileId: Int, method: String, contentType: String, callback: NSURL?->Void) {
        sendUserRequestToUrl("file/get/", parameters: [
            "fileId": fileId,
            "method": method,
            "contentType": contentType]) {
            json in
            if let json = json, fileUrlString = json["fileUrl"].string, fileUrl = NSURL(string: fileUrlString) {
                callback(fileUrl)
            } else {
                callback(nil)
            }
        }
    }

    private func temporaryFileUrl() -> NSURL {
        return NSURL(fileURLWithPath: NSTemporaryDirectory()).URLByAppendingPathComponent(NSUUID().UUIDString)
    }

    func uploadData(data: NSData, toS3Url s3Url: NSURL, contentType: String, progressCallback: (Int, Int)->Void, callback: Bool->Void) {
        let localUrl = temporaryFileUrl()
        data.writeToURL(localUrl, atomically: true)

        let headers = [
            "Content-Type": contentType]
        Alamofire.upload(.PUT, s3Url, headers: headers, file: localUrl)
            .progress {
                bytesWritten, totalBytesWritten, totalBytesExpectedToWrite in
                progressCallback(Int(totalBytesWritten), Int(totalBytesExpectedToWrite))
            }
            .response {
                response in
                if let response = response.1 {
                    callback(response.statusCode == 200)
                } else {
                    callback(false)
                }
            }
    }

    func downloadFileWithUrl(url: NSURL, callback: (NSData?, String?)->Void) {
        let destinationUrl = temporaryFileUrl()
        let destination: (NSURL, NSHTTPURLResponse)->NSURL = {
            temporaryUrl, response in
            return destinationUrl
        }

        Alamofire.download(.GET, url, destination: destination)
            .response { _, response, data, error in
                if let response = response, data = NSData(contentsOfURL: destinationUrl) {
                    callback(data, response.MIMEType)
                } else {
                    callback(nil, nil)
                }
            }
    }

    private func getProfilePicUploadUrl(callback: NSURL?->Void) {
        sendUserRequestToUrl("profilepic/") {
            json in
            if let json = json, uploadUrl = json["uploadUrl"].string {
                callback(NSURL(string: uploadUrl))
            } else {
                callback(nil)
            }
        }
    }

    func uploadProfilePic(profilePic: UIImage, callback: Bool->Void) {
        let localUrl = temporaryFileUrl()

        guard let data = UIImageJPEGRepresentation(profilePic, 0.8) else {
            callback(false)
            return
        }
        data.writeToURL(localUrl, atomically: true)

        getProfilePicUploadUrl {
            s3Url in
            guard let s3Url = s3Url else {
                callback(false)
                return
            }

            let headers = [
                "Content-Type": "image/jpeg"]
            Alamofire.upload(.PUT, s3Url, headers: headers, file: localUrl)
                .response {
                    response in
                    if let response = response.1 {
                        callback(response.statusCode == 200)
                    } else {
                        callback(false)
                    }
                }
        }
    }

    private func errorFromJson(json: JSON?) -> Error? {
        if let json = json {
            return json["status"] == "ok" ? nil : Error(json["status"].string ?? "invalid")
        } else {
            return Error("invalid")
        }
    }

    private func sendRequestToUrl(url: String, parameters: [String:AnyObject], callback: JSON?->Void) {
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

    private func sendUserRequestToUrl(url: String, callback: JSON?->Void) {
        sendUserRequestToUrl(url, parameters: [:], callback: callback)
    }

    private func sendUserRequestToUrl(url: String, var parameters: [String:AnyObject], callback: JSON?->Void) {
        parameters["id"] = User.userId
        parameters["session"] = User.sessionToken

        sendRequestToUrl("user/\(url)", parameters: parameters, callback: callback)
    }
}