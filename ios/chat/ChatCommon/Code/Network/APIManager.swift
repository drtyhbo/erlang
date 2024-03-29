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

public class APIManager: NSObject {
    public class Error {
        let error: String

        init(_ error: String) {
            self.error = error
        }
    }

    public static let sharedManager = APIManager()

    let domain = Constants.host
    let webPort = Constants.webPort == "80" ? "" : ":\(Constants.webPort)"

    public func registerPhoneNumber(phoneNumber: PhoneNumber, deviceUUID: NSUUID, callback: Bool->Void) {
        sendRequestToUrl("register/", parameters: [
            "phone": phoneNumber.fullNumber,
            "deviceUuid": deviceUUID.UUIDString.stringByReplacingOccurrencesOfString("-", withString: "")
        ]) {
            json in
            callback(self.errorFromJson(json) == nil)
        }
    }

    public func confirmPhoneNumber(phoneNumber: PhoneNumber, deviceUUID: NSUUID, withCode code: String, preKeys: [PreKey], callback: (String?, String?, String?, String?, String?, Error?)->Void) {
        sendRequestToUrl("confirm/", parameters: [
            "phone": phoneNumber.fullNumber,
            "deviceUuid": deviceUUID.UUIDString.stringByReplacingOccurrencesOfString("-", withString: ""),
            "code": code,
            "preKeys": preKeys.map({ ["i": $0.index, "pk": $0.keyPair.publicKey.base64 ]})
        ]) {
            json in
            callback(json?["id"].string, json?["deviceId"].string, json?["sessionToken"].string, json?["firstName"].string, json?["lastName"].string, self.errorFromJson(json))
        }
    }

    struct FriendData {
        let id: String
        let phoneNumber: String
    }

    func getFriendsWithPhoneNumbers(phoneNumbers: [PhoneNumber], callback: [FriendData]->Void) {
        sendUserRequestToUrl("friend/check/", parameters: [
            "phone": [phoneNumbers.map({ $0.fullNumber })],
        ]) {
            json in

            var friendsData: [FriendData] = []
            if let friendsJson = json?["friends"].array {
                for i in 0..<friendsJson.count {
                    if friendsJson[i].null != nil {
                        continue
                    }

                    let friendJson = friendsJson[i]
                    if let id = friendJson["id"].string, phoneNumber = friendJson["phone"].string {
                        friendsData.append(FriendData(id: id, phoneNumber: phoneNumber))
                    }
                }
            }
            callback(friendsData)
        }
    }

    func getPrekeyForDevice(device: Device, callback: (Int?, NSData?)->Void) {
        sendUserRequestToUrl("device/prekey/", parameters: [
            "deviceId": device.id
        ]) {
            json in

            guard let keyIndex = json?["keyIndex"].int, base64PublicKey = json?["publicKey"].string, publicKey = NSData.fromBase64(base64PublicKey) else {
                callback(nil, nil)
                return
            }

            callback(keyIndex, publicKey)
        }
    }

    public func activeDevicesForFriends(friends: [Friend], callback: [String]->Void) {
        sendUserRequestToUrl("device/active/", parameters: [
            "userIds": friends.map({ $0.id })
        ]) {
            json in

            guard let deviceIdsJson = json?["deviceIds"].array else {
                callback([])
                return
            }

            var deviceIds: [String] = []
            for i in 0..<deviceIdsJson.count {
                guard let deviceId = deviceIdsJson[i].string else {
                    continue
                }
                deviceIds.append(deviceId)
            }

            callback(deviceIds)
        }
    }

    public func registerDeviceToken(deviceToken: String, callback: Bool->Void) {
        sendUserRequestToUrl("pns/register/", parameters: [
            "token": deviceToken,
            "type": "ios"
        ]) {
            json in
            callback(json != nil && json!["status"].string == "ok")
        }
    }

    public func updateInfoWithFirstName(firstName: String?, lastName: String?, callback: Bool->Void) {
        var parameters: [String:AnyObject] = [:]
        if let firstName = firstName {
            parameters["firstName"] = firstName
        }
        if let lastName = lastName {
            parameters["lastName"] = lastName
        }

        sendUserRequestToUrl("info/update/", parameters: parameters) {
            json in
            callback(json != nil && json!["status"].string == "ok")
        }
    }

    func getInfoForUsersWithIds(userIds: [String], callback: [(firstName: String, lastName: String)]?->Void) {
        sendUserRequestToUrl("info/get/", parameters: [
            "userIds": userIds
        ]) { json in
            guard let nameResults = json?["names"].array else {
                callback(nil)
                return
            }

            var names: [(firstName: String, lastName: String)] = []
            for nameResult in nameResults {
                guard let firstName = nameResult["firstName"].string, lastName = nameResult["lastName"].string else {
                    callback(nil)
                    return
                }
                names.append((firstName: firstName, lastName: lastName))
            }

            callback(names)
        }
    }

    func createFileForFriends(friends: [Friend], numFiles: Int, callback: [String]?->Void) {
        sendUserRequestToUrl("file/create/", parameters: [
            "friendIds": friends.map({ $0.id }),
            "numIds": numFiles]) {
            json in
            if let json = json, fileIdsJson = json["fileIds"].array {
                var fileIds: [String] = []
                for fileIdJson in fileIdsJson {
                    guard let fileId = fileIdJson.string else {
                        callback(nil)
                        return
                    }
                    fileIds.append(fileId)
                }
                callback(fileIds)
            } else {
                callback(nil)
            }
        }
    }

    public func getUrlForFileWithId(fileId: String, callback: NSURL?->Void) {
        getUrlForFileWithId(fileId, method: "GET", contentType: "", callback: callback)
    }

    func getUrlForFileWithId(fileId: String, method: String, contentType: String, callback: NSURL?->Void) {
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

    public func downloadFileWithUrl(url: NSURL, callback: (NSData?, String?)->Void) {
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

    public func uploadProfilePic(profilePic: UIImage, callback: Bool->Void) {
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
        let webProtocol = Constants.webPort == "80" ? "https" : "http"
        Alamofire.request(.POST, "\(webProtocol)://\(domain)\(webPort)/api/\(url)", parameters: parameters)
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
        parameters["id"] = User.deviceId
        parameters["session"] = User.sessionToken

        sendRequestToUrl("user/\(url)", parameters: parameters, callback: callback)
    }
}