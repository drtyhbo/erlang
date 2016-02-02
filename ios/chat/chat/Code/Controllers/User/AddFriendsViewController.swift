//
//  AddFriendsViewController.swift
//  chat
//
//  Created by Andreas Binnewies on 1/31/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Contacts
import Foundation
import UIKit

class AddFriendsViewController: UIViewController {
    @IBOutlet weak var contactsTable: UITableView!

    private let addFriendReuseIdentifier = "AddFriendTableViewCell"

    private var contacts: [Contact] = []

    init() {
        super.init(nibName: "AddFriendsViewController", bundle: nil)
    }

    required init?(coder aDecoder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    override func viewDidLoad() {
        super.viewDidLoad()

        requestContactsAccess {
            hasAccess in
            var contacts = self.getAllContacts()
            let phoneNumbers = contacts.map({ $0.phoneNumber })
            APIManager.usersExistForPhoneNumbers(phoneNumbers) {
                exists in
                for i in 0..<exists.count {
                    contacts[i].userExists = exists[i]
                }

                self.contacts = contacts
                self.contactsTable.reloadData()
            }
        }

        contactsTable.registerNib(UINib(nibName: "AddFriendTableViewCell", bundle: nil), forCellReuseIdentifier: addFriendReuseIdentifier)
    }

    private func requestContactsAccess(completionHandler: Bool->Void) {
        let authorizationStatus = CNContactStore.authorizationStatusForEntityType(CNEntityType.Contacts)
     
        switch authorizationStatus {
        case .Authorized:
            completionHandler(true)
     
        case .Denied, .NotDetermined:
            CNContactStore().requestAccessForEntityType(CNEntityType.Contacts, completionHandler: {
                access, accessError in
                completionHandler(access)
            })
     
        default:
            completionHandler(false)
        }
    }

    private func getAllContacts() -> [Contact] {
        let contactStore = CNContactStore()
        let keysToFetch = [
            CNContactFormatter.descriptorForRequiredKeysForStyle(.FullName),
            CNContactPhoneNumbersKey,
            CNContactImageDataAvailableKey,
            CNContactThumbnailImageDataKey]

        var allContainers: [CNContainer] = []
        do {
            allContainers = try contactStore.containersMatchingPredicate(nil)
        } catch {
            print("Error fetching containers")
        }

        var contacts: [CNContact] = []
        for container in allContainers {
            let fetchPredicate = CNContact.predicateForContactsInContainerWithIdentifier(container.identifier)

            do {
                let containerResults = try contactStore.unifiedContactsMatchingPredicate(fetchPredicate, keysToFetch: keysToFetch)
                contacts.appendContentsOf(containerResults)
            } catch {
            }
        }

        contacts = contacts.filter({ $0.phoneNumbers.count > 0 })

        return contacts.map({
            return Contact(name: nameForContact($0), phoneNumber: phoneNumberForContact($0))
        })
    }

    private func nameForContact(contact: CNContact) -> String {
        return "\(contact.givenName) \(contact.familyName)"
    }

    private func phoneNumberForContact(contact: CNContact) -> String {
        let numberSet = NSCharacterSet(charactersInString: "0123456789").invertedSet
        let unformattedPhoneNumber = (contact.phoneNumbers[0].value as! CNPhoneNumber).stringValue
        return unformattedPhoneNumber.componentsSeparatedByCharactersInSet(numberSet).joinWithSeparator("")
    }
}

extension AddFriendsViewController: UITableViewDataSource, UITableViewDelegate {
    func numberOfSectionsInTableView(tableView: UITableView) -> Int {
        return 1
    }

    func tableView(tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return contacts.count
    }

    func tableView(tableView: UITableView, cellForRowAtIndexPath indexPath: NSIndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCellWithIdentifier(addFriendReuseIdentifier) as! AddFriendTableViewCell
        cell.contact = contacts[indexPath.row]
        return cell
    }

    func tableView(tableView: UITableView, didSelectRowAtIndexPath indexPath: NSIndexPath) {
    }
}