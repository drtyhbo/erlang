//
//  ContactsHelper.swift
//  chat
//
//  Created by Andreas Binnewies on 2/3/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Contacts
import Foundation

class Contact {
    let name: String
    let phoneNumber: PhoneNumber

    var firstName: String {
        return name.componentsSeparatedByString(" ")[0]
    }

    var lastName: String? {
        let names = name.componentsSeparatedByString(" ")
        return names.count > 1 ? names.last : nil
    }

    init(name: String, phoneNumber: String) {
        self.name = name
        self.phoneNumber = PhoneNumber(phoneNumber: phoneNumber)
    }
}

class ContactsHelper {
    func getAllContacts() -> [Contact] {
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

    private func nameForContact(contact: CNContact) -> String {
        return "\(contact.givenName) \(contact.familyName)"
    }

    private func phoneNumberForContact(contact: CNContact) -> String {
        let numberSet = NSCharacterSet(charactersInString: "0123456789").invertedSet
        let unformattedPhoneNumber = (contact.phoneNumbers[0].value as! CNPhoneNumber).stringValue
        return unformattedPhoneNumber.componentsSeparatedByCharactersInSet(numberSet).joinWithSeparator("")
    }
}