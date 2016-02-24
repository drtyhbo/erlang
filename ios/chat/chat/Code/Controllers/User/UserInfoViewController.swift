//
//  UserInfoViewController.swift
//  chat
//
//  Created by Andreas Binnewies on 2/23/16.
//  Copyright © 2016 drtyhbo. All rights reserved.
//

import Foundation
import UIKit

class UserInfoViewController: UIViewController {
    @IBOutlet weak var profilePic: ChatProfilePic!
    @IBOutlet weak var firstName: UITextField!
    @IBOutlet weak var lastName: UITextField!

    private var profilePicImage: UIImage?
    private var imagePickerController: UIImagePickerController!

    init() {
        super.init(nibName: "UserInfoViewController", bundle: nil)
    }

    required init?(coder aDecoder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    override func viewDidLoad() {
        super.viewDidLoad()

        setupNextButton()

        profilePic.addGestureRecognizer(UITapGestureRecognizer(target: self, action: "didTapProfilePic"))

        firstName.becomeFirstResponder()
    }

    private func setupNextButton() {
        let nextButton = UIBarButtonItem(barButtonSystemItem: .Done, target: self, action: "saveInfo")
        navigationItem.rightBarButtonItem = nextButton
    }

    private func setupActivityIndicator() {
        let activityIndicator = UIActivityIndicatorView(activityIndicatorStyle: .Gray)
        navigationItem.rightBarButtonItem = UIBarButtonItem(customView: activityIndicator)
        activityIndicator.startAnimating()
    }

    private func showMainViewController() {
        navigationController?.pushViewController(MainViewController(), animated: true)
    }

    @objc private func saveInfo() {
        setupActivityIndicator()

        APIManager.sharedManager.updateInfoWithFirstName(firstName.text ?? "", lastName: lastName.text ?? "") {
            success in
            if !success {
                self.setupNextButton()
                return
            }

            if let profilePicImage = self.profilePicImage {
                self.uploadProfilePicImage(profilePicImage)
            } else {
                self.showMainViewController()
            }
        }
    }

    private func uploadProfilePicImage(profilePicImage: UIImage) {
        APIManager.sharedManager.uploadProfilePic(profilePicImage) {
            success in
            if !success {
                self.setupNextButton()
                return
            }

            User.profilePic = profilePicImage
            self.showMainViewController()
        }
    }

    @objc private func didTapProfilePic() {
        let imagePickerController = UIImagePickerController()
        imagePickerController.delegate = self
        imagePickerController.sourceType = .PhotoLibrary
        imagePickerController.allowsEditing = true
        presentViewController(imagePickerController, animated: true, completion: nil)

        self.imagePickerController = imagePickerController
    }
}

extension UserInfoViewController: UIImagePickerControllerDelegate, UINavigationControllerDelegate {
    func imagePickerController(picker: UIImagePickerController, didFinishPickingMediaWithInfo info: [String : AnyObject]) {
        if let image = info[UIImagePickerControllerEditedImage] as? UIImage {
            self.profilePicImage = image.resizeToSize(Constants.profilePicSize)
            self.profilePic.image = self.profilePicImage
        }

        dismissViewControllerAnimated(true, completion: nil)
        self.imagePickerController = nil
    }
}