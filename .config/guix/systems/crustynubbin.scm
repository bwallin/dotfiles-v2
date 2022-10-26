;; NOTE: This file is generated from ~/.dotfiles/System.org.  Please see commentary there.

(define-module (crustynubbin)
  #:use-module (base-system)
  #:use-module (gnu)
  #:use-module (nongnu packages linux))

(operating-system
 (inherit base-operating-system)
 (host-name "crustynubbin")

 (firmware (list linux-firmware sof-firmware))

 (mapped-devices
  (list (mapped-device
         (source (uuid "5a8c0c84-18bf-46c1-9f54-569ab0362f75"))
         (target "cryptroot")
         (type luks-device-mapping))))

 (file-systems (cons*
                (file-system
                 (device "/dev/mapper/cryptroot")
                 (mount-point "/")
                 (type "ext4")
                 (dependencies mapped-devices))
                (file-system
                 (device (uuid "6F43-BCA3"
                               'fat32))
                 (mount-point "/boot/efi")
                 (type "vfat"))
                %base-file-systems)))
