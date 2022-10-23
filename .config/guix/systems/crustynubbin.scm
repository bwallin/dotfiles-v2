;; NOTE: This file is generated from ~/.dotfiles/System.org.  Please see commentary there.

(define-module (crustynubbin)
  #:use-module (base-system)
  #:use-module (gnu)
  #:use-module (nongnu packages linux))

(operating-system
 (inherit base-operating-system)
 (host-name "crustynubbin")

  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (targets (list "/dev/sda"))
               (keyboard-layout keyboard-layout)))

 (firmware (list linux-firmware sof-firmware))

 (mapped-devices
  (list (mapped-device
         (source (uuid "FILL ME"))
         (target "system-root")
         (type luks-device-mapping))))

 (file-systems (cons*
                (file-system
                 (device (file-system-label "system-root"))
                 (mount-point "/")
                 (type "ext4")
                 (dependencies mapped-devices))
                (file-system
                 (device "/dev/sda1")
                 (mount-point "/boot/efi")
                 (type "vfat"))
                %base-file-systems)))
