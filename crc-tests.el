;;; crc-tests.el --- Cyclic Redundancy Check's tests  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Jaft <jaft.r@mail.mayfirst.org>

;; This file is part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

;;; Commentary:

;; This file is part of CRC, the Elisp Cyclic Redundancy Check library.
;; See crc.el for more details.

;;; Code:
(require 'ert)

(ert-deftest crc--reverse-bits-test ()
  (should (= (crc--reverse-bits #b10100110 8) #b01100101))
  (should (= (crc--reverse-bits #b10100110 7) #b0110010))
  (should (= (crc--reverse-bits #b10100110 6) #b011001))
  (should (= (crc--reverse-bits #b10100110 5) #b01100))
  (should (= (crc--reverse-bits #b10100110 4) #b0110))
  (should (= (crc--reverse-bits #b10100110 3) #b011))
  (should (= (crc--reverse-bits #b10100110 2) #b01))
  (should (= (crc--reverse-bits #b10100110 1) #b0)))

(ert-deftest crc-8-tests ()
  (should (= (crc-8            "123456789") #xF4))
  (should (= (crc-8/autosar    "123456789") #xDF))
  (should (= (crc-8/bluetooth  "123456789") #x26))
  (should (= (crc-8/cdma2000   "123456789") #xDA))
  (should (= (crc-8/darc       "123456789") #x15))
  (should (= (crc-8/dvb-s2     "123456789") #xBC))
  (should (= (crc-8/ebu        "123456789") #x97))
  (should (= (crc-8/gsm-a      "123456789") #x37))
  (should (= (crc-8/gsm-b      "123456789") #x94))
  (should (= (crc-8/hitag      "123456789") #xB4))
  (should (= (crc-8/i-code     "123456789") #x7E))
  (should (= (crc-8/itu        "123456789") #xA1))
  (should (= (crc-8/lte        "123456789") #xEA))
  (should (= (crc-8/maxim      "123456789") #xA1))
  (should (= (crc-8/mifare-mad "123456789") #x99))
  (should (= (crc-8/nrsc-5     "123456789") #xF7))
  (should (= (crc-8/opensafety "123456789") #x3E))
  (should (= (crc-8/rohc       "123456789") #xD0))
  (should (= (crc-8/sae-j1850  "123456789") #x4B))
  (should (= (crc-8/wcdma      "123456789") #x25)))

(ert-deftest crc-10-tests ()
  (should (= (crc-10          "123456789") #x99))
  (should (= (crc-10/cdma2000 "123456789") #x33))
  (should (= (crc-10/gsm      "123456789") #x2A)))

(ert-deftest crc-11-tests ()
  (should (= (crc-11      "123456789") #xA3))
  (should (= (crc-11/umts "123456789") #x61)))

(ert-deftest crc-12-tests ()
  (should (= (crc-12/cdma2000 "123456789") #x4D))
  (should (= (crc-12/dect     "123456789") #x5B))
  (should (= (crc-12/gsm      "123456789") #x34))
  (should (= (crc-12/umts     "123456789") #xAF)))

(ert-deftest crc-13-tests ()
  (should (= (crc-13/bbc "123456789") #xFA)))

(ert-deftest crc-14-tests ()
  (should (= (crc-14/darc "123456789") #x2D))
  (should (= (crc-14/gsm  "123456789") #xAE)))

(ert-deftest crc-15-tests ()
  (should (= (crc-15         "123456789") #x9E))
  (should (= (crc-15/mpt1327 "123456789") #x66)))

(ert-deftest crc-16-tests ()
  (should (= (crc-16                   "123456789") #xBB3D))
  (should (= (crc-16/cdma2000          "123456789") #x4C06))
  (should (= (crc-16/cms               "123456789") #xAEE7))
  (should (= (crc-16/dds-110           "123456789") #x9ECF))
  (should (= (crc-16/dect-r            "123456789") #x007E))
  (should (= (crc-16/dect-x            "123456789") #x007F))
  (should (= (crc-16/dnp               "123456789") #xEA82))
  (should (= (crc-16/en-13757          "123456789") #xC2B7))
  (should (= (crc-16/genibus           "123456789") #xD64E))
  (should (= (crc-16/gsm               "123456789") #xCE3C))
  (should (= (crc-16/ibm-3740          "123456789") #x29B1))
  (should (= (crc-16/iso-iec-14443-3-a "123456789") #xBF05))
  (should (= (crc-16/iso-iec-14443-3-b "123456789") #x906E))
  (should (= (crc-16/kermit            "123456789") #x2189))
  (should (= (crc-16/lj1200            "123456789") #xBDF4))
  (should (= (crc-16/m17               "123456789") #x772B))
  (should (= (crc-16/maxim-dow         "123456789") #x44C2))
  (should (= (crc-16/mcrf4xx           "123456789") #x6F91))
  (should (= (crc-16/modbus            "123456789") #x4B37))
  (should (= (crc-16/nrsc-5            "123456789") #xA066))
  (should (= (crc-16/opensafety-a      "123456789") #x5D38))
  (should (= (crc-16/opensafety-b      "123456789") #x20FE))
  (should (= (crc-16/profibus          "123456789") #xA819))
  (should (= (crc-16/riello            "123456789") #x63D0))
  (should (= (crc-16/spi-fujitsu       "123456789") #xE5CC))
  (should (= (crc-16/t10-dif           "123456789") #xD0DB))
  (should (= (crc-16/teledisk          "123456789") #x0FB3))
  (should (= (crc-16/tms37157          "123456789") #x26B1))
  (should (= (crc-16/umts              "123456789") #xFEE8))
  (should (= (crc-16/usb               "123456789") #xB4C8))
  (should (= (crc-16/xmodem            "123456789") #x31C3)))

(ert-deftest crc-17-tests ()
  (should (= (crc-17/can-fd "123456789") #x4F03)))

(ert-deftest crc-21-tests ()
  (should (= (crc-21/can-fd "123456789") #xD841)))

(ert-deftest crc-24-tests ()
  (should (= (crc-24            "123456789") #x21CF02))
  (should (= (crc-24/ble        "123456789") #xC25A56))
  (should (= (crc-24/flexray-a  "123456789") #x7979BD))
  (should (= (crc-24/flexray-b  "123456789") #x1F23B8))
  (should (= (crc-24/interlaken "123456789") #xB4F3E6))
  (should (= (crc-24/lte-a      "123456789") #xCDE703))
  (should (= (crc-24/lte-b      "123456789") #x23EF52))
  (should (= (crc-24/os-9       "123456789") #x200FA5)))

(ert-deftest crc-30-tests ()
  (should (= (crc-30/cdma "123456789") #xC34ABF)))

(ert-deftest crc-31-tests ()
  (should (= (crc-31/philips "123456789") #xE9E46C)))

(ert-deftest crc-32-tests ()
  (should (= (crc-32            "123456789") #xCBF43926))
  (should (= (crc-32/aixm       "123456789") #x3010BF7F))
  (should (= (crc-32/autosar    "123456789") #x1697D06A))
  (should (= (crc-32/base91-d   "123456789") #x87315576))
  (should (= (crc-32/bzip2      "123456789") #xFC891918))
  (should (= (crc-32/cd-rom-edc "123456789") #x6EC2EDC4))
  (should (= (crc-32/cksum      "123456789") #x765E7680))
  (should (= (crc-32/iscsi      "123456789") #xE3069283))
  (should (= (crc-32/jamcrc     "123456789") #x340BC6D9))
  (should (= (crc-32/mef        "123456789") #xD2C22F51))
  (should (= (crc-32/mpeg-2     "123456789") #x0376E6E7))
  (should (= (crc-32/sata       "123456789") #xCF72AFE8))
  (should (= (crc-32/xfer       "123456789") #xBD0BE338)))

(ert-deftest crc-40-tests ()
  (should (= (crc-40/gsm "123456789") #xD4164FC646)))

(provide 'crc-tests)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; crc-tests.el ends here
