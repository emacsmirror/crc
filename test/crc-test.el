;;; crc-test.el --- Cyclic Redundancy Check's tests  -*- lexical-binding: t -*-

;; Copyright (C) 2024  Jean Libète

;; Author: Jean Libète <tomenzgg@mail.mayfirst.org>
;; Maintainer: Jean Libète <tomenzgg@mail.mayfirst.org>
;; Keywords: tests

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file is part of CRC, the Elisp Cyclic Redundancy Check library.
;; See crc.el for more details.

;;; Code:
(let ((load-path (append (mapcar (lambda (fragment)
                                   (concat (if load-file-name
                                               (file-name-directory load-file-name)
                                             default-directory)
                                           fragment))
                                 '("." ".."))
                         load-path)))
  (require 'crc)
  (require 'ert))

(ert-deftest crc--reverse-bits-test ()
  (should (= (crc--reverse-bits #b10100110 8) #b01100101))
  (should (= (crc--reverse-bits #b10100110 7) #b0110010))
  (should (= (crc--reverse-bits #b10100110 6) #b011001))
  (should (= (crc--reverse-bits #b10100110 5) #b01100))
  (should (= (crc--reverse-bits #b10100110 4) #b0110))
  (should (= (crc--reverse-bits #b10100110 3) #b011))
  (should (= (crc--reverse-bits #b10100110 2) #b01))
  (should (= (crc--reverse-bits #b10100110 1) #b0)))

(defvar crc-test--string "123456789"
  "The string we'll hash for consistent test data.")
(ert-deftest crc-8-tests ()
  (should (= (crc-8            crc-test--string) #xF4))
  (should (= (crc-8/autosar    crc-test--string) #xDF))
  (should (= (crc-8/bluetooth  crc-test--string) #x26))
  (should (= (crc-8/cdma2000   crc-test--string) #xDA))
  (should (= (crc-8/darc       crc-test--string) #x15))
  (should (= (crc-8/dvb-s2     crc-test--string) #xBC))
  (should (= (crc-8/ebu        crc-test--string) #x97))
  (should (= (crc-8/gsm-a      crc-test--string) #x37))
  (should (= (crc-8/gsm-b      crc-test--string) #x94))
  (should (= (crc-8/hitag      crc-test--string) #xB4))
  (should (= (crc-8/i-code     crc-test--string) #x7E))
  (should (= (crc-8/itu        crc-test--string) #xA1))
  (should (= (crc-8/lte        crc-test--string) #xEA))
  (should (= (crc-8/maxim      crc-test--string) #xA1))
  (should (= (crc-8/mifare-mad crc-test--string) #x99))
  (should (= (crc-8/nrsc-5     crc-test--string) #xF7))
  (should (= (crc-8/opensafety crc-test--string) #x3E))
  (should (= (crc-8/rohc       crc-test--string) #xD0))
  (should (= (crc-8/sae-j1850  crc-test--string) #x4B))
  (should (= (crc-8/wcdma      crc-test--string) #x25)))

(ert-deftest crc-10-tests ()
  (should (= (crc-10          crc-test--string) #x99))
  (should (= (crc-10/cdma2000 crc-test--string) #x33))
  (should (= (crc-10/gsm      crc-test--string) #x2A)))

(ert-deftest crc-11-tests ()
  (should (= (crc-11      crc-test--string) #xA3))
  (should (= (crc-11/umts crc-test--string) #x61)))

(ert-deftest crc-12-tests ()
  (should (= (crc-12/cdma2000 crc-test--string) #x4D))
  (should (= (crc-12/dect     crc-test--string) #x5B))
  (should (= (crc-12/gsm      crc-test--string) #x34))
  (should (= (crc-12/umts     crc-test--string) #xAF)))

(ert-deftest crc-13-tests ()
  (should (= (crc-13/bbc crc-test--string) #xFA)))

(ert-deftest crc-14-tests ()
  (should (= (crc-14/darc crc-test--string) #x2D))
  (should (= (crc-14/gsm  crc-test--string) #xAE)))

(ert-deftest crc-15-tests ()
  (should (= (crc-15         crc-test--string) #x9E))
  (should (= (crc-15/mpt1327 crc-test--string) #x66)))

(ert-deftest crc-16-tests ()
  (should (= (crc-16                   crc-test--string) #xBB3D))
  (should (= (crc-16/cdma2000          crc-test--string) #x4C06))
  (should (= (crc-16/cms               crc-test--string) #xAEE7))
  (should (= (crc-16/dds-110           crc-test--string) #x9ECF))
  (should (= (crc-16/dect-r            crc-test--string) #x007E))
  (should (= (crc-16/dect-x            crc-test--string) #x007F))
  (should (= (crc-16/dnp               crc-test--string) #xEA82))
  (should (= (crc-16/en-13757          crc-test--string) #xC2B7))
  (should (= (crc-16/genibus           crc-test--string) #xD64E))
  (should (= (crc-16/gsm               crc-test--string) #xCE3C))
  (should (= (crc-16/ibm-3740          crc-test--string) #x29B1))
  (should (= (crc-16/iso-iec-14443-3-a crc-test--string) #xBF05))
  (should (= (crc-16/iso-iec-14443-3-b crc-test--string) #x906E))
  (should (= (crc-16/kermit            crc-test--string) #x2189))
  (should (= (crc-16/lj1200            crc-test--string) #xBDF4))
  (should (= (crc-16/m17               crc-test--string) #x772B))
  (should (= (crc-16/maxim-dow         crc-test--string) #x44C2))
  (should (= (crc-16/mcrf4xx           crc-test--string) #x6F91))
  (should (= (crc-16/modbus            crc-test--string) #x4B37))
  (should (= (crc-16/nrsc-5            crc-test--string) #xA066))
  (should (= (crc-16/opensafety-a      crc-test--string) #x5D38))
  (should (= (crc-16/opensafety-b      crc-test--string) #x20FE))
  (should (= (crc-16/profibus          crc-test--string) #xA819))
  (should (= (crc-16/riello            crc-test--string) #x63D0))
  (should (= (crc-16/spi-fujitsu       crc-test--string) #xE5CC))
  (should (= (crc-16/t10-dif           crc-test--string) #xD0DB))
  (should (= (crc-16/teledisk          crc-test--string) #x0FB3))
  (should (= (crc-16/tms37157          crc-test--string) #x26B1))
  (should (= (crc-16/umts              crc-test--string) #xFEE8))
  (should (= (crc-16/usb               crc-test--string) #xB4C8))
  (should (= (crc-16/xmodem            crc-test--string) #x31C3))
  (should (= (crc-16/xmodem-2          crc-test--string) #x0C73)))

(ert-deftest crc-17-tests ()
  (should (= (crc-17/can-fd crc-test--string) #x4F03)))

(ert-deftest crc-21-tests ()
  (should (= (crc-21/can-fd crc-test--string) #xD841)))

(ert-deftest crc-24-tests ()
  (should (= (crc-24            crc-test--string) #x21CF02))
  (should (= (crc-24/ble        crc-test--string) #xC25A56))
  (should (= (crc-24/flexray-a  crc-test--string) #x7979BD))
  (should (= (crc-24/flexray-b  crc-test--string) #x1F23B8))
  (should (= (crc-24/interlaken crc-test--string) #xB4F3E6))
  (should (= (crc-24/lte-a      crc-test--string) #xCDE703))
  (should (= (crc-24/lte-b      crc-test--string) #x23EF52))
  (should (= (crc-24/os-9       crc-test--string) #x200FA5)))

(ert-deftest crc-30-tests ()
  (should (= (crc-30/cdma crc-test--string) #xC34ABF)))

(ert-deftest crc-31-tests ()
  (should (= (crc-31/philips crc-test--string) #xE9E46C)))

(ert-deftest crc-32-tests ()
  (should (= (crc-32            crc-test--string) #xCBF43926))
  (should (= (crc-32/aixm       crc-test--string) #x3010BF7F))
  (should (= (crc-32/autosar    crc-test--string) #x1697D06A))
  (should (= (crc-32/base91-d   crc-test--string) #x87315576))
  (should (= (crc-32/bzip2      crc-test--string) #xFC891918))
  (should (= (crc-32/cd-rom-edc crc-test--string) #x6EC2EDC4))
  (should (= (crc-32/cksum      crc-test--string) #x765E7680))
  (should (= (crc-32/iscsi      crc-test--string) #xE3069283))
  (should (= (crc-32/jamcrc     crc-test--string) #x340BC6D9))
  (should (= (crc-32/koopman    crc-test--string) #x2D3DD0AE))
  (should (= (crc-32/mef        crc-test--string) #xD2C22F51))
  (should (= (crc-32/mpeg-2     crc-test--string) #x0376E6E7))
  (should (= (crc-32/sata       crc-test--string) #xCF72AFE8))
  (should (= (crc-32/xfer       crc-test--string) #xBD0BE338)))

(ert-deftest crc-40-tests ()
  (should (= (crc-40/gsm crc-test--string) #xD4164FC646)))

(ert-deftest crc-64-tests ()
  (should (= (crc-64        crc-test--string) #x6C40DF5F0B497347))
  (should (= (crc-64/go-iso crc-test--string) #xB90956C775A41001))
  (should (= (crc-64/ms     crc-test--string) #x75D4B74F024ECEEA))
  (should (= (crc-64/redis  crc-test--string) #xE9C6D914C4B8D9CA))
  (should (= (crc-64/we     crc-test--string) #x62EC59E3F1A4F00A))
  (should (= (crc-64/xz     crc-test--string) #x995DC9BBDF1939FA)))

(ert-deftest crc-82-tests ()
  (should (= (crc-82/darc crc-test--string) #x9EA83F625023801FD612)))

(provide 'crc-test)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; crc-test.el ends here
