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
  (should (= (crc-15         "123456789") #x9E)))

(provide 'crc-tests)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; crc-tests.el ends here
