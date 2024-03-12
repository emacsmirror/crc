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


(provide 'crc-tests)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; crc-tests.el ends here
