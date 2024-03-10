;;; crc.el --- Cyclic Redundancy Check               -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Jean Libète

;; Author: Jean Libète <tomenzgg@mail.mayfirst.org>
;; URL: https://codeberg.org/tomenzgg/Emacs-CRC
;; Package-Requires: ((emacs "25.1"))
;; Keywords: lisp, checksum, algorithms
;; Version: 1.0.0

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

;; Library implementing checksum algorithm Cyclic Redundancy Check variations.

;;; Code:
;; seq-reduce
(require 'seq)

(defun crc-8--reverse (byte)
  "Reverse the bits of BYTE.

As implied by the argument name, BYTE should be an integer of no more than 8
bits."

  (seq-reduce (lambda (result i) (if (zerop (logand byte (expt 2 i)))
                                     result
                                   (logior result (expt 2 (- 7 i)))))
              (number-sequence 0 7)
              0))
(defun crc-8--general (sequence polynomial init ref-in ref-out xor-out)
  "General Cyclic Redundancy Check, 8-bit, application with customization.

Because there are varying versions of CRC-8 – depending on the Polynomial,
Initialization value, RefIn and RefOut, and the XorOut –, this function serves
to handle any possible iteration that needs to be computed.

SEQUENCE is a list, vector, or string.

POLYNOMIAL and INIT are integers.

REF-IN and REF-OUT are booleans.

XOR-OUT is a integer."

  (logand
    (logxor (funcall (if ref-out #'crc-8--reverse #'identity)
                     (seq-reduce (lambda (res1 byte)
                                   (seq-reduce (lambda (res2 _i)
                                                 (let ((shift1 (ash res2 1)))
                                                   (if (zerop (logand res2 #x80))
                                                       shift1
                                                     (logxor shift1 polynomial))))
                                               (number-sequence 0 7)
                                               (logxor res1
                                                       (funcall (if ref-in
                                                                    #'crc-8--reverse
                                                                  #'identity)
                                                                byte))))
                                 sequence
                                 init))
            xor-out)
    #b11111111))
(defun crc-32 (sequence &optional polynomial)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 32-bit values.

POLYNOMIAL may be a boolean or an integer.

While CRC-32 can work with any integer, only two are widely used.  If POLYNOMIAL
is \\='nil\\=', the IEEE polynomial (3,988,292,384 or #xEDB88320) will be used;
this polynomial is used by Bzip2, Ethernet (IEEE 802.3), Gzip, MPEG-2, PNG,
SATA, and Zip, amongst others.

If POLYNOMIAL is \\='t\\=', the Castagnoli polynomial (2,197,175,160 or
#x82F63B78) will be used; this polynomial is used by Btrfs, Ext4, iSCSI, and
SCTP, amongst others.

If POLYNOMIAL is an integer, the provided integer will be used for the
algorithm."

  (logxor (seq-reduce (lambda (res1 byte)
                        (seq-reduce (lambda (res2 _i)
                                      (logxor (/ res2 2)
                                              (* (cond
                                                  ((integerp polynomial) polynomial)
                                                  (polynomial            #x82F63B78)
                                                  (t                     #xEDB88320))
                                                 (logand res2 1))))
                                    (number-sequence 0 7)
                                    (logxor res1 byte)))
                      sequence
                      #xFFFFFFFF)
          #xFFFFFFFF))

(provide 'crc)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; crc.el ends here
