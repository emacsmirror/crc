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

(defun crc--reverse-bits (integer number-of-bits)
  "Reverse the bits of INTEGER, starting from the right, by the NUMBER-OF-BITS."

  (let ((maxindex (1- number-of-bits)))
    (seq-reduce (lambda (result i) (if (zerop (logand integer (expt 2 i)))
                                       result
                                     (logior result (expt 2 (- maxindex i)))))
                (number-sequence 0 maxindex)
                0)))

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
(defun crc-8 (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 8-bit values.

Aliases: CRC-8/SMBUS.

The System Management Interface Forum, Inc. (3 August 2000), System Management
Bus (SMBus) Specification, version 2.0:
--Definition: Width, Poly, RefIn/Out (Section 4.2, p.19; Section 5.4.1.3, p.27)
https://web.archive.org/web/20230605175600/http://www.smbus.org/specs/smbus20.pdf"

  (crc-8--general sequence #x07 #x00 nil nil #x00))
(defun crc-8/autosar (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 8-bit values.

AUTOSAR (24 November 2022), AUTOSAR Classic Platform release R22-11:
https://www.autosar.org/fileadmin/user_upload/standards/classic/22-11/AUTOSAR_SWS_CRCLibrary.pdf"

  (crc-8--general sequence #x2F #xFF nil nil #xFF))
(defun crc-8/bluetooth (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 8-bit values.

Bluetooth SIG (31 January 2023), Bluetooth Specification:
https://www.bluetooth.org/DocMan/handlers/DownloadDoc.ashx?doc_id=521059"

  (crc-8--general sequence #xA7 #x00   t   t #x00))
(defun crc-8/cdma2000 (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 8-bit values.

3rd Generation Partnership Project 2 (3GPP2) (September 2009), Physical layer
standard for cdma2000 spread spectrum systems, revision E, version 1.0:
https://web.archive.org/web/20230601141629/https://3gpp2.org/Public_html/Specs/C.S0002-E_v1.0_cdma200_1x_PHY-090925.pdf"

  (crc-8--general sequence #x9B #xFF nil nil #x00))
(defun crc-8/darc (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 8-bit values.

ETSI EN 300 751 version 1.2.1 (January 2003):
https://www.etsi.org/deliver/etsi_en/300700_300799/300751/01.02.01_60/en_300751v010201p.pdf"

  (crc-8--general sequence #x39 #x00   t   t #x00))
(defun crc-8/dvb-s2 (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 8-bit values.

Ethernet POWERLINK Standardisation Group (18 October 2018), OpenSAFETY Safety
Profile Specification Working Draft Proposal, version 1.5.2:
https://www.ethernet-powerlink.org/fileadmin/user_upload/Dokumente/Downloads/TECHNICAL_DOCUMENTS/EPSG_WDP_304_V-1-5-2.pdf"

  (crc-8--general sequence #xD5 #x00 nil nil #x00))
(defun crc-8/ebu (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 8-bit values.

Aliases: CRC-8/AES and CRC-8/TECH-3250.

European Broadcasting Union (8 June 2004), Tech 3250: Specification of the
digital audio interface, 3rd edition:
https://tech.ebu.ch/docs/tech/tech3250.pdf"

  (crc-8--general sequence #x1D #xFF   t   t #x00))
(defun crc-8/gsm-a (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 8-bit values.

ETSI TS 100 909 version 8.9.0 (January 2005) (Full mathematical description in
Section 3.1.1.1, pp.17–18):
https://www.etsi.org/deliver/etsi_ts/100900_100999/100909/08.09.00_60/ts_100909v080900p.pdf"

  (crc-8--general sequence #x1D #x00 nil nil #x00))
(defun crc-8/gsm-b (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 8-bit values.

ETSI TS 100 909 version 8.9.0 (January 2005) (Full mathematical description in
Section 5.1.5.1.3, p.71 — Section 5.1.5.2.2, p.73 — Section 5.1.9.1.3, p.80 —
Section 5.1.9.2.2, p.83 — Section 5.1.11.1.3, pp.86–7 — Section 5.1.11.2.2,
p.89):
https://www.etsi.org/deliver/etsi_ts/100900_100999/100909/08.09.00_60/ts_100909v080900p.pdf"

  (crc-8--general sequence #x49 #x00 nil nil #xFF))
(defun crc-8/hitag (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 8-bit values.

Philips Semiconductors (18 July 2006), HTS IC H32/HTS IC H56/HTS IC H48
transponder IC Product Specification, revision 3.1:
--Definition: Width, Poly, Init (Section 13.1, p.45 — Section 13.2.2, p.45)
--Code:       C (Section 13.3, p.46)
https://media.digikey.com/pdf/Data%20Sheets/NXP%20PDFs/HTS.pdf"

  (crc-8--general sequence #x1D #xFF nil nil #x00))
(defun crc-8/i-code (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 8-bit values.

Philips Semiconductors (30 January 2004), SL2 ICS11 Product Specification,
revision 3.0:
--Definition: Width, Poly, Init (Section 10.1, p.26)
--Code:       C (Section 10.3.1, pp.27–9)
--Worked example (as code trace) (Section 10.3.1, p.29)
https://web.archive.org/web/20160407112238/http://www.nxp.com/documents/data_sheet/SL092030.pdf"

  (crc-8--general sequence #x1D #xFD nil nil #x00))
(defun crc-8/itu (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 8-bit values.

Aliases: CRC-8/I-432-1.

ITU-T Recommendation I.432.1 (February 1999):
--Full mathematical description (Section 7.3.2.2, p.5)
https://www.itu.int/rec/T-REC-I.432.1/en"

  (crc-8--general sequence #x07 #x00 nil nil #x55))
(defun crc-8/lte (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 8-bit values.

3rd Generation Partnership Project (3GPP) TS 36.212 version 17.1.0 (March 2022)
— ETSI TS 136 212 version 17.1.0 (April 2022):
--Definition: Width, Poly, Init, XorOut, Residue (Section 5.1.1, pp.10–11)
--Attachment relation, defining RefIn ^ RefOut (Section 5.1.1, p.11)
https://www.etsi.org/deliver/etsi_ts/136200_136299/136212/17.01.00_60/ts_
136212v170100p.pdf

3rd Generation Partnership Project (3GPP) TS 36.321 version 17.5.0 (June 2023) —
ETSI TS 136 321 version 17.5.0 (July 2023):
--Referenced from TS 136 212 section 5.2.2.1, p.26
--Definition: RefIn, RefOut (Section 6.1.1, p.90)
https://www.etsi.org/deliver/etsi_ts/136300_136399/136321/17.05.00_60/ts_136321v170500p.pdf"

  (crc-8--general sequence #x9B #x00 nil nil #x00))
(defun crc-8/maxim (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 8-bit values.

Aliases: CRC-8/MAXIM-DOW, DOW-CRC.

Maxim Integrated (22 August 2012), Application Note 27 (PDF, HTML)
--Definition: Init, Residue (pp.3–4)
--Shift register diagram (p.3)
--Code:       8051 assembler, Pascal (pp.5–8)
--Worked examples (pp.5–9)
https://www.maximintegrated.com/en/design/technical-documents/app-notes/2/27.html"

  (crc-8--general sequence #x31 #x00   t   t #x00))
(defun crc-8/mifare-mad (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 8-bit values.

NXP Semiconductors (6 November 2018), Application note AN10787: MIFARE
Application Directory (MAD), rev.  7.4:
--Definition: Width, Poly, Init (Section 3.7, pp.9–10)
https://www.nxp.com/docs/en/application-note/AN10787.pdf"

  (crc-8--general sequence #x1D #xC7 nil nil #x00))
(defun crc-8/nrsc-5 (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 8-bit values.

iBiquity Digital Corporation (16 December 2016), HD Radio™ Air Interface Design
Description: Audio Transport, rev.  H (courtesy of National Radio Systems
Committee):
--Definition: Width, Poly (Section 5.2.3.2, p.33)
https://www.nrscstandards.org/standards-and-guidelines/documents/standards/nrsc-5-d/reference-docs/1017s.pdf"

  (crc-8--general sequence #x31 #xFF nil nil #x00))
(defun crc-8/opensafety (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 8-bit values.

Ethernet POWERLINK Standardisation Group (18 October 2018), OpenSAFETY Safety
Profile Specification Working Draft Proposal, version 1.5.2:
--Full description (Section 5.1.2.6, pp.43–4)
--Error detection capability (Section 8.1, p.182)
--Code: C (Appendix 1, pp.196–9)
https://www.ethernet-powerlink.org/fileadmin/user_upload/Dokumente/Downloads/TECHNICAL_DOCUMENTS/EPSG_WDP_304_V-1-5-2.pdf"

  (crc-8--general sequence #x2F #x00 nil nil #x00))
(defun crc-8/rohc (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 8-bit values.

IETF RFC 3095 (July 2001):
--Definition: Width, Poly, Init, XorOut (Section 5.9.1, p.125)
https://datatracker.ietf.org/doc/html/rfc3095"

  (crc-8--general sequence #x07 #xFF   t   t #x00))
(defun crc-8/sae-j1850 (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 8-bit values.

AUTOSAR (24 November 2022), AUTOSAR Classic Platform release R22-11,
Specification of CRC Routines:
--Comprehensive primer on CRC theory (Section 7.1, pp.19–22)
--All parameters (Section 7.2.1.1, p.24)
https://www.autosar.org/fileadmin/user_upload/standards/classic/22-11/AUTOSAR_SWS_CRCLibrary.pdf"

  (crc-8--general sequence #x1D #xFF nil nil #xFF))
(defun crc-8/wcdma (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 8-bit values.

Andrew Richardson (17 March 2005), WCDMA Design Handbook, Cambridge University
Press, ISBN 0-521-82815-5 (embedded content):
--Definition: Width, Poly, Residue (Section 7.1.3, Table 7.3, p.223)
--Shift register diagram (Figure 7.4, p.223)
https://books.google.com/books?id=yN5lve5L4vwC&pg=PA223&source=gbs_selected_pages&cad=2"

  (crc-8--general sequence #x9B #x00   t   t #x00))

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
