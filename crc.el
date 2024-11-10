;;; crc.el --- Cyclic Redundancy Check               -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Jean Libète

;; Author: Jean Libète <tomenzgg@mail.mayfirst.org>
;; Maintainer: Jean Libète <tomenzgg@mail.mayfirst.org>
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

;; All references to documentation/implementations are thanks to the Catalogue
;; of Parametrised CRC Algorithms: https://reveng.sourceforge.io/crc-catalogue/

;;; Code:
;; seq-reduce
(require 'seq)

(defun crc--reverse-bits (integer number-of-bits)
  "Reverse the bits of INTEGER, starting from the right, by the NUMBER-OF-BITS."
  (declare (pure t) (side-effect-free t))

  (let ((maxindex (1- number-of-bits)) (result 0))
    (dotimes (i (1+ maxindex))
      (setq result (if (zerop (logand integer (expt 2 i)))
                       result
                     (logior result (expt 2 (- maxindex i))))))

    result))
(defun crc--truncate-by-bits (integer-to-truncate bits-to-truncate-by)
  "Truncate a number to a measure of bytes using a number of bits.

The reason bits are used to determine truncation amount is that some General
Cyclic Redundancy Checks use a bit amount that's not a full byte(s).

Leaving calculating the number of bytes to truncate a number to with this
function makes it trivial to just pass the number of bits a General Cyclic
Redundancy Check is handling to this function.

BITS-TO-TRUNCATE-BY is computed to a factor of 8 bits so that the resulting
number is the mininimal number of non-fractional bytes that the specified
number of bits could be (with 1s in every place-value of the computed number
when said number is in binary form).

INTEGER-TO-TRUNCATE is, then, bitwise-and–ed against this computed number so
that INTEGER-TO-TRUNCATE is truncated to the smallest number of non-fractional
bytes that is possible while ensuring it has at least the number of bits
specified by BITS-TO-TRUNCATE-BY.

Both INTEGER-TO-TRUNCATE and BITS-TO-TRUNCATE-BY are integers."
  (declare (pure t) (side-effect-free t))

  (logand integer-to-truncate (1- (expt 2 (* (/ bits-to-truncate-by 8) 8)))))
(defun crc--general-reducer-inner (number-of-bits polynomial)
  "Returns the logic for the inner `seq-reduce' of `crc--general' as a `lambda'.

NUMBER-OF-BITS and POLYNOMIAL are integers."
  (declare (pure t) (side-effect-free t))

  (lambda (result _bit-index)
    (let ((shift1 (ash result 1)))
      (if (zerop (logand result (expt 2 (1- number-of-bits))))
          shift1
        (logxor shift1 polynomial)))))
(defun crc--general-reducer-outer (number-of-bits polynomial ref-in)
  "Returns the logic for the outer `seq-reduce' of `crc--general' as a `lambda'.

NUMBER-OF-BITS and POLYNOMIAL are integers.

REF-IN is a boolean."
  (declare (pure t) (side-effect-free t))

  (lambda (result byte-of-sequence)
    (seq-reduce (crc--general-reducer-inner number-of-bits polynomial)
                (number-sequence 0 7)
                (crc--truncate-by-bits (logxor result
                                               (if ref-in
                                                   (crc--reverse-bits byte-of-sequence
                                                                      number-of-bits)
                                                 (ash byte-of-sequence
                                                      (- number-of-bits 8))))
                                       ;; long sequences cause an overflow error
                                       ;; by the `ash' for 'shift1' of
                                       ;; `crc--general-reducer-inner'; so
                                       ;; truncate enough to not do that but,
                                       ;; also, not lose data
                                       (* number-of-bits 2)))))
(defun crc--general (sequence number-of-bits polynomial init ref-in ref-out xor-out)
  "General Cyclic Redundancy Check application with customizations (via arg.s).

Because there are varying versions of CRC – depending on the Number of Bits,
Polynomial, Initialization value, Reflect-Input and Reflect-Output, and the XOR
Output –, this function serves to handle any possible iteration that needs to be
computed.

NUMBER-OF-BITS is an integer (such as 8, 16, 32, etc.).

SEQUENCE is a list, vector, or string.

POLYNOMIAL and INIT are integers.

REF-IN and REF-OUT are booleans.

XOR-OUT is a integer."

  (let ((reduced (seq-reduce (crc--general-reducer-outer number-of-bits
                                                         polynomial
                                                         ref-in)
                             (if (stringp sequence)
                                 (encode-coding-string sequence 'binary)
                               sequence)
                             init)))
    (when ref-out (setq reduced (crc--reverse-bits reduced number-of-bits)))

    (crc--truncate-by-bits (logxor reduced xor-out) number-of-bits)))

(defun crc-8 (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 8-bit value.

Aliases: CRC-8/SMBUS.

The System Management Interface Forum, Inc. (3 August 2000), System Management
Bus (SMBus) Specification, version 2.0:
--Definition: Width, Poly, RefIn/Out (Section 4.2, p.19; Section 5.4.1.3, p.27)
https://web.archive.org/web/20230605175600/http://www.smbus.org/specs/smbus20.pdf"

  (crc--general sequence 8 #x07 #x00 nil nil #x00))
(defun crc-8/autosar (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 8-bit value.

Aliases: CRC-8/8H2F.

AUTOSAR (24 November 2022), AUTOSAR Classic Platform release R22-11:
https://www.autosar.org/fileadmin/user_upload/standards/classic/22-11/AUTOSAR_SWS_CRCLibrary.pdf"

  (crc--general sequence 8 #x2F #xFF nil nil #xFF))
(defun crc-8/bluetooth (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 8-bit value.

Bluetooth SIG (31 January 2023), Bluetooth Specification:
https://www.bluetooth.org/DocMan/handlers/DownloadDoc.ashx?doc_id=521059"

  (crc--general sequence 8 #xA7 #x00   t   t #x00))
(defun crc-8/cdma2000 (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 8-bit value.

3rd Generation Partnership Project 2 (3GPP2) (September 2009), Physical layer
standard for cdma2000 spread spectrum systems, revision E, version 1.0:
https://web.archive.org/web/20230601141629/https://3gpp2.org/Public_html/Specs/C.S0002-E_v1.0_cdma200_1x_PHY-090925.pdf"

  (crc--general sequence 8 #x9B #xFF nil nil #x00))
(defun crc-8/darc (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 8-bit value.

ETSI EN 300 751 version 1.2.1 (January 2003):
https://www.etsi.org/deliver/etsi_en/300700_300799/300751/01.02.01_60/en_300751v010201p.pdf"

  (crc--general sequence 8 #x39 #x00   t   t #x00))
(defun crc-8/dvb-s2 (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 8-bit value.

Ethernet POWERLINK Standardisation Group (18 October 2018), OpenSAFETY Safety
Profile Specification Working Draft Proposal, version 1.5.2:
https://www.ethernet-powerlink.org/fileadmin/user_upload/Dokumente/Downloads/TECHNICAL_DOCUMENTS/EPSG_WDP_304_V-1-5-2.pdf"

  (crc--general sequence 8 #xD5 #x00 nil nil #x00))
(defun crc-8/ebu (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 8-bit value.

Aliases: CRC-8/AES and CRC-8/TECH-3250.

European Broadcasting Union (8 June 2004), Tech 3250: Specification of the
digital audio interface, 3rd edition:
https://tech.ebu.ch/docs/tech/tech3250.pdf"

  (crc--general sequence 8 #x1D #xFF   t   t #x00))
(defun crc-8/gsm-a (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 8-bit value.

Aliases: CRC-8/SAE-J1850-ZERO.

ETSI TS 100 909 version 8.9.0 (January 2005) (Full mathematical description in
Section 3.1.1.1, pp.17–18):
https://www.etsi.org/deliver/etsi_ts/100900_100999/100909/08.09.00_60/ts_100909v080900p.pdf"

  (crc--general sequence 8 #x1D #x00 nil nil #x00))
(defun crc-8/gsm-b (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 8-bit value.

ETSI TS 100 909 version 8.9.0 (January 2005) (Full mathematical description in
Section 5.1.5.1.3, p.71 — Section 5.1.5.2.2, p.73 — Section 5.1.9.1.3, p.80 —
Section 5.1.9.2.2, p.83 — Section 5.1.11.1.3, pp.86–7 — Section 5.1.11.2.2,
p.89):
https://www.etsi.org/deliver/etsi_ts/100900_100999/100909/08.09.00_60/ts_100909v080900p.pdf"

  (crc--general sequence 8 #x49 #x00 nil nil #xFF))
(defun crc-8/hitag (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 8-bit value.

Philips Semiconductors (18 July 2006), HTS IC H32/HTS IC H56/HTS IC H48
transponder IC Product Specification, revision 3.1:
--Definition: Width, Poly, Init (Section 13.1, p.45 — Section 13.2.2, p.45)
--Code:       C (Section 13.3, p.46)
https://media.digikey.com/pdf/Data%20Sheets/NXP%20PDFs/HTS.pdf"

  (crc--general sequence 8 #x1D #xFF nil nil #x00))
(defun crc-8/i-code (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 8-bit value.

Philips Semiconductors (30 January 2004), SL2 ICS11 Product Specification,
revision 3.0:
--Definition: Width, Poly, Init (Section 10.1, p.26)
--Code:       C (Section 10.3.1, pp.27–9)
--Worked example (as code trace) (Section 10.3.1, p.29)
https://web.archive.org/web/20160407112238/http://www.nxp.com/documents/data_sheet/SL092030.pdf"

  (crc--general sequence 8 #x1D #xFD nil nil #x00))
(defun crc-8/itu (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 8-bit value.

Aliases: CRC-8/I-432-1.

ITU-T Recommendation I.432.1 (February 1999):
--Full mathematical description (Section 7.3.2.2, p.5)
https://www.itu.int/rec/T-REC-I.432.1/en"

  (crc--general sequence 8 #x07 #x00 nil nil #x55))
(defun crc-8/lte (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 8-bit value.

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

  (crc--general sequence 8 #x9B #x00 nil nil #x00))
(defun crc-8/maxim (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 8-bit value.

Aliases: CRC-8/MAXIM-DOW, DOW-CRC.

Maxim Integrated (22 August 2012), Application Note 27 (PDF, HTML)
--Definition: Init, Residue (pp.3–4)
--Shift register diagram (p.3)
--Code:       8051 assembler, Pascal (pp.5–8)
--Worked examples (pp.5–9)
https://www.maximintegrated.com/en/design/technical-documents/app-notes/2/27.html"

  (crc--general sequence 8 #x31 #x00   t   t #x00))
(defun crc-8/mifare-mad (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 8-bit value.

NXP Semiconductors (6 November 2018), Application note AN10787: MIFARE
Application Directory (MAD), rev.  7.4:
--Definition: Width, Poly, Init (Section 3.7, pp.9–10)
https://www.nxp.com/docs/en/application-note/AN10787.pdf"

  (crc--general sequence 8 #x1D #xC7 nil nil #x00))
(defun crc-8/nrsc-5 (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 8-bit value.

iBiquity Digital Corporation (16 December 2016), HD Radio™ Air Interface Design
Description: Audio Transport, rev.  H (courtesy of National Radio Systems
Committee):
--Definition: Width, Poly (Section 5.2.3.2, p.33)
https://www.nrscstandards.org/standards-and-guidelines/documents/standards/nrsc-5-d/reference-docs/1017s.pdf"

  (crc--general sequence 8 #x31 #xFF nil nil #x00))
(defun crc-8/opensafety (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 8-bit value.

Ethernet POWERLINK Standardisation Group (18 October 2018), OpenSAFETY Safety
Profile Specification Working Draft Proposal, version 1.5.2:
--Full description (Section 5.1.2.6, pp.43–4)
--Error detection capability (Section 8.1, p.182)
--Code: C (Appendix 1, pp.196–9)
https://www.ethernet-powerlink.org/fileadmin/user_upload/Dokumente/Downloads/TECHNICAL_DOCUMENTS/EPSG_WDP_304_V-1-5-2.pdf"

  (crc--general sequence 8 #x2F #x00 nil nil #x00))
(defun crc-8/rohc (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 8-bit value.

IETF RFC 3095 (July 2001):
--Definition: Width, Poly, Init, XorOut (Section 5.9.1, p.125)
https://datatracker.ietf.org/doc/html/rfc3095"

  (crc--general sequence 8 #x07 #xFF   t   t #x00))
(defun crc-8/sae-j1850 (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 8-bit value.

AUTOSAR (24 November 2022), AUTOSAR Classic Platform release R22-11,
Specification of CRC Routines:
--Comprehensive primer on CRC theory (Section 7.1, pp.19–22)
--All parameters (Section 7.2.1.1, p.24)
https://www.autosar.org/fileadmin/user_upload/standards/classic/22-11/AUTOSAR_SWS_CRCLibrary.pdf"

  (crc--general sequence 8 #x1D #xFF nil nil #xFF))
(defun crc-8/wcdma (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 8-bit value.

Andrew Richardson (17 March 2005), WCDMA Design Handbook, Cambridge University
Press, ISBN 0-521-82815-5 (embedded content):
--Definition: Width, Poly, Residue (Section 7.1.3, Table 7.3, p.223)
--Shift register diagram (Figure 7.4, p.223)
https://books.google.com/books?id=yN5lve5L4vwC&pg=PA223&source=gbs_selected_pages&cad=2"

  (crc--general sequence 8 #x9B #x00   t   t #x00))

(defun crc-10 (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 8-bit value.

Aliases: CRC-10/ATM, CRC-10/I-610.

ITU-T Recommendation I.610 (February 1999):
--Full mathematical description (Section 10.1, p.40)
https://www.itu.int/rec/T-REC-I.610/en"

  (crc--general sequence 10 #x233 #x000 nil nil #x000))
(defun crc-10/cdma2000 (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 8-bit value.

3rd Generation Partnership Project 2 (3GPP2) (September 2009), Physical layer
standard for cdma2000 spread spectrum systems, revision E, version 1.0:
--Definition: Width, Poly (Section 2.1.3.1.4.1.3, p.2-96)
--Full description (Section 2.1.3.1.4.1, p.2-95)
--Shift register diagram (Figure 2.1.3.1.4.1.3-1, p.2-96)
https://web.archive.org/web/20230601141629/https://3gpp2.org/Public_html/Specs/C.S0002-E_v1.0_cdma200_1x_PHY-090925.pdf"

  (crc--general sequence 10 #x3D9 #x3FF nil nil #x000))
(defun crc-10/gsm (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 8-bit value.

ETSI TS 100 909 version 8.9.0 (January 2005):
--Full mathematical description (Section 4.7, p.65)
https://www.etsi.org/deliver/etsi_ts/100900_100999/100909/08.09.00_60/ts_100909v080900p.pdf"

  (crc--general sequence 10 #x175 #x000 nil nil #x3FF))

(defun crc-11 (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 8-bit value.

Aliases: CRC-11/FLEXRAY.

FlexRay Consortium (October 2010), FlexRay Communications System Protocol
Specification, version 3.0.1:
--Definition: Width, Poly, Init, RefOut (Section 4.2.8, pp.114–5)
--Pseudocode (Section 4.5, pp.118–9)
https://svn.ipd.kit.edu/nlrp/public/FlexRay/FlexRay%E2%84%A2%20Protocol%20Specification%20Version%203.0.1.pdf"

  (crc--general sequence 11 #x385 #x01A nil nil #x000))
(defun crc-11/umts (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 8-bit value.

3rd Generation Partnership Project (3GPP) TS 25.427 version 17.0.0 (April
2022) — ETSI TS 125 427 version 17.0.0 (April 2022):
--Definition: Width, Poly, Init, XorOut (Section 7.2.1, p.40)
--Definition: RefIn, RefOut (Section 6.1.1, p.20; Section 7.2.1.1, pp.40–1)
https://www.etsi.org/deliver/etsi_ts/125400_125499/125427/17.00.00_60/ts_125427v170000p.pdf"

  (crc--general sequence 11 #x307 #x000 nil nil #x000))

(defun crc-12/cdma2000 (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 8-bit value.

3rd Generation Partnership Project 2 (3GPP2) (September 2009), Physical layer
standard for cdma2000 spread spectrum systems, revision E, version 1.0:
--Definition: Width, Poly (Section 2.1.3.1.4.1.2, p.2-95)
--Full description (Section 2.1.3.1.4.1, p.2-95)
--Shift register diagram (Figure 2.1.3.1.4.1.2-1, p.2-96)
https://web.archive.org/web/20230601141629/https://3gpp2.org/Public_html/Specs/C.S0002-E_v1.0_cdma200_1x_PHY-090925.pdf"

  (crc--general sequence 12 #xF13 #xFFF nil nil #x000))
(defun crc-12/dect (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 8-bit value.

Aliases: X-CRC-12.

ETSI EN 300 175-3 version 2.5.1 (August 2013):
--Definition: Residue; full mathematical desc.  (Section 6.2.5.4, pp.99–101)
https://www.etsi.org/deliver/etsi_en/300100_300199/30017503/02.05.01_60/en_30017503v020501p.pdf"

  (crc--general sequence 12 #x80F #x000 nil nil #x000))
(defun crc-12/gsm (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 8-bit value.

ETSI TS 100 909 version 8.9.0 (January 2005):
--Full mathematical description (Section 5.1.5.1.4, p.72 — Section 5.1.6.1.4,
    p.75 — Section 5.1.7.1.4, p.77 — Section 5.1.8.1.4, p.78 — Section
    5.1.9.1.4, p.81 — Section 5.1.10.1.4, p.85 — Section 5.1.11.1.4, p.87 —
    Section 5.1.12.1.4, p.91 — Section 5.1.13.1.4, p.93)
https://www.etsi.org/deliver/etsi_ts/100900_100999/100909/08.09.00_60/ts_100909v080900p.pdf"

  (crc--general sequence 12 #xD31 #x000 nil nil #xFFF))
(defun crc-12/umts (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 8-bit value.

Aliases: CRC-12/3GPP.

3rd Generation Partnership Project (3GPP) TS 25.212 version 17.0.0 (March 2022)
— ETSI TS 125 212 version 17.0.0 (May 2025):
--Mathematical description, defining Width, Poly, Init, Residue (Section
    4.2.1.1, pp.19–20)
--Attachment relation, defining RefIn ^ RefOut (Section 4.2.1.2, p.20)
https://www.etsi.org/deliver/etsi_ts/125200_125299/125212/17.00.00_60/ts_125212v170000p.pdf"

  (crc--general sequence 12 #x80F #x000 nil   t #x000))

(defun crc-13/bbc (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 8-bit value.

S. R. Ely, D. T. Wright, British Broadcasting Corporation (March 1982), L.F.
Radio-data: specification of the BBC experimental transmissions 1982:
--Full mathematical description (Section 5.3, pp.4–5)
--Definition of bit order (Section 5.2, p.4)
--Worked example (as generator matrix) (Figure 10, p.10)
https://downloads.bbc.co.uk/rd/pubs/reports/1982-02.pdf"

  (crc--general sequence 13 #x1CF5 #x0000 nil nil #x0000))

(defun crc-14/darc (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 8-bit value.

ETSI EN 300 751 version 1.2.1 (January 2003):
--Definition: Width, Poly (Section 11.1, p.67)
--Definition: RefIn, RefOut (Section 12, pp.69–70)
https://www.etsi.org/deliver/etsi_en/300700_300799/300751/01.02.01_60/en_300751v010201p.pdf"

  (crc--general sequence 14 #x0805 #x0000   t   t #x0000))
(defun crc-14/gsm (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 8-bit value.

ETSI TS 100 909 version 8.9.0 (January 2005):
--Full mathematical description (Section 3.9.1.2, p.28 — Section 3.9.5.2, p.40 —
    Section 3.10.1.2, p.42 — Section 3.10.9.2, p.54)
https://www.etsi.org/deliver/etsi_ts/100900_100999/100909/08.09.00_60/ts_100909v080900p.pdf"

  (crc--general sequence 14 #x202D #x0000 nil nil #x3fff))

(defun crc-15 (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 8-bit value.

Aliases: CRC-15/CAN.

Robert Bosch GmbH (September 1991), CAN 2.0 Specification:
--Full definition (except Check) (Section 3.1.1, pp.13–14)
--Pseudocode
https://github.com/ThrowTheSwitch/pcan-ruby/raw/master/doc/can2spec.pdf"

  (crc--general sequence 15 #x4599 #x0000 nil nil #x0000))
(defun crc-15/mpt1327 (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 8-bit value.

UK Radiocommunications Agency (20 June 1997), MPT 1327: A signalling standard
for trunked private land mobile radio systems, 3rd edition:
--Full mathematical description (Section 3.2.3, p.3-3)
--Error control properties (Appendix 2, p.A2-1)
https://webarchive.nationalarchives.gov.uk/ukgwa/20160106142225/http://www.ofcom.org.uk/static/archive/ra/publication/mpt/mpt_pdf/mpt1327.pdf"

  (crc--general sequence 15 #x6815 #x0000 nil nil #x0001))

(defun crc-16 (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 16-bit value.

Aliases: ARC, CRC-16/ARC, CRC-16/LHA, CRC-IBM.

AUTOSAR (24 November 2022), AUTOSAR Classic Platform release R22-11,
Specification of CRC Routines:
--Comprehensive primer on CRC theory (Section 7.1, pp.19–22)
--All parameters (Section 7.2.2.2, p.27)
https://www.autosar.org/fileadmin/user_upload/standards/classic/22-11/AUTOSAR_SWS_CRCLibrary.pdf"

  (crc--general sequence 16 #x8005 #x0000   t   t #x0000))
(defun crc-16/cdma2000 (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 16-bit value.

3rd Generation Partnership Project 2 (3GPP2) (September 2009), Physical layer
standard for cdma2000 spread spectrum systems, revision E, version 1.0:
--Definition: Width, Poly (Section 2.1.3.1.4.1.1, p.2-95)
--Full description (Section 2.1.3.1.4.1, p.2-95)
--Shift register diagram (Figure 2.1.3.1.4.1.1-1, p.2-95)
https://web.archive.org/web/20230601141629/https://3gpp2.org/Public_html/Specs/C.S0002-E_v1.0_cdma200_1x_PHY-090925.pdf"

  (crc--general sequence 16 #xC867 #xFFFF nil nil #x0000))
(defun crc-16/cms (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 16-bit value.

\"movilstore\" (24 January 2011), Computer Interfacing Forum topic 1650:
--Reference to application of algorithm on Samsung mobiles
https://web.archive.org/web/20181116144459/https://www.lammertbies.nl/forum/viewtopic.php?t=1650"

  (crc--general sequence 16 #x8005 #xFFFF nil nil #x0000))
(defun crc-16/dds-110 (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 16-bit value.

ELV Elektronik AG (March 2008), Software-Schnittstelle der Funktionsgeneratoren
DDS 10/DDS 110 (in German):
--Definition: Width, Poly, CRC byte order (p.67)
https://web.archive.org/web/20150121125358/http://www.elv-downloads.de/downloads/journal/dds10_110.pdf"

  (crc--general sequence 16 #x8005 #x800D nil nil #x0000))
(defun crc-16/dect-r (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 16-bit value.

Aliases: R-CRC-16.

ETSI EN 300 175-3 version 2.5.1 (August 2013)
--Full mathematical description (Section 6.2.5.2, p.99)
--Performance of polynomial (Annex B, p.297)
https://www.etsi.org/deliver/etsi_en/300100_300199/30017503/02.05.01_60/en_30017503v020501p.pdf"

  (crc--general sequence 16 #x0589 #x0000 nil nil #x0001))
(defun crc-16/dect-x (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 16-bit value.

Aliases: X-CRC-16.

ETSI EN 300 175-3 version 2.5.1 (August 2013):
--Definition: Residue; mathematical description (Section 6.2.5.4, pp.99–101)
--Performance of polynomial (Annex B, p.297)
https://www.etsi.org/deliver/etsi_en/300100_300199/30017503/02.05.01_60/en_30017503v020501p.pdf"

  (crc--general sequence 16 #x0589 #x0000 nil nil #x0000))
(defun crc-16/dnp (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 16-bit value.

Lammert Bies (August 2011), CRC calculator
--Implementation
https://www.lammertbies.nl/comm/info/crc-calculation"

  (crc--general sequence 16 #x3D65 #x0000   t   t #xFFFF))
(defun crc-16/en-13757 (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 16-bit value.

Patrick Seem, Texas Instruments, Inc. (23 October 2008), AN067: Wireless MBUS
Implementation with CC1101 and MSP430:
--Definition: Width, Poly, Init, XorOut (Section 5.4, p.9)
--Describes synchronous transfer with MSBs sent first, implying RefIn and RefOut
    (Sections 7.2.1, 7.3.1, pp.13–14)
https://www.ti.com/lit/an/swra234a/swra234a.pdf"

  (crc--general sequence 16 #x3D65 #x0000 nil nil #xFFFF))
(defun crc-16/genibus (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 16-bit value.

Aliases: CRC-16/DARC, CRC-16/EPC, CRC-16/EPC-C1G2, CRC-16/I-CODE.

EPCglobal Inc™ (4 September 2018), EPC UHF Gen2 Air Interface Protocol: EPC™
Radio-Frequency Identity Protocols; Generation-2 UHF RFID Standard, release 2.1:
--Definition: Width, Poly, Init, Residue (Table 6-11, p.33)
--Definition: RefIn, Residue (Annex F.2, p.143)
--Shift register circuit diagram (Figure F-2, p.144)
--Transmission order, implying RefIn and RefOut (Section 6.3.1.4, p.33)
https://www.gs1.org/sites/default/files/docs/epc/gs1-epc-gen2v2-uhf-airinterface_i21_r_2018-09-04.pdf"

  (crc--general sequence 16 #x1021 #xFFFF nil nil #xFFFF))
(defun crc-16/gsm (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 16-bit value.

ETSI TS 100 909 version 8.9.0 (January 2005):
--Full mathematical description (Section 5.1.2.2, p.67 — Section 5.1.3.2,
    p.69 — Section 5.1.4.2, p.70)
https://www.etsi.org/deliver/etsi_ts/100900_100999/100909/08.09.00_60/ts_100909v080900p.pdf"

  (crc--general sequence 16 #x1021 #x0000 nil nil #xFFFF))
(defun crc-16/ibm-3740 (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 16-bit value.

Aliases: CRC-16/AUTOSAR, CRC-16/CCITT-FALSE.

AUTOSAR (24 November 2022), AUTOSAR Classic Platform release R22-11,
Specification of CRC Routines:
--Comprehensive primer on CRC theory (Section 7.1, pp.19–22)
--All parameters (Section 7.2.2.1, p.26)
https://www.autosar.org/fileadmin/user_upload/standards/classic/22-11/AUTOSAR_SWS_CRCLibrary.pdf"

  (crc--general sequence 16 #x1021 #xFFFF nil nil #x0000))
(defun crc-16/iso-iec-14443-3-a (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 16-bit value.

Aliases: CRC-A.

ISO/IEC FCD 14443-3 (24 November 2008):
--Definition: Init, XorOut (Section 6.2.4, p.11)
--Citation for rest of algorithm: ISO/IEC 13239 (see CRC-16/IBM-SDLC)
https://wg8.de/wg8n1496_17n3613_Ballot_FCD14443-3.pdf"

  (crc--general sequence 16 #x1021 #xC6C6   t   t #x0000))
(defun crc-16/iso-iec-14443-3-b (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 16-bit value.

Aliases: CRC-16/IBM-SDLC, CRC-16/ISO-HDLC, CRC-16/X-25, CRC-B, X-25.

ITU-T Recommendation T.30 (September 2005):
--Definition: Residue; full mathematical description (Section 5.3.7, p.78)
https://www.itu.int/rec/T-REC-T.30/en"

  (crc--general sequence 16 #x1021 #xFFFF   t   t #xFFFF))
(defun crc-16/kermit (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 16-bit value.

Aliases: CRC-16/BLUETOOTH, CRC-16/CCITT, CRC-16/CCITT-TRUE, CRC-16/V-41-LSB,
           CRC-CCITT, KERMIT.

ITU-T Recommendation V.41 (November 1988):
--Definition: Residue; full mathematical description (Section 2, p.2)
--Shift register diagrams (Appendix I, p.9)
https://www.itu.int/rec/T-REC-V.41/en"

  (crc--general sequence 16 #x1021 #x0000   t   t #x0000))
(defun crc-16/lj1200 (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 16-bit value.

Dennis Sheirer (24 January 2015), SDRTrunk module:
--Code: Java
--Worked example (as generator matrix)
https://github.com/DSheirer/sdrtrunk/blob/master/src/main/java/io/github/dsheirer/edac/CRCLJ.java"

  (crc--general sequence 16 #x6F63 #x0000 nil nil #x0000))
(defun crc-16/m17 (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 16-bit value.

M17 Project (13 July 2023), M17 protocol specification, version 1.0:
--Definition: Width, Poly, Init, RefIn, RefOut, Check (Section 2.5.4, pp.24–5;
    Table 2.9, p.25)
https://spec.m17project.org/pdf/M17_spec.pdf"

  (crc--general sequence 16 #x5935 #xFFFF nil nil #x0000))
(defun crc-16/maxim-dow (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 16-bit value.

Aliases: CRC-16/MAXIM.

Maxim Integrated (8 August 2012), DS1921G Datasheet:
--Definition: Width, Poly, Init, XorOut (p.32)
--Shift register diagram (p.34)
https://datasheets.maximintegrated.com/en/ds/DS1921G.pdf"

  (crc--general sequence 16 #x8005 #x0000   t   t #xFFFF))
(defun crc-16/mcrf4xx (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 16-bit value.

Youbok Lee, PhD, Microchip Technology Inc. (16 July 2001), \"CRC Algorithm for
MCRF45X Read/Write Device\":
--Definition: Width, Poly (reverse form) (p.1)
--Shift register diagram (p.1)
--Flowchart (p.2)
--Worked example (p.3)
--Code: C (pp.4–5)
https://ww1.microchip.com/downloads/en/AppNotes/00752a.pdf"

  (crc--general sequence 16 #x1021 #xFFFF   t   t #x0000))
(defun crc-16/modbus (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 16-bit value.

Aliases: MODBUS.

MODICON Inc. (June 1996), Modbus Protocol Reference Guide, Rev.  J:
--Algorithm (pp.112–3)
--Code: C (pp.113–5)
https://modbus.org/docs/PI_MBUS_300.pdf"

  (crc--general sequence 16 #x8005 #xFFFF   t   t #x0000))
(defun crc-16/nrsc-5 (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 16-bit value.

iBiquity Digital Corporation (23 August 2011), HD Radio™ Air Interface Design
Description: Station Information Service Transport, rev.  J (courtesy of
National Radio Systems Committee):
--Definition: Poly (mantissa only); method (Section 4.10, p.38)
https://www.nrscstandards.org/standards-and-guidelines/documents/standards/nrsc-5-d/reference-docs/1020s.pdf"

  (crc--general sequence 16 #x080B #xFFFF   t   t #x0000))
(defun crc-16/opensafety-a (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 16-bit value.

Ethernet POWERLINK Standardisation Group (18 October 2018), OpenSAFETY Safety
Profile Specification Working Draft Proposal, version 1.5.2:
--Full description (Section 5.1.2.6, pp.43–4)
--Error detection capability (Section 8.1, p.182)
--Code: C (Appendix 1, pp.196–9)
https://www.ethernet-powerlink.org/fileadmin/user_upload/Dokumente/Downloads/TECHNICAL_DOCUMENTS/EPSG_WDP_304_V-1-5-2.pdf"

  (crc--general sequence 16 #x5935 #x0000 nil nil #x0000))
(defun crc-16/opensafety-b (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 16-bit value.

Ethernet POWERLINK Standardisation Group (18 October 2018), OpenSAFETY Safety
Profile Specification Working Draft Proposal, version 1.5.2:
--Full description (Section 5.1.2.6, pp.43–4)
--Error detection capability (Section 8.1, p.182)
--Code: C (Appendix 1, pp.196–9)
https://www.ethernet-powerlink.org/fileadmin/user_upload/Dokumente/Downloads/TECHNICAL_DOCUMENTS/EPSG_WDP_304_V-1-5-2.pdf"

  (crc--general sequence 16 #x755B #x0000 nil nil #x0000))
(defun crc-16/profibus (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 16-bit value.

Aliases: CRC-16/IEC-61158-2.

PROFIBUS International (March 1998), PROFIBUS Specification, Normative Parts,
Edition 1.0, Part 9:
--Transmission order, implying RefIn and RefOut (Part 9 Section 8.5.1, p.902)
--Definition: Width, Poly, Init, Residue (Part 9 Section 8.8.4, pp.905–7)
--Properties of polynomial (Part 9 Section 8.8.4, p.906)
https://web.archive.org/web/20081116195826/https://www.kuebler.com/PDFs/Feldbus_Multiturn/specification_DP.pdf"

  (crc--general sequence 16 #x1DCF #xFFFF nil nil #xFFFF))
(defun crc-16/riello (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 16-bit value.

\"Snamprogetti\" (17 April 2009), Computer Interfacing Forum topic 1305:
https://web.archive.org/web/20181116155838/https://www.lammertbies.nl/forum/viewtopic.php?t=1305"

  (crc--general sequence 16 #x1021 #xB2AA   t   t #x0000))
(defun crc-16/spi-fujitsu (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 16-bit value.

Aliases: CRC-16/AUG-CCITT.

Fujitsu Semiconductor (10 October 2007), FlexRay ASSP MB88121B User's Manual:
--Definition: Width, Poly, Init (Section 2.5.1, p.153)
https://web.archive.org/web/20221017013602/https://www.fujitsu.com/downloads/MICRO/fma/pdfmcu/um-mb88121-am15-11201-1e.pdf"

  (crc--general sequence 16 #x1021 #x1D0F nil nil #x0000))
(defun crc-16/t10-dif (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 16-bit value.

Gerald Houlder, INCITS Technical Committee T10 (2 May 2003), End-to-End Data
Protection Proposal:
--Definition: Poly, Init (p.6)
--Shift register diagram (p.18)
https://www.t10.org/ftp/t10/document.03/03-111r0.pdf"

  (crc--general sequence 16 #x8BB7 #x0000 nil nil #x0000))
(defun crc-16/teledisk (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 16-bit value.

Will Kranz (8 December 2002), wteledsk 1.01:
--Implementation
https://github.com/jmechnich/wteledsk"

  (crc--general sequence 16 #xA097 #x0000 nil nil #x0000))
(defun crc-16/tms37157 (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 16-bit value.

Texas Instruments, Inc. (November 2009), TMS37157 datasheet:
--Full definition (except Check) (pp.39–40)
--Shift register diagram (Figure 51, p.39)
--Flowchart (Figure 52, p.40)
https://www.ti.com/lit/ds/symlink/tms37157.pdf"

  (crc--general sequence 16 #x1021 #x89EC   t   t #x0000))
(defun crc-16/umts (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 16-bit value.

Aliases: CRC-16/BUYPASS, CRC-16/VERIFONE.

Verifone, Inc. (May 1995), TCLOAD Reference Manual:
--Definition: Poly.  CRC byte order, implying RefIn/RefOut (Section 4, p.4-1)
https://web.archive.org/web/20120603221525/http://www.verifone.com/PDF/guides/tcl_ref.pdf"

  (crc--general sequence 16 #x8005 #x0000 nil nil #x0000))
(defun crc-16/usb (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 16-bit value.

Anonymous (10 July 1997), \"Cyclic Redundancy Checks in USB\" (Draft):
--Definition: Width, Poly, Init, XorOut (p.2)
--Definition: Residue (p.3)
--Code: Perl (p.4)
https://web.archive.org/web/20160326215031/http://www.usb.org/developers/whitepapers/crcdes.pdf"

  (crc--general sequence 16 #x8005 #xFFFF   t   t #xFFFF))
(defun crc-16/xmodem (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 16-bit value.

Aliases: CRC-16/ACORN, CRC-16/LTE, CRC-16/V-41-MSB, XMODEM, ZMODEM.

ITU-T Recommendation V.41 (November 1988):
--Definition: Residue; full mathematical description (Section 2, p.2)
--Shift register diagrams (Appendix I, p.9)
https://www.itu.int/rec/T-REC-V.41/en"

  (crc--general sequence 16 #x1021 #x0000 nil nil #x0000))
(defun crc-16/xmodem-2 (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 16-bit value.

https://www.lddgo.net/en/encrypt/crc"

  (crc--general sequence 16 #x8408 #x0000   t   t #x0000))

(defun crc-17/can-fd (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 16-bit value.

Robert Bosch GmbH (17 April 2012), CAN with Flexible Data-Rate: Specification,
version 1.0:
--Full definition (except Check); pseudocode (Section 3.2.1, pp.13–14)
https://web.archive.org/web/20151017122935/http://www.bosch-semiconductors.de/media/pdf_1/canliteratur/can_fd_spec.pdf"

  (crc--general sequence 17 #x1685b #x00000 nil nil #x00000))

(defun crc-21/can-fd (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 16-bit value.

Robert Bosch GmbH (17 April 2012), CAN with Flexible Data-Rate: Specification,
version 1.0:
--Full definition (except Check); pseudocode (Section 3.2.1, pp.13–14)
https://reveng.sourceforge.io/crc-catalogue/17plus.htm#crc.cat-bits.32"

  (crc--general sequence 21 #x102899 #x000000 nil nil #x000000))

(defun crc-24 (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 24-bit value.

Aliases: CRC-24/OPENPGP.

IETF RFC 4880 (November 2007):
--Definition: Width, Poly, Init (Section 6, p.53)
--Code: C (Section 6.1, p.54)
https://datatracker.ietf.org/doc/html/rfc4880"

  (crc--general sequence 24 #x864CFB #xB704CE nil nil #x000000))
(defun crc-24/ble (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 24-bit value.

Bluetooth SIG (31 January 2023), Bluetooth Specification, Core Version 5.4:
--Full definition (except Check) (Section 6.B.3.1.1, pp.2746–7)
--Shift register diagram (Figure 6.B.3.4, p.2747)
https://www.bluetooth.org/DocMan/handlers/DownloadDoc.ashx?doc_id=521059"

  (crc--general sequence 24 #x00065B #x555555   t   t #x000000))
(defun crc-24/flexray-a (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 24-bit value.

FlexRay Consortium (October 2010), FlexRay Communications System Protocol
Specification, version 3.0.1:
--Definition: Width, Poly, Init, RefOut (Section 4.4, pp.117–8)
--Pseudocode (Section 4.5, pp.118–20)
https://svn.ipd.kit.edu/nlrp/public/FlexRay/FlexRay%E2%84%A2%20Protocol%20Specification%20Version%203.0.1.pdf"

  (crc--general sequence 24 #x5D6DCB #xFEDCBA nil nil #x000000))
(defun crc-24/flexray-b (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 24-bit value.

FlexRay Consortium (October 2010), FlexRay Communications System Protocol
Specification, version 3.0.1:
--Definition: Width, Poly, Init, RefOut (Section 4.4, pp.117–8)
--Pseudocode (Section 4.5, pp.118–20)
https://svn.ipd.kit.edu/nlrp/public/FlexRay/FlexRay%E2%84%A2%20Protocol%20Specification%20Version%203.0.1.pdf"

  (crc--general sequence 24 #x5D6DCB #xABCDEF nil nil #x000000))
(defun crc-24/interlaken (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 24-bit value.

The Interlaken Alliance (7 October 2008), Interlaken Protocol Definition,
version 1.2:
--Definition: Width, Poly (Section 5.3.2.2, p.18)
--Definition: Init, RefIn, RefOut, XorOut (Appendix B, p.48)
https://4b1b46.a2cdn1.secureserver.net/wp-content/uploads/2019/12/Interlaken_Protocol_Definition_v1.2.pdf"

  (crc--general sequence 24 #x328B63 #xFFFFFF nil nil #xFFFFFF))
(defun crc-24/lte-a (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 24-bit value.

3rd Generation Partnership Project (3GPP) TS 36.212 version 17.1.0 (March
2022) — ETSI TS 136 212 version 17.1.0 (April 2022):
--Definition: Width, Poly, Init, XorOut, Residue (Section 5.1.1, pp.10–11)
--Attachment relation, defining RefIn ^ RefOut (Section 5.1.1, p.11)
https://www.etsi.org/deliver/etsi_ts/136200_136299/136212/17.01.00_60/ts_136212v170100p.pdf"

  (crc--general sequence 24 #x864CFB #x000000 nil nil #x000000))
(defun crc-24/lte-b (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 24-bit value.

3rd Generation Partnership Project (3GPP) TS 36.212 version 17.1.0 (March 2022)
— ETSI TS 136 212 version 17.1.0 (April 2022):
--Definition: Width, Poly, Init, XorOut, Residue (Section 5.1.1, pp.10–11)
--Attachment relation, defining RefIn ^ RefOut (Section 5.1.1, p.11)
https://www.etsi.org/deliver/etsi_ts/136200_136299/136212/17.01.00_60/ts_136212v170100p.pdf"

  (crc--general sequence 24 #x800063 #x000000 nil nil #x000000))
(defun crc-24/os-9 (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 24-bit value.

Microware Systems Corporation (January 1983), OS-9 Operating System, System
Programmer's Manual, revision F-1:
--Definition: Width, Poly, Init, XorOut, Residue (Section 10.1.4, pp.51–2)
--Code: C (Section 10.1.4, p.52)
https://www.roug.org/soren/6809/os9sysprog.html"

  (crc--general sequence 24 #x800063 #xFFFFFF nil nil #xFFFFFF))

(defun crc-30/cdma (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 24-bit value.

CCSA Telecommunication Industry Standard YD/T 1838.3-2008 (5 November 2008):
--Full definition (except Check, Residue) (Section 2.1.1.5.1.2, pp.2-44–5)
https://web.archive.org/web/20181118221951/https://www.ccsa.org.cn/english/yd1838/Technical%20Requirements%20of%20Link%20Access%20Control%20(LAC)%20for%20the%20CDMA-based%20Digital%20Trunking%20Mobile%20Communication%20System.pdf"

  (crc--general sequence 30 #x2030B9C7 #x3FFFFFFF nil nil #x3FFFFFFF))

(defun crc-31/philips (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 24-bit value.

Philips 37PF9731 LCD TV:
--Implementation

\"gigirex\" (1 March 2012), Computer Interfacing Forum topic 1774:
https://web.archive.org/web/20181116141051/https://www.lammertbies.nl/forum/viewtopic.php?t=1774"

  (crc--general sequence 31 #x04C11DB7 #x7FFFFFFF nil nil #x7FFFFFFF))

(defun crc-32 (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 32-bit value.

Aliases: CRC-32/ADCCP, CRC-32/ISO-HDLC, CRC-32/V-42, CRC-32/XZ, PKZIP.

ITU-T Recommendation V.42 (March 2002):
--Definition: Residue; full mathematical description (Section 8.1.1.6.2, p.17)
https://www.itu.int/rec/T-REC-V.42/en"

  (crc--general sequence 32 #x04C11DB7 #xFFFFFFFF   t   t #xFFFFFFFF))
(defun crc-32/aixm (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 32-bit value.

Aliases: CRC-32Q.

EUROCONTROL (20 March 2006), AIXM Primer 4.5:
--Definition: Width, Poly, Init, XorOut, RefIn (Section 6.2, pp.23–5)
--Flowchart (p.24)
--Code: Java (Appendix B, pp.41–5)
https://www.aixm.aero/sites/aixm.aero/files/imce/library/aixm_primer_4-5.pdf"

  (crc--general sequence 32 #x814141AB #x00000000 nil nil #x00000000))
(defun crc-32/autosar (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 32-bit value.

AUTOSAR (24 November 2022), AUTOSAR Classic Platform release R22-11,
Specification of CRC Routines:
--Comprehensive primer on CRC theory (Section 7.1, pp.19–22)
--All parameters (Section 7.2.3.2, pp.28–9)
https://www.autosar.org/fileadmin/user_upload/standards/classic/22-11/AUTOSAR_SWS_CRCLibrary.pdf"

  (crc--general sequence 32 #xF4ACFB13 #xFFFFFFFF   t   t #xFFFFFFFF))
(defun crc-32/base91-d (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 32-bit value.

Aliases: CRC-32D.

Prof.  Philip Koopman (July 2002), \"32-Bit Cyclic Redundancy Codes for Internet
Applications\", Proceedings of The International Conference on Dependable
Systems and Networks:
--Polynomial discovered by Castagnoli; properties confirmed by Koopman
https://users.ece.cmu.edu/~koopman/networks/dsn02/dsn02_koopman.pdf"

  (crc--general sequence 32 #xA833982B #xFFFFFFFF   t   t #xFFFFFFFF))
(defun crc-32/bzip2 (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 32-bit value.

Aliases: CRC-32/AAL5, CRC-32/DECT-B, B-CRC-32.

ITU-T Recommendation I.363.5 (August 1996):
--Definition: Residue; full mathematical description (Section 9.2.1.2 (f), p.12)
https://www.itu.int/rec/T-REC-I.363.5/en"

  (crc--general sequence 32 #x04C11DB7 #xFFFFFFFF nil nil #xFFFFFFFF))
(defun crc-32/cd-rom-edc (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 32-bit value.

ECMA standard ECMA-130, edition 2 (June 1996) — ISO/IEC 10149:1995:
--Definition: Width, Poly, Refin, RefOut, Residue (Section 14.3, p.16)
https://www.ecma-international.org/wp-content/uploads/ECMA-130_2nd_edition_june_1996.pdf"

  (crc--general sequence 32 #x8001801B #x00000000   t   t #x00000000))
(defun crc-32/cksum (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 32-bit value.

Aliases: CKSUM, CRC-32/POSIX.

The Open Group (1997), Single Unix Specification, version 2, Commands &
Utilities Issue 5, Reference Pages: cksum:
--Full definition (except Check, Residue)
https://pubs.opengroup.org/onlinepubs/7990989775/xcu/cksum.html"

  (crc--general sequence 32 #x04C11DB7 #x00000000 nil nil #xFFFFFFFF))
(defun crc-32/iscsi (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 32-bit value.

Aliases: CRC-32/BASE91-C, CRC-32/CASTAGNOLI, CRC-32/INTERLAKEN, CRC-32C.

Used by Btrfs, Ext4, iSCSI, and SCTP, amongst others.

IETF RFC 7143 (April 2014):
--Full definition (except Check) (Section 13.1, pp.230–1)
https://datatracker.ietf.org/doc/html/rfc7143"

  (crc--general sequence 32 #x1EDC6F41 #xFFFFFFFF   t   t #xFFFFFFFF))
(defun crc-32/jamcrc (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 32-bit value.

Aliases: JAMCRC.

Altera Corporation (April 1999), crc MegaCore Function Data Sheet, version 2:
--All parameters (except Residue) (p.6)
https://web.archive.org/web/20070221144121/http://www.msc.rl.ac.uk/europractice/vendors/dscrc.pdf"

  (crc--general sequence 32 #x04C11DB7 #xFFFFFFFF   t   t #x00000000))
(defun crc-32/koopman (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 32-bit value.

https://www.lddgo.net/en/encrypt/crc"

  (crc--general sequence 32 #x741B8CD7 #xFFFFFFFF   t   t #xFFFFFFFF))
(defun crc-32/mef (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 32-bit value.

Mayo Foundation (19 February 2016), Multiscale Electrophysiology File Format
Version 3.0 (MEF3) specification:
--Definition: Init (p.90), Width (p.91)
--Citation for Poly: \"Koopman\" (p.91)
https://github.com/msel-source/meflib/raw/multiplatform/MEF%203%20Specification.pdf"

  (crc--general sequence 32 #x741B8CD7 #xFFFFFFFF   t   t #x00000000))
(defun crc-32/mpeg-2 (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 32-bit value.

ISO/IEC 13818-1:2000 — ITU-T Recommendation H.222.0 (February 2000):
--Definition: Width, Poly, Init, RefIn, RefOut, Residue (Annex A, p.97)
--CRC checking algorithm (Annex A, p.97)
--Partial shift register diagram (Annex A, p.97)"

  (crc--general sequence 32 #x04C11DB7 #xFFFFFFFF nil nil #x00000000))
(defun crc-32/sata (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 32-bit value."

  (crc--general sequence 32 #x04C11DB7 #x52325032 nil nil #x00000000))
(defun crc-32/xfer (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 32-bit value.

Aliases: XFER.

Jon Welch (12 January 2007), XFER in C, version 5.1a:
--Implementation
https://www.g7jjf.com/bbc.htm"

  (crc--general sequence 32 #x000000AF #x00000000 nil nil #x00000000))

(defun crc-40/gsm (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 40-bit value.

ETSI TS 100 909 version 8.9.0 (January 2005):
--Full mathematical description (Section 4.1.2, p.61)
https://www.etsi.org/deliver/etsi_ts/100900_100999/100909/08.09.00_60/ts_100909v080900p.pdf"

  (crc--general sequence 40 #x0004820009 #x0000000000 nil nil #xFFFFFFFFFF))

(defun crc-64 (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 64-bit value.

Aliases: CRC-64/ECMA-182.

ECMA standard ECMA-182 (December 1992) — ISO/IEC 13421:1993
--Full mathematical description (Annex B, p.51)
https://www.ecma-international.org/wp-content/uploads/ECMA-182_1st_edition_december_1992.pdf"

  (crc--general sequence           64  #x42F0E1EBA9EA3693
                #X0000000000000000 nil nil                #x0000000000000000))
(defun crc-64/go-iso (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 64-bit value.

The Go Authors, The Go Programming Language, package crc64:
--Implementation (using constant crc64.ISO)
https://pkg.go.dev/hash/crc64"

  (crc--general sequence           64  #x000000000000001B
                #xFFFFFFFFFFFFFFFF   t   t                #xFFFFFFFFFFFFFFFF))
(defun crc-64/ms (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 64-bit value.

Microsoft Corporation (25 June 2021), [MS-FCIADS]: File Classification
Infrastructure Alternate Data Stream (ADS) File Format:
--Full mathematical description (Section 2.8, pp.11–12)
https://winprotocoldoc.blob.core.windows.net/productionwindowsarchives/MS-FCIADS/%5bMS-FCIADS%5d.pdf"

  (crc--general sequence           64  #x259C84CBA6426349
                #xFFFFFFFFFFFFFFFF   t   t                #x0000000000000000))
(defun crc-64/redis (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 64-bit value.

Matt Stancliff et al.  (16 November 2021), Redis module redis/src/crc64.c:
--Definition: Width, Poly, RefIn, RefOut, XorOut, Check
--Code: C
https://github.com/redis/redis/blob/unstable/src/crc64.c"

  (crc--general sequence           64  #xAD93D23594C935A9
                #x0000000000000000   t   t                #x0000000000000000))
(defun crc-64/we (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 64-bit value.

Wolfgang Ehrhardt (27 March 2012), CRC/Hash plugin for FAR Manager:
--Implementation"

  (crc--general sequence           64  #x42F0E1EBA9EA3693
                #xFFFFFFFFFFFFFFFF nil nil                #xFFFFFFFFFFFFFFFF))
(defun crc-64/xz (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 64-bit value.

Aliases: CRC-64/GO-ECMA.

AUTOSAR (24 November 2022), AUTOSAR Classic Platform release R22-11,
Specification of CRC Routines:
--Comprehensive primer on CRC theory (Section 7.1, pp.19–22)
--All parameters (Section 7.2.4.1, pp.29–30)
https://www.autosar.org/fileadmin/user_upload/standards/classic/22-11/AUTOSAR_SWS_CRCLibrary.pdf"

  (crc--general sequence           64  #x42F0E1EBA9EA3693
                #xFFFFFFFFFFFFFFFF   t   t                #xFFFFFFFFFFFFFFFF))

(defun crc-82/darc (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to a hashed 80-bit value.

ETSI EN 300 751 version 1.2.1 (January 2003):
--Definition: Width, Poly (Section 11.1, p.67)
--Definition: RefIn, RefOut (Section 12, pp.69–70)
https://www.etsi.org/deliver/etsi_en/300700_300799/300751/01.02.01_60/en_300751v010201p.pdf"

  (crc--general sequence
                82
                #x0308C0111011401440411
                #x000000000000000000000
                  t
                  t
                #x000000000000000000000))

(provide 'crc)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; crc.el ends here
