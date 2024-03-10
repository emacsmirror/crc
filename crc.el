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
    (logxor (funcall (if ref-out #'crc--reverse-bits (lambda (n _b) n))
                     (seq-reduce (lambda (res1 byte)
                                   (seq-reduce (lambda (res2 _i)
                                                 (let ((shift1 (ash res2 1)))
                                                   (if (zerop (logand res2 #x80))
                                                       shift1
                                                     (logxor shift1 polynomial))))
                                               (number-sequence 0 7)
                                               (logxor res1
                                                       (funcall (if ref-in
                                                                    #'crc--reverse-bits
                                                                  (lambda (n _b) n))
                                                                byte
                                                                8))))
                                 sequence
                                 init)
                     8)
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

(defun crc-16--general (sequence polynomial init ref-in ref-out xor-out)
  "General Cyclic Redundancy Check, 16-bit, application with customization.

Because there are varying versions of CRC-16 – depending on the Polynomial,
Initialization value, RefIn and RefOut, and the XorOut –, this function serves
to handle any possible iteration that needs to be computed.

SEQUENCE is a list, vector, or string.

POLYNOMIAL and INIT are integers.

REF-IN and REF-OUT are booleans.

XOR-OUT is a integer."

  (logand
    (logxor (funcall (if ref-out #'crc--reverse-bits (lambda (n _b) n))
                     (seq-reduce (lambda (res1 byte)
                                   (seq-reduce (lambda (res2 _i)
                                                 (let ((shift1 (ash res2 1)))
                                                   (if (zerop (logand res2 #x8000))
                                                       shift1
                                                     (logxor shift1 polynomial))))
                                               (number-sequence 0 7)
                                               (logxor res1
                                                       (if ref-in
                                                           (crc--reverse-bits byte 16)
                                                         (ash byte 8)))))
                                 sequence
                                 init)
                     16)
            xor-out)
    #b1111111111111111))
(defun crc-16 (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 16-bit values.

Aliases: ARC, CRC-16/ARC, CRC-16/LHA, CRC-IBM.

AUTOSAR (24 November 2022), AUTOSAR Classic Platform release R22-11,
Specification of CRC Routines:
--Comprehensive primer on CRC theory (Section 7.1, pp.19–22)
--All parameters (Section 7.2.2.2, p.27)
https://www.autosar.org/fileadmin/user_upload/standards/classic/22-11/AUTOSAR_SWS_CRCLibrary.pdf"

  (crc-16--general sequence #x8005 #x0000   t   t #x0000))
(defun crc-16/cdma2000 (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 16-bit values.

3rd Generation Partnership Project 2 (3GPP2) (September 2009), Physical layer
standard for cdma2000 spread spectrum systems, revision E, version 1.0:
--Definition: Width, Poly (Section 2.1.3.1.4.1.1, p.2-95)
--Full description (Section 2.1.3.1.4.1, p.2-95)
--Shift register diagram (Figure 2.1.3.1.4.1.1-1, p.2-95)
https://web.archive.org/web/20230601141629/https://3gpp2.org/Public_html/Specs/C.S0002-E_v1.0_cdma200_1x_PHY-090925.pdf"

  (crc-16--general sequence #xC867 #xFFFF nil nil #x0000))
(defun crc-16/cms (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 16-bit values.

\"movilstore\" (24 January 2011), Computer Interfacing Forum topic 1650:
--Reference to application of algorithm on Samsung mobiles
https://web.archive.org/web/20181116144459/https://www.lammertbies.nl/forum/viewtopic.php?t=1650"

  (crc-16--general sequence #x8005 #xFFFF nil nil #x0000))
(defun crc-16/dds-110 (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 16-bit values.

ELV Elektronik AG (March 2008), Software-Schnittstelle der Funktionsgeneratoren
DDS 10/DDS 110 (in German):
--Definition: Width, Poly, CRC byte order (p.67)
https://web.archive.org/web/20150121125358/http://www.elv-downloads.de/downloads/journal/dds10_110.pdf"

  (crc-16--general sequence #x8005 #x800D nil nil #x0000))
(defun crc-16/dect-r (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 16-bit values.

Aliases: R-CRC-16.

ETSI EN 300 175-3 version 2.5.1 (August 2013)
--Full mathematical description (Section 6.2.5.2, p.99)
--Performance of polynomial (Annex B, p.297)
https://www.etsi.org/deliver/etsi_en/300100_300199/30017503/02.05.01_60/en_30017503v020501p.pdf"

  (crc-16--general sequence #x0589 #x0000 nil nil #x0001))
(defun crc-16/dect-x (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 16-bit values.

Aliases: X-CRC-16.

ETSI EN 300 175-3 version 2.5.1 (August 2013):
--Definition: Residue; mathematical description (Section 6.2.5.4, pp.99–101)
--Performance of polynomial (Annex B, p.297)
https://www.etsi.org/deliver/etsi_en/300100_300199/30017503/02.05.01_60/en_30017503v020501p.pdf"

  (crc-16--general sequence #x0589 #x0000 nil nil #x0000))
(defun crc-16/dnp (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 16-bit values.

Lammert Bies (August 2011), CRC calculator
--Implementation
https://www.lammertbies.nl/comm/info/crc-calculation"

  (crc-16--general sequence #x3D65 #x0000   t   t #xFFFF))
(defun crc-16/en-13757 (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 16-bit values.

Patrick Seem, Texas Instruments, Inc. (23 October 2008), AN067: Wireless MBUS
Implementation with CC1101 and MSP430:
--Definition: Width, Poly, Init, XorOut (Section 5.4, p.9)
--Describes synchronous transfer with MSBs sent first, implying RefIn and RefOut
    (Sections 7.2.1, 7.3.1, pp.13–14)
https://www.ti.com/lit/an/swra234a/swra234a.pdf"

  (crc-16--general sequence #x3D65 #x0000 nil nil #xFFFF))
(defun crc-16/genibus (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 16-bit values.

Aliases: CRC-16/DARC, CRC-16/EPC, CRC-16/EPC-C1G2, CRC-16/I-CODE.

EPCglobal Inc™ (4 September 2018), EPC UHF Gen2 Air Interface Protocol: EPC™
Radio-Frequency Identity Protocols; Generation-2 UHF RFID Standard, release 2.1:
--Definition: Width, Poly, Init, Residue (Table 6-11, p.33)
--Definition: RefIn, Residue (Annex F.2, p.143)
--Shift register circuit diagram (Figure F-2, p.144)
--Transmission order, implying RefIn and RefOut (Section 6.3.1.4, p.33)
https://www.gs1.org/sites/default/files/docs/epc/gs1-epc-gen2v2-uhf-airinterface_i21_r_2018-09-04.pdf"

  (crc-16--general sequence #x1021 #xFFFF nil nil #xFFFF))
(defun crc-16/gsm (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 16-bit values.

ETSI TS 100 909 version 8.9.0 (January 2005):
--Full mathematical description (Section 5.1.2.2, p.67 — Section 5.1.3.2,
    p.69 — Section 5.1.4.2, p.70)
https://www.etsi.org/deliver/etsi_ts/100900_100999/100909/08.09.00_60/ts_100909v080900p.pdf"

  (crc-16--general sequence #x1021 #x0000 nil nil #xFFFF))
(defun crc-16/ibm-3740 (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 16-bit values.

Aliases: CRC-16/AUTOSAR, CRC-16/CCITT-FALSE.

AUTOSAR (24 November 2022), AUTOSAR Classic Platform release R22-11,
Specification of CRC Routines:
--Comprehensive primer on CRC theory (Section 7.1, pp.19–22)
--All parameters (Section 7.2.2.1, p.26)
https://www.autosar.org/fileadmin/user_upload/standards/classic/22-11/AUTOSAR_SWS_CRCLibrary.pdf"

  (crc-16--general sequence #x1021 #xFFFF nil nil #x0000))
(defun crc-16/ibm-sdlc (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 16-bit values.

Aliases: CRC-16/ISO-HDLC, CRC-16/ISO-IEC-14443-3-B, CRC-16/X-25, CRC-B, X-25.

ITU-T Recommendation T.30 (September 2005):
--Definition: Residue; full mathematical description (Section 5.3.7, p.78)
https://www.itu.int/rec/T-REC-T.30/en"

  (crc-16--general sequence #x1021 #xFFFF   t   t #xFFFF))
(defun crc-16/iso-iec-14443-3-a (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 16-bit values.

Aliases: CRC-A.

ISO/IEC FCD 14443-3 (24 November 2008):
--Definition: Init, XorOut (Section 6.2.4, p.11)
--Citation for rest of algorithm: ISO/IEC 13239 (see CRC-16/IBM-SDLC)
https://wg8.de/wg8n1496_17n3613_Ballot_FCD14443-3.pdf"

  (crc-16--general sequence #x1021 #xC6C6   t   t #x0000))
(defun crc-16/kermit (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 16-bit values.

Aliases: CRC-16/BLUETOOTH, CRC-16/CCITT, CRC-16/CCITT-TRUE, CRC-16/V-41-LSB,
           CRC-CCITT, KERMIT.

ITU-T Recommendation V.41 (November 1988):
--Definition: Residue; full mathematical description (Section 2, p.2)
--Shift register diagrams (Appendix I, p.9)
https://www.itu.int/rec/T-REC-V.41/en"

  (crc-16--general sequence #x1021 #x0000   t   t #x0000))
(defun crc-16/lj1200 (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 16-bit values.

Dennis Sheirer (24 January 2015), SDRTrunk module:
--Code: Java
--Worked example (as generator matrix)
https://github.com/DSheirer/sdrtrunk/blob/master/src/main/java/io/github/dsheirer/edac/CRCLJ.java"

  (crc-16--general sequence #x6F63 #x0000 nil nil #x0000))
(defun crc-16/m17 (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 16-bit values.

M17 Project (13 July 2023), M17 protocol specification, version 1.0:
--Definition: Width, Poly, Init, RefIn, RefOut, Check (Section 2.5.4, pp.24–5;
    Table 2.9, p.25)
https://spec.m17project.org/pdf/M17_spec.pdf"

  (crc-16--general sequence #x5935 #xFFFF nil nil #x0000))
(defun crc-16/maxim-dow (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 16-bit values.

Aliases: CRC-16/MAXIM.

Maxim Integrated (8 August 2012), DS1921G Datasheet:
--Definition: Width, Poly, Init, XorOut (p.32)
--Shift register diagram (p.34)
https://datasheets.maximintegrated.com/en/ds/DS1921G.pdf"

  (crc-16--general sequence #x8005 #x0000   t   t #xFFFF))
(defun crc-16/mcrf4xx (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 16-bit values.

Youbok Lee, PhD, Microchip Technology Inc. (16 July 2001), \"CRC Algorithm for
MCRF45X Read/Write Device\":
--Definition: Width, Poly (reverse form) (p.1)
--Shift register diagram (p.1)
--Flowchart (p.2)
--Worked example (p.3)
--Code: C (pp.4–5)
https://ww1.microchip.com/downloads/en/AppNotes/00752a.pdf"

  (crc-16--general sequence #x1021 #xFFFF   t   t #x0000))
(defun crc-16/modbus (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 16-bit values.

Aliases: MODBUS.

MODICON Inc. (June 1996), Modbus Protocol Reference Guide, Rev.  J:
--Algorithm (pp.112–3)
--Code: C (pp.113–5)
https://modbus.org/docs/PI_MBUS_300.pdf"

  (crc-16--general sequence #x8005 #xFFFF   t   t #x0000))
(defun crc-16/nrsc-5 (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 16-bit values.

iBiquity Digital Corporation (23 August 2011), HD Radio™ Air Interface Design
Description: Station Information Service Transport, rev.  J (courtesy of
National Radio Systems Committee):
--Definition: Poly (mantissa only); method (Section 4.10, p.38)
https://www.nrscstandards.org/standards-and-guidelines/documents/standards/nrsc-5-d/reference-docs/1020s.pdf"

  (crc-16--general sequence #x080B #xFFFF   t   t #x0000))
(defun crc-16/opensafety-a (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 16-bit values.

Ethernet POWERLINK Standardisation Group (18 October 2018), OpenSAFETY Safety
Profile Specification Working Draft Proposal, version 1.5.2:
--Full description (Section 5.1.2.6, pp.43–4)
--Error detection capability (Section 8.1, p.182)
--Code: C (Appendix 1, pp.196–9)
https://www.ethernet-powerlink.org/fileadmin/user_upload/Dokumente/Downloads/TECHNICAL_DOCUMENTS/EPSG_WDP_304_V-1-5-2.pdf"

  (crc-16--general sequence #x5935 #x0000 nil nil #x0000))
(defun crc-16/opensafety-b (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 16-bit values.

Ethernet POWERLINK Standardisation Group (18 October 2018), OpenSAFETY Safety
Profile Specification Working Draft Proposal, version 1.5.2:
--Full description (Section 5.1.2.6, pp.43–4)
--Error detection capability (Section 8.1, p.182)
--Code: C (Appendix 1, pp.196–9)
https://www.ethernet-powerlink.org/fileadmin/user_upload/Dokumente/Downloads/TECHNICAL_DOCUMENTS/EPSG_WDP_304_V-1-5-2.pdf"

  (crc-16--general sequence #x755B #x0000 nil nil #x0000))
(defun crc-16/profibus (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 16-bit values.

Aliases: CRC-16/IEC-61158-2.

PROFIBUS International (March 1998), PROFIBUS Specification, Normative Parts,
Edition 1.0, Part 9:
--Transmission order, implying RefIn and RefOut (Part 9 Section 8.5.1, p.902)
--Definition: Width, Poly, Init, Residue (Part 9 Section 8.8.4, pp.905–7)
--Properties of polynomial (Part 9 Section 8.8.4, p.906)
https://web.archive.org/web/20081116195826/https://www.kuebler.com/PDFs/Feldbus_Multiturn/specification_DP.pdf"

  (crc-16--general sequence #x1DCF #xFFFF nil nil #xFFFF))
(defun crc-16/riello (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 16-bit values.

\"Snamprogetti\" (17 April 2009), Computer Interfacing Forum topic 1305:
https://web.archive.org/web/20181116155838/https://www.lammertbies.nl/forum/viewtopic.php?t=1305"

  (crc-16--general sequence #x1021 #xB2AA   t   t #x0000))
(defun crc-16/spi-fujitsu (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 16-bit values.

Aliases: CRC-16/AUG-CCITT.

Fujitsu Semiconductor (10 October 2007), FlexRay ASSP MB88121B User's Manual:
--Definition: Width, Poly, Init (Section 2.5.1, p.153)
https://web.archive.org/web/20221017013602/https://www.fujitsu.com/downloads/MICRO/fma/pdfmcu/um-mb88121-am15-11201-1e.pdf"

  (crc-16--general sequence #x1021 #x1D0F nil nil #x0000))
(defun crc-16/t10-dif (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 16-bit values.

Gerald Houlder, INCITS Technical Committee T10 (2 May 2003), End-to-End Data
Protection Proposal:
--Definition: Poly, Init (p.6)
--Shift register diagram (p.18)
https://www.t10.org/ftp/t10/document.03/03-111r0.pdf"

  (crc-16--general sequence #x8BB7 #x0000 nil nil #x0000))
(defun crc-16/teledisk (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 16-bit values.

Will Kranz (8 December 2002), wteledsk 1.01:
--Implementation
https://github.com/jmechnich/wteledsk"

  (crc-16--general sequence #xA097 #x0000 nil nil #x0000))
(defun crc-16/tms37157 (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 16-bit values.

Texas Instruments, Inc. (November 2009), TMS37157 datasheet:
--Full definition (except Check) (pp.39–40)
--Shift register diagram (Figure 51, p.39)
--Flowchart (Figure 52, p.40)
https://www.ti.com/lit/ds/symlink/tms37157.pdf"

  (crc-16--general sequence #x1021 #x89EC   t   t #x0000))
(defun crc-16/umts (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 16-bit values.

Aliases: CRC-16/BUYPASS, CRC-16/VERIFONE.

Verifone, Inc. (May 1995), TCLOAD Reference Manual:
--Definition: Poly.  CRC byte order, implying RefIn/RefOut (Section 4, p.4-1)
https://web.archive.org/web/20120603221525/http://www.verifone.com/PDF/guides/tcl_ref.pdf"

  (crc-16--general sequence #x8005 #x0000 nil nil #x0000))
(defun crc-16/usb (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 16-bit values.

Anonymous (10 July 1997), \"Cyclic Redundancy Checks in USB\" (Draft):
--Definition: Width, Poly, Init, XorOut (p.2)
--Definition: Residue (p.3)
--Code: Perl (p.4)
https://web.archive.org/web/20160326215031/http://www.usb.org/developers/whitepapers/crcdes.pdf"

  (crc-16--general sequence #x8005 #xFFFF   t   t #xFFFF))
(defun crc-16/xmodem (sequence)
  "Convert a SEQUENCE (a list, vector, or string) to hashed 16-bit values.

Aliases: CRC-16/ACORN, CRC-16/LTE, CRC-16/V-41-MSB, XMODEM, ZMODEM.

ITU-T Recommendation V.41 (November 1988):
--Definition: Residue; full mathematical description (Section 2, p.2)
--Shift register diagrams (Appendix I, p.9)
https://www.itu.int/rec/T-REC-V.41/en"

  (crc-16--general sequence #x1021 #x0000 nil nil #x0000))

(defun crc-32--general (sequence polynomial init ref-in ref-out xor-out)
  "General Cyclic Redundancy Check, 32-bit, application with customization.

Because there are varying versions of CRC-32 – depending on the Polynomial,
Initialization value, RefIn and RefOut, and the XorOut –, this function serves
to handle any possible iteration that needs to be computed.

SEQUENCE is a list, vector, or string.

POLYNOMIAL and INIT are integers.

REF-IN and REF-OUT are booleans.

XOR-OUT is a integer."

  (logand
    (logxor (funcall (if ref-out #'crc--reverse-bits (lambda (n _b) n))
                     (seq-reduce (lambda (res1 byte)
                                   (seq-reduce (lambda (res2 _i)
                                                 (let ((shift1 (ash res2 1)))
                                                   (if (zerop (logand res2 #x80000000))
                                                       shift1
                                                     (logxor shift1 polynomial))))
                                               (number-sequence 0 7)
                                               (logxor res1
                                                       (if ref-in
                                                           (crc--reverse-bits byte 32)
                                                         (ash byte (- 32 8))))))
                                 sequence
                                 init)
                     32)
            xor-out)
    #b11111111111111111111111111111111))
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
