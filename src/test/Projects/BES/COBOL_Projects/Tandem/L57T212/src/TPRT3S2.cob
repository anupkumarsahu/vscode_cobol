 identification division.
* program submitted for TPR 890306 1802 13513
 program-id.         t35-prog.
 author.             ALEX  LAM  (T.A. Consultants Ltd).
 date-written.       28/12/87.

**********************************************************************

 environment division.
 configuration section.
 source-computer.    tandem ext/10.
 object-computer.    tandem ext/10.

 input-output section.
 file-control.


 data division.
 file section.


 working-storage section.

**************************************************************
*    copy system file map, subroutine file map.              *
**************************************************************

 01  gesplprt-variables.
     03  ingesplfl                        pic x(34).
     03  gesplfl-err                      pic 9(4).


 extended-storage section.

**  report layout  **

 01 page-head-line-1.
    05  p-report-name              pic x(60).
    05  filler                     pic x(03) value space.
    05  p-bank-name                pic x(40).
    05  filler                     pic x(07) value space.
    05  filler                     pic x(07) value "DATE : ".
    05  p-report-date              pic x(09).

 01 page-head-line-2.
    05  filler                     pic x(12) value
                                    "FREQUENCY : ".
    05  p-frequency                pic x(18) value space.
    05  filler                     pic x(33) value spaces.
    05  p-bch1-name                pic x(40).
    05  filler                     pic x(07) value space.
    05  p-report-id                pic x(10).
    05  filler                     pic x(02) value space.
    05  filler                     pic x(05) value "PAGE ".
    05  p-page-no                  pic zzz9.

 01 page-head-line-3.
    05  filler                     pic x(15) value
                                    "DISTRIBUTION : ".
    05  p-dept-name                pic x(20).
    05  filler                     pic x(75) value space.
    05  filler                     pic x(17) value
                                    "RETAINED TILL  : ".
    05  p-retain-period            pic x(04).

 01 heading-line-10.
     05  filler                   pic x(20) value spaces.
     05  filler                   pic x(10) value
                                  "O/S AMOUNT".

 01 heading-line-11.
     05  filler                   pic x(10) value
                                  " REF. NO. ".
     05  filler                   pic x(08) value spaces.
     05  filler                   pic x(13) value
                                  "(ORIG.AMOUNT)".
     05  filler                   pic x(12) value spaces.
     05  p-lcy-1                  pic xxx.
     05  filler                   pic x(11) value
                                  " EQUIVALENT".
     05  filler                   pic x(7)  value spaces.
     05  filler                   pic x(8)  value
                                  "DUE DATE".
     05  filler                   pic x(3)  value spaces.
     05  filler                   pic x(13) value
                                  "INTEREST PAID".
     05  filler                   pic x(11) value spaces.
     05  filler                   pic x(11) value
                                  "MERCHANDISE".
     05  filler                   pic x(13) value spaces.
     05  filler                   pic x(6)  value
                                  "STATUS".
     05  filler                   pic x(3)  value spaces.

 01 heading-line-120.
     05  filler                   pic x(10) value all "-".
     05  filler                   pic x(1)  value spaces.
     05  filler                   pic x(26) value all "-".
     05  filler                   pic x(1)  value "P".
     05  filler                   pic x(24) value all "-".
     05  filler                   pic x(2)  value spaces.
     05  filler                   pic x(8)  value all "-".
     05  filler                   pic x(2)  value spaces.
     05  filler                   pic x(14) value all "-".
     05  filler                   pic x(2)  value spaces.
     05  filler                   pic x(30) value all "-".
     05  filler                   pic x(2)  value spaces.
     05  filler                   pic x(8)  value all "-".
     05  filler                   pic x(2)  value spaces.

 01 heading-line-122.
     05  filler                   pic x(10) value all "-".
     05  filler                   pic x(1)  value spaces.
     05  filler                   pic x(26) value all "-".
     05  p-test                   pic x(1)  value spaces.
     05  filler                   pic x(24) value all "-".
     05  filler                   pic x(2)  value spaces.
     05  filler                   pic x(8)  value all "-".
     05  filler                   pic x(2)  value spaces.
     05  filler                   pic x(14) value all "-".
     05  filler                   pic x(2)  value spaces.
     05  filler                   pic x(30) value all "-".
     05  filler                   pic x(2)  value spaces.
     05  filler                   pic x(8)  value all "-".
     05  filler                   pic x(2)  value spaces.

 01 heading-line-121.
     05  filler                   pic x(10) value all "-".
     05  filler                   pic x(1)  value spaces.
     05  filler                   pic x(26) value all "-".
     05  filler                   pic x(1)  value spaces.
     05  filler                   pic x(24) value all "-".
     05  filler                   pic x(2)  value spaces.
     05  filler                   pic x(8)  value all "-".
     05  filler                   pic x(2)  value spaces.
     05  filler                   pic x(14) value all "-".
     05  filler                   pic x(2)  value spaces.
     05  filler                   pic x(30) value all "-".
     05  filler                   pic x(2)  value spaces.
     05  filler                   pic x(8)  value all "-".
     05  filler                   pic x(2)  value spaces.

 01 heading-line-20.
    05  filler                     pic x(20) value
                                   "BRANCH SUMMARY : -  ".
    05  filler                     pic x(112) value spaces.

 01 heading-line-21.
    05  filler                     pic x(51) value space.
    05  p-lcy-2                    pic x(03).
    05  filler                     pic x(01) value " ".
    05  filler                     pic x(10) value
                                   "EQUIVALENT".
    05  filler                     pic x(30) value space.
    05  p-lcy-3                    pic x(03).
    05  filler                     pic x(01) value " ".
    05  filler                     pic x(10) value
                                   "EQUIVALENT".
    05  filler                     pic x(08) value space.
    05  filler                     pic x(05) value
                                   "NO.OF".
    05  filler                     pic x(10) value space.

 01 heading-line-22.
    05  filler                     pic x(16) value space.
    05  filler                     pic x(03) value "CCY".
    05  filler                     pic x(10) value space.
    05  filler                     pic x(10) value
                                   "O/S AMOUNT".
    05  filler                     pic x(12) value space.
    05  filler                     pic x(15) value
                                   "AT VARIOUS RATE".
    05  filler                     pic x(09) value space.
    05  filler                     pic x(10) value
                                   "BILAN RATE".
    05  filler                     pic x(10) value space.
    05  filler                     pic x(13) value
                                   "AT BILAN RATE".
    05  filler                     pic x(09) value space.
    05  filler                     pic x(05) value
                                   "ITEMS".
    05  filler                     pic x(10) value space.

 01 heading-line-23.
    05  filler                     pic x(16) value space.
    05  filler                     pic x(03) value all "-".
    05  filler                     pic x(05) value space.
    05  filler                     pic x(20) value all "-".
    05  filler                     pic x(04) value space.
    05  filler                     pic x(22) value all "-".
    05  filler                     pic x(04) value space.
    05  filler                     pic x(13) value all "-".
    05  filler                     pic x(04) value space.
    05  filler                     pic x(22) value all "-".
    05  filler                     pic x(04) value space.
    05  filler                     pic x(05) value all "-".
    05  filler                     pic x(10) value space.

 01 heading-line-30.
    05  filler                     pic x(18) value
                                   "BANK SUMMARY : -  ".
    05  filler                     pic x(114) value spaces.

 01 heading-line-31.
    05  filler                     pic x(66) value space.
    05  p-lcy-4                    pic x(03).
    05  filler                     pic x(01) value " ".
    05  filler                     pic x(10) value
                                   "EQUIVALENT".
    05  filler                     pic x(26) value space.
    05  p-lcy-5                    pic x(03).
    05  filler                     pic x(01) value " ".
    05  filler                     pic x(10) value
                                   "EQUIVALENT".
    05  filler                     pic x(06) value space.
    05  filler                     pic x(05) value
                                   "NO.OF".
    05  filler                     pic x(01) value space.

 01 heading-line-32.
    05  filler                     pic x(12) value space.
    05  filler                     pic x(11) value
                                   "BRANCH NAME".
    05  filler                     pic x(21) value space.
    05  filler                     pic x(10) value
                                   "O/S AMOUNT".
    05  filler                     pic x(12) value space.
    05  filler                     pic x(15) value
                                   "AT VARIOUS RATE".
    05  filler                     pic x(07) value space.
    05  filler                     pic x(10) value
                                   "BILAN RATE".
    05  filler                     pic x(08) value space.
    05  filler                     pic x(13) value
                                   "AT BILAN RATE".
    05  filler                     pic x(07) value space.
    05  filler                     pic x(05) value
                                   "ITEMS".
    05  filler                     pic x(01) value space.

 01 heading-line-33.
    05  filler                     pic x(35) value all "-".
    05  filler                     pic x(02) value space.
    05  filler                     pic x(24) value all "-".
    05  filler                     pic x(02) value space.
    05  filler                     pic x(22) value all "-".
    05  filler                     pic x(02) value space.
    05  filler                     pic x(13) value all "-".
    05  filler                     pic x(02) value space.
    05  filler                     pic x(22) value all "-".
    05  filler                     pic x(02) value space.
    05  filler                     pic x(05) value all "-".
    05  filler                     pic x(01) value space.

*/* ib  outstanding listing */

 01 detail-line-11.
    05  filler                   pic x(16) value
                                 "CUSTOMER NAME : ".
    05  p-cust-name-1            pic x(35).
    05  filler                   pic x(5)  value spaces.
    05  filler                   pic x(15) value
                                 "CUSTOMER A/C : ".
    05  p-cust-no-1              pic x(12).
    05  filler                   pic x(48) value spaces.

* /* bank summary */


   01  total-line-11.
     05  filler                   pic x(64) value spaces.
     05  filler                   pic x(26) value all "-".
     05  filler                   pic x(42) value spaces.

*  /* branch summary */

  01  total-line-21.
    05 filler                      pic x(48) value space.
    05 filler                      pic x(22) value all "-".
    05 filler                      pic x(21) value space.
    05 filler                      pic x(22) value all "-".
    05 filler                      pic x(04) value space.
    05 filler                      pic x(05) value all "-".


 01 total-line-23.
    05 filler                      pic x(48) value space.
    05 filler                      pic x(22) value all "=".
    05 filler                      pic x(21) value space.
    05 filler                      pic x(22) value all "=".
    05 filler                      pic x(04) value space.
    05 filler                      pic x(05) value all "=".

* /* bank summary */

 01 total-line-31.
    05 filler                      pic x(63) value space.
    05 filler                      pic x(22) value all "-".
    05 filler                      pic x(17) value space.
    05 filler                      pic x(22) value all "-".
    05 filler                      pic x(02) value space.
    05 filler                      pic x(05) value all "-".

 01 total-line-33.
    05 filler                      pic x(63) value space.
    05 filler                      pic x(22) value all "=".
    05 filler                      pic x(17) value space.
    05 filler                      pic x(22) value all "=".
    05 filler                      pic x(02) value space.
    05 filler                      pic x(05) value all "=".

 01 report-ending-19.
    05  filler                     pic x(35)
              value "*** E N D   O F   L I S T I N G ***".
    05  filler                     pic x(31)
              value " TOTAL NO OF RECORDS PRINTED = ".
    05  p-rec-cnt                  pic zzz,zz9.
    05  filler                     pic x(04) value " ***".
    05  filler                     pic x(55) value space.

**  layout completed  **

 01 p-blank-line                   pic x(132) value spaces.

 01  tmp-message.
     05  filler                      pic  x(16) value "STANDARD ERROR: ".
     05  msg-code                    pic 9(4) value 4321.

 LINKAGE SECTION.
 01  error-rtn  PIC X(20).

 procedure division USING error-rtn.

 a0100-start-program.
     if tmp-message = "STANDARD ERROR: 4321"
         MOVE SPACES TO error-rtn
     ELSE
         MOVE tmp-message TO error-rtn
     END-IF
     exit     program.


**************************************************************
*                  end of program                            *
**************************************************************
