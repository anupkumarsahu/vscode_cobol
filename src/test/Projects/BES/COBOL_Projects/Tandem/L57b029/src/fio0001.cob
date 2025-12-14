?SYMBOLS
 IDENTIFICATION DIVISION.

 PROGRAM-ID.   FIO0001.
 AUTHOR.       Arun Fernandes.
 INSTALLATION. TANDEM SOFTWARE DEPARTMENT.
 DATE-WRITTEN. 04/12/00.
 DATE-COMPILED.

******************************************************************************
*
* PURPOSE: POSITIVE TEST : It tests that a COBOL85 program using CRE and
*          FAST I/O does not skip any records at the beginning of an indexed
*          file.
*          The program should compile successfully and pass at runtime.
*
*
* TPR #:
* ASSOCIATED FILES:          FIO0001R
* LOCAL & GLOBAL PARAMETERS: None
* EXECUTION THREADS:         All
* COMPILATION INSTRUCTIONS:  ENV COMMON.
* EXECUTION INSTRUCTIONS:    The following must be set to execute this program:
*
*    4. The program object file must run as shown :
*     run cobol85/in FIO0001,out FIO00019/ FIO0001X ;env common
*
* CLEANUP:                   None
*
* MODIFIER       |   DATE     |  DESCRIPTION OF CHANGE
*-----------------------------------------------------------------------------
* A.Fernandes      04/14/2000   Initial release.
*
*
******************************************************************************


 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
   SPECIAL-NAMES.
      FILE  "=cbllib" IS cobollib.

 INPUT-OUTPUT SECTION.
 FILE-CONTROL.
     SELECT t08-file ASSIGN TO MMPM907
         RESERVE 5 AREAS
         ORGANIZATION IS INDEXED
         ACCESS MODE IS SEQUENTIAL
         RECORD KEY IS t08-rec-key
         FILE STATUS IS t08-fs.

     SELECT t08-file2 ASSIGN TO MMPM907
         RESERVE 2 AREAS
         ORGANIZATION IS INDEXED
         ACCESS MODE IS SEQUENTIAL
         RECORD KEY IS t08-rec-key2
         FILE STATUS IS t08-fs.

?NOLIST
      COPY copylib-print-file IN b30qalib.
?LIST

?HEADING "FILE SECTION"
/

 DATA DIVISION.

 FILE SECTION.

 FD  t08-file.
 01  t08-rec.
     02  t08-rec-key  PIC X(12).
     02  FILLER       PIC X(3836).

 FD  t08-file2.
 01  t08-rec2.
     02  t08-rec-key2 PIC X(12).
     02  FILLER       PIC X(3836).

?NOLIST
     COPY copylib-standard-file IN b30qalib.
?LIST

?HEADING "WORKING-STORAGE SECTION"
/
 WORKING-STORAGE SECTION.
 01  result-rec.
     03  result-remarks  PIC X(10) VALUE SPACES.
     03                  PIC X VALUE ">".
     03  result-should-be-an  PIC X(20) VALUE SPACES.
     03  result-should-be-num REDEFINES result-should-be-an  PIC -9(9).9(9).
     03  FILLER  PIC XX VALUE "<>".
     03  result-is-an  PIC X(20) VALUE SPACES.
     03  result-is-num REDEFINES result-is-an  PIC -9(9).9(9).
     03                  PIC X VALUE "<".
 01  t08-fs PIC XX.

?NOLIST
    COPY  copylib-standard-data IN b30qalib REPLACING "MMMA" BY "JAN"
          "YYYA" BY "2000" "MMMB" BY "JAN" "YYYB" BY "2000".
    COPY copylib-prog-id IN b30qalib REPLACING "XXXXXXX" BY "FIO0001".
?LIST

?HEADING "EXTENDED-STORAGE SECTION"
/
 EXTENDED-STORAGE SECTION.


 PROCEDURE DIVISION.

?HEADING "MAIN SECTION"
/
 MAIN SECTION.

     PERFORM COPYLIB-OPEN-FILE.

     PERFORM 100-TEST-1.
     PERFORM 200-END-TESTS . 

*  Test 1 - Fast I-O read skips first recs, while without FAST I/O it
*  works fine.
*  CASE 10-000407-2900, SOLN 10-000407-7335


 100-TEST-1.

*   We have 2 READ statements to the same indexed
*   file MMPM907. The first one is by FAST I/O method (with
*   RESERVE 5 AREAS ) and the other is without FAST I/O
*   (RESERVE 2 AREAS ). After the READs are done we checks that both do not
*   skip any records from the file.
*

* Open the two files (though they reference the same file there are
* two file discriptors. So we use the OPEN statement twice).
     OPEN INPUT t08-file PROTECTED.
     OPEN INPUT t08-file2 PROTECTED.

* Now read from the file (one READ with FAST I/O and the second without)
* and make sure that both the RECORD KEYS have the same value.

     PERFORM UNTIL t08-fs = "10"
         READ t08-file AT END
             READ t08-file2 NOT AT END
                 MOVE "FIO 1" TO result-remarks
                 MOVE "Early at end on 1" TO result-is-an
                 PERFORM 400-FAILS
                 MOVE "10" TO t08-fs
                 EXIT PARAGRAPH
             END-READ
         NOT AT END
             READ t08-file2 AT END
                 MOVE "FIO 2" TO result-remarks
                 MOVE "Early at end on 2" TO result-is-an
                 PERFORM 400-FAILS
                 EXIT PARAGRAPH
             END-READ
             IF t08-rec-key NOT = t08-rec-key2
                 MOVE "FIO 3" TO result-remarks
                 MOVE t08-rec-key TO result-is-an
                 MOVE t08-rec-key2 TO result-should-be-an
                 PERFORM 400-FAILS
                 EXIT PARAGRAPH
             END-IF
         END-READ
     END-PERFORM
     CLOSE t08-file, t08-file2
     PERFORM 300-PASSES.

 200-END-TESTS.
*    Stop execution after closing files.
     PERFORM COPYLIB-CLOSE-FILE.
     STOP   RUN.

 300-PASSES.
         PERFORM COPYLIB-TEST-INIT.
         PERFORM PASS.
         MOVE "Read Recs - FAST I/0"          TO feature.
         MOVE "100-TEST-1          "          TO par-name.
         MOVE spaces                          TO correct-a.
         MOVE "FAST I/O reads 1'st record   " TO re-mark.
         PERFORM PRINT-DETAIL.


 400-FAILS.

        PERFORM COPYLIB-TEST-INIT.
        PERFORM FAIL.
        MOVE "Read Recs - FAST I/0"          TO feature.
        MOVE "100-TEST-1          "          TO par-name.
        MOVE spaces                          TO correct-a.
        MOVE "FAST I/O skips 1'st record   " TO re-mark.
        PERFORM PRINT-DETAIL.


?NOLIST
     COPY copylib-open-file         IN b30qalib.
     COPY copylib-test-init         IN b30qalib.
     COPY copylib-close-file        IN b30qalib.
     COPY copylib-general-utilities IN b30qalib.
?LIST
