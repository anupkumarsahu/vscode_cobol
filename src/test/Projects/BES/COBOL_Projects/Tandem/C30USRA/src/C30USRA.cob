 IDENTIFICATION DIVISION.
 PROGRAM-ID.   B30USRa.
 AUTHOR.       JCW.
 INSTALLATION. TANDEM SOFTWARE DEPARTMENT.
 DATE-WRITTEN. 06/27/85.
 DATE-COMPILED.
* User-defined words and system-names.
* The same COBOL word may be used as a system-name and as a user-defined
* word within a source program; the context in which a COBOL word
* occurs determines what it is.
*                                                                     !B40A
*   REVISION HISTORY                                                  !B40A
*++ 14 MAY 1986   Kenneth Luu                                         !B40A
*+  Fixed syntax errors in this test.                                 !B40A

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER.  TANDEM-16.
 OBJECT-COMPUTER.  TANDEM-16.
 INPUT-OUTPUT SECTION.
 FILE-CONTROL.
?NOLIST
****                                                                  !B40A
**** The following line was missing in the test before; therefore,    !B40A
**** the test was never compiled successfully.  This problem is now   !B40A
**** fixed.                                                           !B40A
****                                                                  !B40A
?LIST
  SELECT SEQ-FILE ASSIGN TO B30USRaZ
    ORGANIZATION SEQUENTIAL
    ACCESS MODE SEQUENTIAL
 .
 DATA DIVISION.

 FILE SECTION.
 FD  SEQ-FILE
     LABEL RECORD OMITTED
     RECORD VARYING FROM 2 TO 58 DEPENDING RECORD-SIZE
     DATA RECORD IS SEQ-RECORD.
 01  SEQ-RECORD.
     05 SEQ-COUNTER   PIC 9(2).
     05               PIC N(1).
     05 SEQ-CONTENT   PIC N(27).

?HEADING "WORKING-STORAGE SECTION"
/
 WORKING-STORAGE SECTION.

 01  RECORD-SIZE      PIC 9(2) VALUE 30.

 01 TANDEM-16            PIC N(30) VALUE N"・托ｼ抵ｼ難ｼ費ｼ包ｼ厄ｼ暦ｼ假ｼ呻ｼ・".

 01 B30USRaZ             PIC N(50) VALUE N"莠應ｺ應ｺ應ｺ應ｺ應ｺ應ｺ應ｺ應ｺ應ｺ・".

?HEADING "MAIN SECTION"
/
 PROCEDURE DIVISION.
 MAIN SECTION.

     PERFORM USER-DEFINED-TEST-1
     PERFORM USER-DEFINED-TEST-2
     STOP    RUN
 .
?HEADING "USER-DEFINED-TEST-1 SECTION"
/
 USER-DEFINED-TEST-1 SECTION.

  DISPLAY "DEFINED-TEST-TEST-1"
     MOVE N"荳荳荳荳荳荳荳荳荳荳" to TANDEM-16
     IF TANDEM-16 = N"荳荳荳荳荳荳荳荳荳荳" THEN
        DISPLAY "PASSED"
     ELSE
        DISPLAY "FAILED" TANDEM-16
     END-IF
 .
?HEADING "USER-DEFINED-TEST-2 SECTION"
/
 USER-DEFINED-TEST-2 SECTION.

  DISPLAY "DEFINED-TEST-TEST-2"
     MOVE N"荳荳荳荳荳荳荳荳荳荳" to B30USRaZ
     IF B30USRaZ  = N"荳荳荳荳荳荳荳荳荳荳" THEN
        DISPLAY "PASSED"
     ELSE
        DISPLAY "FAILED" SPACE B30USRaZ
     END-IF
 .
