?main cod3022
 IDENTIFICATION DIVISION.
*c+
 PROGRAM-ID.     COD3022.
 AUTHOR.         gauravb.
 INSTALLATION.   Tandem Languages and Tools Quality Assurance Section.
 DATE-WRITTEN.   06 07 1994.
*
* PURPOSE: TO CALL COBOL BY  REFERENCE / CONTENT WITH PASSED PARAMETERS IN
*         LINKAGE SECTION .
* TPR #: Sxxxxxxxxxxxxxxxx
* ASSOCIATED FILES:  To include all related files needed for program execution
* LOCAL & GLOBAL PARAMETERS: List all of the parameters needed for execution
*                    of the program.
* EXECUTION THREADS: Historic(Normal)/CRE
* COMPILATION INSTRUCTIONS: PORT, ENV Special directives needed.
* EXECUTION INSTRUCTIONS: List defines or special directives needed.
* CLEANUP: Mention any nonstandard cleanup that is neede.
*
* MODIFIER                |   DATE   |  DESCRIPTION OF CHANGE
*-----------------------------------------------------------------------------
* name                    | mm/dd/yy | description of the update
*
*c-

 ENVIRONMENT DIVISION.

 CONFIGURATION SECTION.
 SOURCE-COMPUTER. Tandem Nonstop System.
 OBJECT-COMPUTER. Tandem Nonstop System.
 SPECIAL-NAMES.
              FILE "COB21AX" IS COBPROGA
              FILE "COB21BX" IS COBPROGB .
 INPUT-OUTPUT SECTION.
 FILE-CONTROL.
    COPY copylib-print-file IN c00qalib.

 DATA DIVISION.
 FILE SECTION.
?NOLIST
     COPY copylib-standard-file IN c00qalib.
?LIST

?HEADING "WORKING-STORAGE SECTION"
/
 WORKING-STORAGE SECTION.
?NOLIST
     COPY copylib-standard-data IN c00qalib REPLACING "MMMA" BY "JULY"
           "YYYA" BY "1994" "MMMB" BY "JULY" "YYYB" BY "1994".
     COPY copylib-prog-id IN c00qalib REPLACING "XXXXXXX" BY
    "cod3021".
?LIST


 01 dummy-data                          PIC 9 VALUE 0.

 01  SET-VAL1                      PIC  A(4).
 01  SET-VAL2                      PIC  X(6) .
 01  SET-VAL3                      PIC  9(3).
 01  SET-VAL4                      PIC  S9(4) COMP.
 01  SET-VAL5                      USAGE IS NATIVE-2.
 01  SET-VAL6                      USAGE IS NATIVE-4.
 01  SET-VAL7                      USAGE IS NATIVE-8.
 01  SET-VAL8                      PIC $B99V99 .
 01  SET-VAL9                      PIC XXXXX/.

    01 alpha-dummy                      PIC A(4) .
    01 alpha-num-dummy                  PIC X(6) .
    01 num-dummy                        PIC 9(3) .
    01 num-dummy-comp                   PIC S9(4) USAGE IS COMP.
    01 num-dummy2                        USAGE IS NATIVE-2.
    01 num-dummy4                        USAGE IS NATIVE-4.
    01 num-dummy8                        USAGE IS NATIVE-8.
    01 num-edited-dummy                 PIC $B99v99.
    01 alnum-edited-dummy               PIC XXXXX/ .



 LINKAGE SECTION .

    01 alpha-data                       PIC A(4).
    01 alpha-num-data                   PIC X(6).
    01 num-data                         PIC 9(3).
    01 num-data-comp                    PIC S9(4) USAGE IS COMP.
    01 num-data2                        USAGE IS NATIVE-2.
    01 num-data4                        USAGE IS NATIVE-4.
    01 num-data8                        USAGE IS NATIVE-8.
    01 num-edited-data                  PIC $B99v99 .
    01 alnum-edited-data                PIC XXXXX/.


 PROCEDURE DIVISION.
?HEADING "MAIN SECTION"
/
 main SECTION.
     PERFORM copylib-open-file
     PERFORM test-22a .
     PERFORM test-22b .
     PERFORM copylib-close-file
     STOP RUN
 .

?HEADING "test-22A SECTION"
/
 test-22A SECTION.

     PERFORM copylib-test-init
     MOVE "CALL BY CONT -LS" TO feature
     MOVE "test-22A" TO par-name
     MOVE "1" TO correct-a
     MOVE "Expected result 1" TO re-mark .

     SET ADDRESS OF ALPHA-DATA        TO ADDRESS OF SET-VAL1.
     SET ADDRESS OF ALPHA-NUM-DATA    TO ADDRESS OF SET-VAL2.
     SET ADDRESS OF NUM-DATA          TO ADDRESS OF SET-VAL3.
     SET ADDRESS OF NUM-DATA-COMP     TO ADDRESS OF SET-VAL4.
     SET ADDRESS OF NUM-DATA2         TO ADDRESS OF SET-VAL5.
     SET ADDRESS OF NUM-DATA4         TO ADDRESS OF SET-VAL6.
     SET ADDRESS OF NUM-DATA8         TO ADDRESS OF SET-VAL7.
     SET ADDRESS OF NUM-EDITED-DATA   TO ADDRESS OF SET-VAL8.
     SET ADDRESS OF ALNUM-EDITED-DATA TO ADDRESS OF SET-VAL9.

     MOVE "ABCD" TO alpha-data    .
     MOVE "A1B2C3" TO alpha-num-data .
     MOVE 123 TO num-data.
     MOVE 123 TO num-data-comp .
     MOVE 123 TO num-data2     .
     MOVE 123 TO num-data4     .
     MOVE 123 TO num-data8     .
     MOVE 45.67 TO num-edited-data .
     MOVE "JAN94" TO alnum-edited-data .

     MOVE  alpha-data  TO ALPHA-DUMMY.
     MOVE  num-data TO NUM-DUMMY .
     MOVE  num-data8  TO NUM-DUMMY8.
     MOVE  alnum-edited-data TO ALNUM-EDITED-DUMMY.


     CALL  COB21A IN COBPROGA
     USING BY CONTENT  alpha-data
                       alpha-num-data
                       num-data
                       num-data-comp
                       num-data2
                       num-data4
                       num-data8
                       num-edited-data
                       alnum-edited-data
     END-CALL  .
     MOVE return-code TO dummy-data  .
     MOVE dummy-data TO computed-a   .
     IF ALPHA-DATA = ALPHA-DUMMY  AND  NUM-DATA = NUM-DUMMY AND
       NUM-DATA8 = NUM-DUMMY8 AND ALNUM-EDITED-DATA =  ALNUM-EDITED-DUMMY
     AND computed-a = correct-a THEN
        PERFORM PASS
     ELSE
        PERFORM FAIL
     END-IF   .
     PERFORM print-detail
     .

?HEADING "test-22B SECTION"
/
 test-22b SECTION.

     PERFORM copylib-test-init
     MOVE "CALL BY REFERENCE-LS" TO feature
     MOVE "test-22B" TO par-name
     MOVE "1" TO correct-a
     MOVE "Expected result 1" TO re-mark

     SET ADDRESS OF ALPHA-DATA        TO ADDRESS OF SET-VAL1.
     SET ADDRESS OF ALPHA-NUM-DATA    TO ADDRESS OF SET-VAL2.
     SET ADDRESS OF NUM-DATA          TO ADDRESS OF SET-VAL3.
     SET ADDRESS OF NUM-DATA-COMP     TO ADDRESS OF SET-VAL4.
     SET ADDRESS OF NUM-DATA2         TO ADDRESS OF SET-VAL5.
     SET ADDRESS OF NUM-DATA4         TO ADDRESS OF SET-VAL6.
     SET ADDRESS OF NUM-DATA8         TO ADDRESS OF SET-VAL7.
     SET ADDRESS OF NUM-EDITED-DATA   TO ADDRESS OF SET-VAL8.
     SET ADDRESS OF ALNUM-EDITED-DATA TO ADDRESS OF SET-VAL9.

     MOVE "ABCD" TO alpha-data    .
     MOVE "A1B2C3" TO alpha-num-data .
     MOVE 123 TO num-data.
     MOVE 123 TO num-data-comp
     MOVE 123 TO num-data2
     MOVE 123 TO num-data4
     MOVE 123 TO num-data8
     MOVE 45.67 TO num-edited-data
     MOVE "JAN94" TO alnum-edited-data

     MOVE  alpha-data  TO ALPHA-DUMMY.
     MOVE  num-data TO NUM-DUMMY .
     MOVE  num-data8  TO NUM-DUMMY8.
     MOVE  alnum-edited-data TO ALNUM-EDITED-DUMMY.
     CALL  COB21B IN COBPROGB
     USING BY REFERENCE alpha-data
                       alpha-num-data
                       num-data
                       num-data-comp
                       num-data2
                       num-data4
                       num-data8
                       num-edited-data
                       alnum-edited-data
     END-CALL
     MOVE return-code TO dummy-data
     MOVE dummy-data TO computed-a

    IF  NUM-DATA NOT = NUM-DUMMY
      AND computed-a = correct-a THEN
        PERFORM PASS
     ELSE
        PERFORM FAIL
     END-IF
     PERFORM print-detail
      .

?HEADING " "
?NOLIST
     COPY copylib-open-file  IN c00qalib.
     COPY copylib-test-init  IN c00qalib.
     COPY copylib-close-file IN c00qalib.
     COPY copylib-general-utilities IN c00qalib.
?LIST
