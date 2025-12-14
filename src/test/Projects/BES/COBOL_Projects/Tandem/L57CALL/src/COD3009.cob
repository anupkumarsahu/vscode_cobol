 IDENTIFICATION DIVISION.
*c+
 PROGRAM-ID.     COD3009.
 AUTHOR.         gauravb.
 INSTALLATION.   Tandem Languages and Tools Quality Assurance Section.
 DATE-WRITTEN.   17 AUG 1994.
*
* PURPOSE: Call   fortran program by REFERENCE .
*
* TPR #: Sxxxxxxxxxxxxxxxx
* ASSOCIATED FILES:  To include all related files needed for program execution
* LOCAL & GLOBAL PARAMETERS: List all of the parameters needed for execution
*                    of the program.
* EXECUTION THREADS: Historic(Normal)/CRE
* COMPILATION INSTRUCTIONS: List special directives needed.
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
                FILE "FORTREFX" IS FORTPROG .

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
     COPY copylib-standard-data IN c00qalib REPLACING "MMMA" BY "JAN "
           "YYYA" BY "1986" "MMMB" BY "JULY" "YYYB" BY "1994".
     COPY copylib-prog-id IN c00qalib REPLACING "XXXXXXX" BY
     "COD3009".

?LIST

 01 dummy-data                          PIC 9 VALUE 0.

 01 passing-data.
    05 num-data                         PIC S9(4) USAGE IS COMP.
    05 num-data2                        USAGE IS NATIVE-2.
    05 num-data4                        USAGE IS NATIVE-4.
    05 num-data8                        USAGE IS NATIVE-8.

 01 passing-dummy-data.
    05 num-dummy-comp                   PIC S9(4) comp.
    05 num-dummy2                       USAGE IS NATIVE-2.
    05 num-dummy4                       USAGE IS NATIVE-4.
    05 num-dummy8                       USAGE IS NATIVE-8.

 01 val-grp .
     03  alpha-num-val                 PIC X.
     03  num-val-nat2                  Usage is   NATIVE-2 .
     03  num-val-nat4                  Usage is   NATIVE-4 .
     03  num-val-nat8                  Usage is   NATIVE-8 .
     03  comp5-val-4                   PIC S9(4) Usage is COMP-5 .
     03  comp5-val-9                   PIC S9(9) Usage is COMP-5 .
     03  comp5-val-18                  PIC S9(18) Usage is COMP-5 .

 01 val-dummy.
     03  alpha-num-val-dummy           PIC  X.
     03  num-val-nat2-dummy            Usage is   NATIVE-2 .
     03  num-val-nat4-dummy            Usage is   NATIVE-4 .
     03  num-val-nat8-dummy            Usage is   NATIVE-8 .
     03  comp5-val-4-dummy             PIC S9(4) Usage is COMP-5 .
     03  comp5-val-9-dummy             PIC S9(9) Usage is COMP-5 .
     03  comp5-val-18-dummy            PIC S9(18) Usage is COMP-5 .




 PROCEDURE DIVISION.
?HEADING "MAIN SECTION"
/
 main SECTION.
     PERFORM copylib-open-file
     PERFORM test-9a
     PERFORM copylib-close-file
     STOP RUN
    .




?HEADING "test-9a SECTION"
/
 test-9a SECTION.

     PERFORM copylib-test-init
     MOVE "Call Fort by REF " TO feature
     MOVE "test-9a" TO par-name
     MOVE "1" TO correct-a
     MOVE "Expected result 1" TO re-mark
     MOVE 123 TO num-data  .
     MOVE 123 TO num-data2 .
     MOVE 123 TO num-data4 .
     MOVE 123 TO num-data8 .
     MOVE passing-data TO passing-dummy-data .
     CALL   IREF   IN  FORTPROG
     USING  BY REFERENCE   num-data
                            num-data2
                            num-data4
                            num-data8
     END-CALL
     MOVE return-code TO dummy-data
     MOVE dummy-data TO computed-a
     DISPLAY "PASSING-DUMMY-DATA   " PASSING-DUMMY-DATA .
     DISPLAY "PASSING-DATA         " PASSING-DATA .
     IF passing-data NOT EQUAL passing-dummy-data AND
       computed-a = correct-a THEN
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
