?MAIN COD3025
 IDENTIFICATION DIVISION.
*c+
 PROGRAM-ID.     COD3025.
 AUTHOR.         gauravb.
 INSTALLATION.   Tandem Languages and Tools Quality Assurance Section.
 DATE-WRITTEN.   06 07 1994.
*
* PURPOSE: CALL COBOL BY REFERENCE / CONTENT WITH PASSED PARAMETERS IN
*          EXTENDED STORAGE SECTION.
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
                FILE "COB21BX" IS COBPROGB.
 INPUT-OUTPUT SECTION.
 FILE-CONTROL.
    COPY copylib-print-file IN c00qalib.

 DATA DIVISION.
 FILE SECTION.
?NOLIST
     COPY copylib-standard-file IN c00qalib.
?LIST


?HEADING "WORKING-STORAGE  SECTION"
/
 WORKING-STORAGE SECTION.
?NOLIST
     COPY copylib-standard-data IN c00qalib REPLACING "MMMA" BY "JULY"
           "YYYA" BY "1994" "MMMB" BY "JULY" "YYYB" BY "1994".
     COPY copylib-prog-id IN c00qalib REPLACING "XXXXXXX" BY
     "cod3025".
?LIST

 EXTENDED-STORAGE SECTION.


 01 dummy-data                       PIC 9 VALUE 0.
 01 passing-data.
    05 alpha-data                       PIC A(4).
    05 alpha-num-data                   PIC X(6).
    05 num-data                         PIC 9(3).
    05 num-data-comp                    PIC 9(4) USAGE IS COMP.
    05 num-data2                        USAGE IS NATIVE-2.
    05 num-data4                        USAGE IS NATIVE-4.
    05 num-data8                        USAGE IS NATIVE-8.
    05 num-edited-data                  PIC $B99v99 .
    05 alnum-edited-data                PIC XXXXX/.
 01 passing-dummy-data.
    05 alpha-dummy                      PIC A(4).
    05 alpha-num-dummy                  PIC X(6).
    05 num-dummy                        PIC 9(3).
    05 num-dummy-comp                   PIC S9(4) USAGE IS COMP.
    05 num-dummy2                       USAGE IS NATIVE-2.
    05 num-dummy4                       USAGE IS NATIVE-4.
    05 num-dummy8                       USAGE IS NATIVE-8.
    05 num-edited-dummy                 PIC $B99v99.
    05 alnum-edited-dummy               PIC XXXXX/ .


 PROCEDURE DIVISION.
?HEADING "MAIN SECTION"
/
 main SECTION.
     PERFORM copylib-open-file
     PERFORM test-25a
     PERFORM test-25b
     PERFORM copylib-close-file
     STOP RUN
 .

?HEADING "TEST-25A SECTION"
/
 test-25A SECTION.

     PERFORM copylib-test-init
     MOVE "CALL BY CONT-ES" TO feature
     MOVE "TEST-25A" TO par-name
     MOVE "1" TO correct-a
     MOVE "Expected result 1" TO re-mark
     MOVE 123 TO num-data-comp
     MOVE 123 TO num-data
     MOVE 123 TO num-data2
     MOVE 123 TO num-data4
     MOVE 123 TO num-data8
     MOVE 45.67 TO num-edited-data
     MOVE "JAN94" TO alnum-edited-data
     MOVE passing-data TO passing-dummy-data .
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
     END-CALL
     MOVE return-code TO dummy-data
     MOVE dummy-data TO computed-a
     IF passing-data = passing-dummy-data AND
       computed-a = correct-a THEN
        PERFORM PASS
     ELSE
        PERFORM FAIL
     END-IF
     PERFORM print-detail
 .

?HEADING "TEST-25B SECTION"
/
 test-25b SECTION.

     PERFORM copylib-test-init
     MOVE "CALL BY REF IN ES" TO feature
     MOVE "TEST-25B" TO par-name
     MOVE "1" TO correct-a
     MOVE "Expected result 1" TO re-mark
     MOVE 123 TO num-data-comp
     MOVE 123 TO num-data2
     MOVE 123 TO num-data4
     MOVE 123 TO num-data8
     MOVE 45.67 TO num-edited-data
     MOVE "JAN94" TO alnum-edited-data
     MOVE passing-data TO passing-dummy-data
     CALL  COB21B  IN COBPROGB
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
     IF passing-data NOT = passing-dummy-data AND
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
