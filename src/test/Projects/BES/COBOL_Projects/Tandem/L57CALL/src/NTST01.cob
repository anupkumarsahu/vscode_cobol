 IDENTIFICATION DIVISION.
*c+
 PROGRAM-ID.     ntst01.
 AUTHOR.         gauravb.
 INSTALLATION.   Tandem Languages and Tools Quality Assurance Section.
 DATE-WRITTEN.   25 07 1994.
*
* PURPOSE: CALL BY VALUE a COBOL PROGRAM WITHOUT  PORT & ENV COMMON .
* ( NEGATIVE TESTS  1 ,3 & 8 )
* TPR #: NONE               .
* ASSOCIATED FILES:  To include all related files needed for program execution
* LOCAL & GLOBAL PARAMETERS: List all of the parameters needed for execution
*                    of the program.
* EXECUTION THREADS: CRE
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
 SPECIAL-NAMES. FILE "NCOB01X" IS COBPROG.
 INPUT-OUTPUT SECTION.
 FILE-CONTROL.
    COPY copylib-print-file IN C00QALIB.

 DATA DIVISION.
 FILE SECTION.
?NOLIST
     COPY copylib-standard-file IN C00QALIB.
?LIST

?HEADING "WORKING-STORAGE SECTION"
/
 WORKING-STORAGE SECTION.
?NOLIST
     COPY copylib-standard-data IN C00QALIB REPLACING "MMMA" BY "JULY"
           "YYYA" BY "1994" "MMMB" BY "JULY" "YYYB" BY "1994".
     COPY copylib-prog-id IN C00QALIB REPLACING "XXXXXXX" BY "ntst01".
?LIST

 01 dummy-data                       PIC 9 VALUE 0.
 01 num-data-comp                    PIC X VALUE "A".
 01 num-dummy-comp                   PIC X.


 PROCEDURE DIVISION.
?HEADING "MAIN SECTION"
/
 main SECTION.
     PERFORM copylib-open-file
     PERFORM test-1
     PERFORM copylib-close-file
     STOP RUN
 .

?HEADING "TEST-1  SECTION"
/
 test-1  SECTION.

     PERFORM copylib-test-init
     MOVE "Call without PORT" TO feature
     MOVE "TEST-1" TO par-name
     MOVE "1" TO correct-a
     MOVE "Expected result 1" TO re-mark
     MOVE 123 TO num-data-comp
     MOVE 1 TO dummy-data
     MOVE num-data-comp TO num-dummy-comp
     CALL  NCOB01  IN COBPROG
     USING BY VALUE   num-data-comp
     END-CALL
     MOVE dummy-data TO computed-a
     IF num-data-comp = num-dummy-comp AND
       computed-a = correct-a THEN
        PERFORM PASS
     ELSE
        PERFORM FAIL
     END-IF
     PERFORM print-detail
  .

?HEADING " "
?NOLIST
     COPY copylib-open-file  IN C00QALIB.
     COPY copylib-test-init  IN C00QALIB.
     COPY copylib-close-file IN C00QALIB.
     COPY copylib-general-utilities IN C00QALIB.
?LIST
