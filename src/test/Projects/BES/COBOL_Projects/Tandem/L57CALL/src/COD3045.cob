 IDENTIFICATION DIVISION.

*c+
 PROGRAM-ID.     Cod3045.
 AUTHOR.         gauravb.
 INSTALLATION.   Tandem Languages and Tools Quality Assurance Section.
 DATE-WRITTEN.   23 07 1994.
*
* PURPOSE: No parameters passed and Empty linkage section in called cobol
*          routine.
* TPR #: Sxxxxxxxxxxxxxxxx
* ASSOCIATED FILES:  To include all related files needed for program execution
* LOCAL & GLOBAL PARAMETERS: List all of the parameters needed for execution
*                    of the program.
* EXECUTION THREADS: Historic(Normal)/CRE
* COMPILATION INSTRUCTIONS:Use PORT, ENV COMMON     needed.
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
 SOURCE-COMPUTER. TANDEM.
 OBJECT-COMPUTER. TANDEM.

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
           "YYYA" BY "1986" "MMMB" BY "JAN " "YYYB" BY "1986".
     COPY copylib-prog-id IN c00qalib REPLACING "XXXXXXX" BY
  "xxxxxxy".
?LIST

 01 dummy-data                       PIC 9 VALUE 0.
 01 again-dummy-data                 PIC 9 VALUE 0.
 01 cobparam                         PIC X(9) VALUE "No-change".
 01 cobprog                          PIC X(7) VALUE "COB45".

 PROCEDURE DIVISION.
?HEADING "MAIN SECTION"
/
 main SECTION.
     PERFORM copylib-open-file
     PERFORM test-45
     PERFORM copylib-close-file
     STOP RUN
 .

?HEADING "TEST-45 SECTION"
/
 test-45 SECTION.

     PERFORM copylib-test-init
     MOVE "EMPTY LINKAGE SECTION"  TO feature
     MOVE "TEST-45" TO par-name
     MOVE "1" TO correct-a
     MOVE "Expected result 1" TO re-mark
     CALL  COBPROG
     END-CALL
     MOVE return-code TO COMPUTED-A
     DISPLAY " CORR-A  COMPUTED A   "  CORRECT-A      COMPUTED-A   .
     DISPLAY  " RETURN-CODE "  RETURN-CODE
     IF RETURN-CODE = 1 THEN
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
