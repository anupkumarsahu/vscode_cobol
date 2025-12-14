 IDENTIFICATION DIVISION.
*c+
 PROGRAM-ID.     cod3050.
 AUTHOR.         gauravb.
 INSTALLATION.   Tandem Languages and Tools Quality Assurance Section.
 DATE-WRITTEN.   21 07 1994.
*
* PURPOSE: To call a VOID C function and check that return-code is not changed.
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
 SOURCE-COMPUTER. TANDEM.
 OBJECT-COMPUTER. TANDEM.
 SPECIAL-NAMES. FILE "CALL50X" IS CPROG.

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
 01 cparam                           PIC 9 VALUE 0.

 PROCEDURE DIVISION.
?HEADING "MAIN SECTION"
/
 main SECTION.
     PERFORM copylib-open-file
     PERFORM test-50
     PERFORM copylib-close-file
     STOP RUN
 .

?HEADING "TEST-50 SECTION"
/
 test-50 SECTION.

     PERFORM copylib-test-init
     MOVE "feature description" TO feature
     MOVE "TEST-50" TO par-name
     MOVE "1" TO correct-a
     MOVE "Expected result 1" TO re-mark
     MOVE 1 TO return-code .
     display " RETURN-CODE BEFORE   " return-code
     CALL  NORET  IN CPROG
     USING BY REFERENCE cparam
     END-CALL
     MOVE return-code TO computed-a .
     display " RETURN-CODE AFTER    " return-code
     IF return-code = 1 THEN
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
