 IDENTIFICATION DIVISION.
*c+
 PROGRAM-ID.     COD3051.
 AUTHOR.         gauravb.
 INSTALLATION.   Tandem Languages and Tools Quality Assurance Section.
 DATE-WRITTEN.   23 07  1994.
*
* PURPOSE: To call a C function which performs I/O operations and allocates/
*          deallocates memory.
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
 SPECIAL-NAMES. FILE "CCALL51X" IS CPROG.

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
  "COD3051".
?LIST

 01 dummy-data                       PIC 9 VALUE 0.
 01 cparam                           PIC 9 VALUE 0.
 01 cparam-buffer                    PIC 9 VALUE 0.


 PROCEDURE DIVISION.
?HEADING "MAIN SECTION"
/
 main SECTION.
     PERFORM copylib-open-file
     PERFORM test-51
     PERFORM copylib-close-file
     STOP RUN
 .

?HEADING "TEST-51 SECTION"
/
 test-51 SECTION.

     PERFORM copylib-test-init
     MOVE "Call C for I/O oper" TO feature
     MOVE "TEST-51" TO par-name
     MOVE "1" TO correct-a
     MOVE "Expected result 1" TO re-mark
     MOVE  0  TO return-code
     MOVE cparam to cparam-buffer.
     CALL  RWFUNC IN CPROG
     USING BY REFERENCE cparam
     END-CALL
     MOVE return-code TO dummy-data .
     MOVE dummy-data TO computed-a  .
     IF dummy-data = 1  AND
       cparam  NOT  = cparam-buffer THEN
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
