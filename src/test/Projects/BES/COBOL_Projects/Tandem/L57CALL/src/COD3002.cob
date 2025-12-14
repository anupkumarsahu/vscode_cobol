
 IDENTIFICATION DIVISION.
*c+
 PROGRAM-ID.     cod3002.
 AUTHOR.         gauravb.
 INSTALLATION.   Tandem Languages and Tools Quality Assurance Section.
 DATE-WRITTEN.   dd mmm yyyy.
*
* PURPOSE: CALL POSITIVE TEST for calling mixed-case C function name.
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
 SPECIAL-NAMES. FILE "CCALL2X" IS CPROG.
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
     COPY copylib-prog-id IN c00qalib REPLACING "XXXXXXX" BY "xxxxxxy".
?LIST

 01 dummy-data                       PIC 9 VALUE 0.
 01 cparam                           PIC 9 VALUE 0.

 PROCEDURE DIVISION.
?HEADING "MAIN SECTION"
/
 main SECTION.
     PERFORM copylib-open-file
     PERFORM test-2
     PERFORM copylib-close-file
     STOP RUN
 .

?HEADING "TEST-2 SECTION"
/
 test-2 SECTION.

     PERFORM copylib-test-init
     MOVE "checking mixedcased" TO feature
     MOVE "TEST-2" TO par-name
     MOVE "1" TO correct-a
     MOVE 0 TO return-code
     MOVE "Expected result 1" TO re-mark
     CALL "CfuNc1"  OF CPROG
     USING BY REFERENCE cparam
     END-CALL
     MOVE return-code TO dummy-data
     MOVE dummy-data TO computed-a
     IF dummy-data = 1 AND
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
