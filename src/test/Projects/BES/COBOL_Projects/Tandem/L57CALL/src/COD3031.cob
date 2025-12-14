 IDENTIFICATION DIVISION.
*c+
 PROGRAM-ID.     COD3031.
 AUTHOR.         gauravb.
 INSTALLATION.   Tandem Languages and Tools Quality Assurance Section.
 DATE-WRITTEN.   13 07 1994.
*
* PURPOSE: To CALL a C  & COBOL program without changing return-code and check
* that it remains unaltered.
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
 SPECIAL-NAMES.
 FILE "CCALL31X" IS CPROG
 FIlE "COB31X" IS COBPROG.

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
     COPY copylib-standard-data IN C00QALIB REPLACING "MMMA" BY "JAN "
           "YYYA" BY "1986" "MMMB" BY "JAN " "YYYB" BY "1986".
     COPY copylib-prog-id IN C00QALIB REPLACING "XXXXXXX" BY
     "xxxxxxy".

?LIST

 01 dummy-data                       PIC 9 VALUE 0.
 01 again-dummy-data                 PIC 9 VALUE 0.
 01 cobparam                         PIC 9 VALUE 1.
 77 cparam                           PIC 9 VALUE 1.

 PROCEDURE DIVISION.
?HEADING "MAIN SECTION"
/
 main SECTION.
     PERFORM copylib-open-file
     PERFORM test-31
     PERFORM copylib-close-file
     STOP RUN
 .

?HEADING "TEST-31 SECTION"
/
 test-31 SECTION.

     PERFORM copylib-test-init
     MOVE "feature description" TO feature
     MOVE "TEST-31" TO par-name
     MOVE "1" TO correct-a
     MOVE "Expected result 1" TO re-mark
     MOVE return-code TO dummy-data
     CALL  CFUNC1 IN CPROG
     USING BY REFERENCE cparam
     END-CALL
     CALL  COB31  IN COBPROG
     USING BY REFERENCE cobparam
     END-CALL
     display " prog retutrns with retcode   " return-code .
     MOVE return-code TO again-dummy-data
     IF dummy-data =  again-dummy-data
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
