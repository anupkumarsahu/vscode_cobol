 IDENTIFICATION DIVISION.
*c+
 PROGRAM-ID.     C0D3046.
 AUTHOR.         gauravb.
 INSTALLATION.   Tandem Languages and Tools Quality Assurance Section.
 DATE-WRITTEN.   23 07 1994.
*
* PURPOSE: TO CALL A C PROGRAM AND PASS THE SAME VARIABLES 5 TIMES
* TPR #: Sxxxxxxxxxxxxxxxx
* ASSOCIATED FILES:  To include all related files needed for program execution
* LOCAL & GLOBAL PARAMETERS: List all of the parameters needed for execution
*                    of the program.
* EXECUTION THREADS: Historic(Normal)/CRE
* COMPILATION INSTRUCTIONS: Use ENV COMMOM , PORT
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
 SPECIAL-NAMES. FILE "CCALL46X" IS CPROG.

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
      "cod3046".
?LIST

?HEADING "EXTENDED-STORAGE SECTION"
/
 EXTENDED-STORAGE SECTION.



 01 dummy-data                       PIC 9 VALUE 0.
 01 cparam                           PIC 9 VALUE 0.

 PROCEDURE DIVISION.
?HEADING "MAIN SECTION"
/
 main SECTION.
     PERFORM copylib-open-file
     PERFORM test-46
     PERFORM copylib-close-file
     STOP RUN
 .

?HEADING "TEST-46 SECTION"
/
 test-46 SECTION.

     PERFORM copylib-test-init
     MOVE "feature description" TO feature
     MOVE "TEST-46" TO par-name
     MOVE "1" TO correct-a
     MOVE "Expected result 1" TO re-mark
     CALL  CFUNC     IN CPROG
     USING BY REFERENCE cparam
                        cparam
                        cparam
                        cparam
                        cparam
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
     COPY copylib-open-file  IN C00QALIB.
     COPY copylib-test-init  IN C00QALIB.
     COPY copylib-close-file IN C00QALIB.
     COPY copylib-general-utilities IN C00QALIB.
?LIST
