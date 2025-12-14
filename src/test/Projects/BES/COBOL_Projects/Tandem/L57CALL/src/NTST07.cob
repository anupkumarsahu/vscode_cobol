 IDENTIFICATION DIVISION.
*c+
 PROGRAM-ID.     ntst07.
 AUTHOR.         gauravb.
 INSTALLATION.   Tandem Languages and Tools Quality Assurance Section.
 DATE-WRITTEN.   25 07 1994.
*
* PURPOSE: File mnemonic specified without a Special-names.
* TPR #: NONE
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
     COPY copylib-prog-id IN c00qalib REPLACING "XXXXXXX" BY "ntst04".
?LIST

 01 dummy-data                       PIC 9 VALUE 0.
 01 num-data                         PIC X VALUE "Y".
 01 num-dummy                        PIC X.


 PROCEDURE DIVISION.
?HEADING "MAIN SECTION"
/
 main SECTION.
     PERFORM copylib-open-file
     PERFORM test-7
     PERFORM copylib-close-file
     STOP RUN
 .

?HEADING "TEST-7  SECTION"
/
 test-7  SECTION.

     PERFORM copylib-test-init
     MOVE "No spname for filmne" TO feature
     MOVE "TEST-6" TO par-name
     MOVE "1" TO correct-a
     MOVE "Expected result 1" TO re-mark
     MOVE 123 TO num-data
     MOVE num-data  TO num-dummy
     CALL  COBPROG IN COB
     USING BY REFERENCE num-data
     END-CALL
     MOVE return-code TO dummy-data
     MOVE dummy-data TO computed-a
     IF num-data NOT EQUAL num-dummy AND
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
