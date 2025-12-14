 IDENTIFICATION DIVISION.
*c+
 PROGRAM-ID.     ntst26.
 AUTHOR.         gauravb.
 INSTALLATION.   Tandem Languages and Tools Quality Assurance Section.
 DATE-WRITTEN.   25 07 1994.
*
* PURPOSE: To call a C program in Old Environment & without PORT directive.
* TPR #: NONE.
* ASSOCIATED FILES:  To include all related files needed for program execution
* LOCAL & GLOBAL PARAMETERS: List all of the parameters needed for execution
*                    of the program.
* EXECUTION THREADS: Historic(Normal)/CRE
* COMPILATION INSTRUCTIONS: none.
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
 SPECIAL-NAMES.   FILE "NOBJ20" IS CPROG.

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
     COPY copylib-prog-id IN c00qalib REPLACING "XXXXXXX" BY "ntst26".
?LIST

 01 dummy-data                       PIC 9(7) VALUE 0.
 01 num-data-comp                    PIC 999  VALUE 123.
 01 num-dummy-comp                   PIC 999 .

 PROCEDURE DIVISION.
?HEADING "MAIN SECTION"
/
 main SECTION.
     PERFORM copylib-open-file
     PERFORM test-26
     PERFORM copylib-close-file
     STOP RUN
 .

?HEADING "TEST-26 SECTION"
/
 test-26 SECTION.

     PERFORM copylib-test-init
     MOVE "Call C frm ENV OLD" TO feature
     MOVE "TEST-20" TO par-name
     MOVE "1" TO correct-a
     MOVE "Expected result 1" TO re-mark
     MOVE num-data-comp TO num-dummy-comp
     CALL  CFUNC   IN CPROG
     USING BY VALUE   num-data-comp
     END-CALL
     MOVE return-code  TO  dummy-data
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
     COPY copylib-open-file  IN c00qalib.
     COPY copylib-test-init  IN c00qalib.
     COPY copylib-close-file IN c00qalib.
     COPY copylib-general-utilities IN c00qalib.
?LIST
