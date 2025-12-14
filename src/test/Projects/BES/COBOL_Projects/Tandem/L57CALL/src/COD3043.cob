 IDENTIFICATION DIVISION.
*c+
 PROGRAM-ID.     COD3043.
 AUTHOR.         gauravb.
 INSTALLATION.   Tandem Languages and Tools Quality Assurance Section.
 DATE-WRITTEN.   06 07 1994.
*
* PURPOSE: Accessing same file from calling and called  program  using file con
*         nectors .
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

 INPUT-OUTPUT SECTION.
 FILE-CONTROL.
    COPY copylib-print-file IN c00qalib.
    SELECT COMMON-FILE   ASSIGN TO "ATSTFILE".


 DATA DIVISION.
 FILE SECTION.
?NOLIST
     COPY copylib-standard-file IN c00qalib.
?LIST
 FD COMMON-FILE.

 01 COMMON-REC                       PIC X(5).

 01 BUFFER-REC                       PIC X(5).

?HEADING "WORKING-STORAGE SECTION"
/
 WORKING-STORAGE SECTION.
?NOLIST
     COPY copylib-standard-data IN c00qalib REPLACING "MMMA" BY "JULY"
           "YYYA" BY "1994" "MMMB" BY "JULY" "YYYB" BY "1994".
     COPY copylib-prog-id IN c00qalib REPLACING "XXXXXXX" BY
     "cod3043".
?LIST

 01 dummy-data                       PIC 9 VALUE 0.
 01 sending-data                     PIC X(7) VALUE "INITIAL".
 01 cobprog                          PIC X(7) VALUE "COB43".

 PROCEDURE DIVISION.
?HEADING "MAIN SECTION"
/
 main SECTION.
     PERFORM copylib-open-file
     PERFORM test-43
     PERFORM copylib-close-file
     STOP RUN
 .

?HEADING "TEST-43 SECTION"
/
 test-43 SECTION.

     PERFORM copylib-test-init
     MOVE "Diff File Connector" TO feature
     MOVE "TEST-43" TO par-name
     MOVE "1" TO correct-a
     MOVE "Expected result 1" TO re-mark
     OPEN INPUT COMMON-FILE
     MOVE "ABCDE" TO COMMON-REC
     MOVE "*****" TO BUFFER-REC
     CLOSE COMMON-FILE
     CALL  COBPROG
     USING BY REFERENCE sending-data
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
