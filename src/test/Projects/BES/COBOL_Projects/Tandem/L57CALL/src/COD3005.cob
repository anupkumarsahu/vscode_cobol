?main cod3005
 IDENTIFICATION DIVISION.
*c+
 PROGRAM-ID.     COD3005.
 AUTHOR.         gauravb.
 INSTALLATION.   Tandem Languages and Tools Quality Assurance Section.
 DATE-WRITTEN.   06 07 1994.
*
* PURPOSE: include lengthy description of what the program tests and why
*          it exists.
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
 SPECIAL-NAMES. FILE "COB5X"  IS COBPROG.

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
     COPY copylib-prog-id IN c00qalib REPLACING "XXXXXXX" BY
     "cod3005".
?LIST


 01 dummy-data                          PIC 9 VALUE 0.

 01 num-data                            PIC 9(3)  VALUE 123.

 01 num-dummy                           PIC 9(3)  VALUE 0.

 PROCEDURE DIVISION.
?HEADING "MAIN SECTION"
/
 main SECTION.
     PERFORM copylib-open-file
     PERFORM test-5
     PERFORM copylib-close-file
     STOP RUN
 .

?HEADING "TEST-5 SECTION"
/
 test-5 SECTION.

     PERFORM copylib-test-init
     MOVE "CALL COB same routine" TO feature
     MOVE "TEST-5" TO par-name
     MOVE "1" TO correct-a
     MOVE "Expected result 1" TO re-mark
     MOVE 123 TO num-data
     CALL  COB5 IN COBPROG
     USING BY REFERENCE  num-data
     END-CALL
     MOVE return-code TO dummy-data
     MOVE dummy-data TO computed-a
     IF num-data NOT EQUAL  num-dummy   AND
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

 END PROGRAM COD3005.

 IDENTIFICATION DIVISION.
*c+
 PROGRAM-ID.     COB5.
 AUTHOR.         gauravb.
 INSTALLATION.   Tandem Languages and Tools Quality Assurance Section.
 DATE-WRITTEN.   06 07 1994.
*
* PURPOSE: include lengthy description of what the program tests and why
*          it exists.
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

 DATA DIVISION.

 WORKING-STORAGE SECTION.
 01 buffer-num                     PIC 9(3).

 LINKAGE SECTION.

 01 recieved-num                   PIC 9(3).

 PROCEDURE DIVISION USING RECIEVED-NUM.

 CALLED-PROG.

 MOVE recieved-num TO buffer-num
 MOVE 1 TO return-code
 MOVE 456 TO recieved-num
*Changing recieved-num from '123' to '456'.Reflected in corresponding parameter
*(ie num-data) of the calling program.
     IF buffer-num  = 123  THEN
         EXIT PROGRAM.

 END PROGRAM COB5.
