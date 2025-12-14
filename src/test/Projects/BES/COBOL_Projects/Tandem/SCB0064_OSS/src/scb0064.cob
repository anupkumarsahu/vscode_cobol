*******************************************************************************
*
* Program: XCB64P0
*
* Purpose: This program executes a number of ENTER TAL verbs.
*
?save STARTUP
?save PARAM
?symbols
 identification division.
 program-id.  main-program.

 environment division.

 configuration section.

 special-names.
   FILE $system.system.COBOLLIB IS coblib.

 input-output section.

 file-control.
   SELECT seq1-file
     ASSIGN "C000DAT1"
     ORGANIZATION IS SEQUENTIAL
     ACCESS MODE IS SEQUENTIAL
     FILE STATUS IS seq1-status.

 data division.
 file section.

 FD seq1-file
    LABEL RECORDS ARE OMITTED
    RECORD CONTAINS 102 CHARACTERS.

   01  seq1-rec.
      05  composer                    PIC A(8).
      05  FILLER                      PIC X.
      05  title                       PIC A(13).
      05  FILLER                      PIC X.
      05  players                     PIC A(52).

 working-storage section.

 01 startup-value  pic x(30) value spaces.
 01 startup-name   pic x(6) value "VOLUME".
 01 param-value    pic xx value spaces.
 01 param-name     pic x(9) value "BACKUPCPU".
 01 retval         native-2.

 01 seq1-status PIC XX VALUE ZEROS.
 01 error-code PIC S999 COMP.
 01 file-name PIC X(24).
 01 file-number PIC S99 COMP.

 procedure division.

 main-program-begin.

   DISPLAY "main-program-begin".

   PERFORM main-get-startup-volume.

   PERFORM main-get-cobol-fileinfo.

   PERFORM main-get-backupcpu.

   PERFORM main-call-debug.

   PERFORM main-program-exit.

 main-program-exit.

   DISPLAY "main-program-exit".

   STOP RUN.

 main-get-startup-volume.

   ENTER TAL "SMU_Startup_GetText_" USING startup-name startup-value
                                    GIVING retval.
   IF retval < 0
      DISPLAY "SMU_Startup_GetText_ FAILED (This is NOT expected!): " retval
   END-IF.

   DISPLAY "ENTER TAL Startup Volume: " startup-value.

 main-get-cobol-fileinfo.

   ENTER TAL COBOLFILEINFO USING seq1-file,
                                 error-code,
                                 file-name,
                                 file-number.

   DISPLAY "ENTER TAL COBOLFILEINFO error code: " error-code.

 main-get-backupcpu.

   ENTER TAL "SMU_Param_GetText_" USING param-name param-value
                                  GIVING retval.
   IF retval < 0 THEN
      DISPLAY "SMU_Param_GetText_ FAILED (This is expected!): " retval
   ELSE
      DISPLAY "ENTER TAL Backup CPU: " param-value.

 main-call-debug.

   DISPLAY "About to call DEBUG".

   ENTER TAL DEBUG.

 END PROGRAM main-program.
******************************************************************************
