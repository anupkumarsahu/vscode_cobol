*******************************************************************************
*
* Program: XCB66P0
*
* Purpose: This program executes an ENTER TAL "PROCESS_STOP_".
*
?save STARTUP
?symbols
 identification division.
 program-id.  main-program.

 data division.

 working-storage section.

 01 retval       native-2.

 procedure division.

 main-program-begin.

   DISPLAY "main-program-begin".

   PERFORM main-call-stop.

   PERFORM main-program-exit.

 main-program-exit.

   DISPLAY "main-program-exit".

   STOP RUN.

 main-call-stop.

   DISPLAY "About to call PROCESS_STOP_".

   ENTER TAL "PROCESS_STOP_" GIVING retval.

 END PROGRAM main-program.
******************************************************************************
