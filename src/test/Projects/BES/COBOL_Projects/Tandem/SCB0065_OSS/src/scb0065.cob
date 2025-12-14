*******************************************************************************
*
* Program: XCB65P0
*
* Purpose: This program executes an ENTER TAL ABEND.
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

   PERFORM main-call-abend.

   PERFORM main-program-exit.

 main-program-exit.

   DISPLAY "main-program-exit".

   STOP RUN.

 main-call-abend.

   DISPLAY "About to call ABEND".

   ENTER TAL ABEND.

 END PROGRAM main-program.
******************************************************************************
