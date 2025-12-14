?INSPECT
?BLANK
?RUNNABLE
?SYMBOLS
?OPTIMIZE 0

 IDENTIFICATION DIVISION.

     PROGRAM-ID.  ECOBBUGS.

*===========================================================================*
*                  E N V I R O N M E N T   D I V I S I O N                  *
*===========================================================================*
 ENVIRONMENT DIVISION.

*---------------------------------------------------------------------------*
*                 C O N F I G U R A T I O N   S E C T I O N                 *
*---------------------------------------------------------------------------*
 CONFIGURATION SECTION.

 SOURCE-COMPUTER.      TANDEM NON-STOP.
 OBJECT-COMPUTER.      TANDEM NON-STOP.

*---------------------------------------------------------------------------*
*                  I N P U T - O U T P U T   S E C T I O N                  *
*---------------------------------------------------------------------------*
 INPUT-OUTPUT SECTION.

/
*===========================================================================*
*                         D A T A   D I V I S I O N                         *
*===========================================================================*
 DATA DIVISION.

*---------------------------------------------------------------------------*
*                          F I L E   S E C T I O N                          *
*---------------------------------------------------------------------------*
 FILE SECTION.

/
*---------------------------------------------------------------------------*
*               W O R K I N G - S T O R A G E   S E C T I O N               *
*---------------------------------------------------------------------------*
 WORKING-STORAGE SECTION.

 01  TEST-X                      PIC X(1)       VALUE SPACES.
 01  TEST-X-WS                   PIC X(24)      VALUE SPACES.
 01  NATIVE-2-WS                 NATIVE-2       VALUE ZEROS.
 01  COMP-4-WS                   PIC 9(4) COMP  VALUE ZEROS.
 01  COMP-6-A-WS                 PIC 9(6) COMP  VALUE ZEROS.
 01  COMP-6-B-WS                 PIC 9(6) COMP  VALUE ZEROS.
 01  EDIT-ZZZ9                   PIC ZZZ9       VALUE ZEROS.

 01  GROUP-NAME-WS.
     03  PIC-9-12-WS             PIC 9(12).
     03  PIC-9-3-WS              PIC 9(03).

 01 ANOTHER-REC.
     02 FIRST-PACKED             PIC S99999 COMP        VALUE -23456.
     02 SECOND-PACKED            PIC 99999  COMP        VALUE 34567.

 01  X-WS                        PIC 9(4)       VALUE ZEROS.
 01  TABLE-WS.
     03 A-WS OCCURS 25 TIMES.
       05 B-WS                   NATIVE-2       VALUE ZEROS.

?HEADING "-- Extended Storage Section --"
/
 EXTENDED-STORAGE SECTION.

?HEADING "-- Procedure Division --"
/
*
* P r o c e d u r e   d i v i s i o n
*
 PROCEDURE DIVISION.

/
 MAIN SECTION.
 0010-MAIN.

     MOVE "ABCDEFGHIJKLMNOP"  TO TEST-X-WS.

     PERFORM 25 TIMES
        ADD 1 TO X-WS
        MOVE X-WS TO B-WS(X-WS)
     END-PERFORM.

* ON SIZE ERROR path is always taken, clearing out the result.
     MOVE 17 TO COMP-6-A-WS.
     MOVE 13 TO COMP-6-B-WS.
     COMPUTE NATIVE-2-WS = COMP-6-A-WS + COMP-6-B-WS
        ON SIZE ERROR
            MOVE ZEROS TO NATIVE-2-WS
     END-COMPUTE.
     MOVE NATIVE-2-WS    TO EDIT-ZZZ9.
     DISPLAY "ON SIZE ERR ZZZ9 edited output from NATIVE-2   : " EDIT-ZZZ9.

* No ON SIZE ERROR clause leaves result corrent
     MOVE 17 TO COMP-6-A-WS.
     MOVE 13 TO COMP-6-B-WS.
     COMPUTE NATIVE-2-WS = COMP-6-A-WS + COMP-6-B-WS.
     MOVE NATIVE-2-WS    TO EDIT-ZZZ9.
     DISPLAY "ZZZ9 edited output from NATIVE-2   : " EDIT-ZZZ9.

* ON SIZE ERROR clause is not taken when result field is COMP.
     MOVE 17 TO COMP-6-A-WS.
     MOVE 13 TO COMP-6-B-WS.
     COMPUTE COMP-4-WS = COMP-6-A-WS + COMP-6-B-WS
        ON SIZE ERROR
            MOVE ZEROS TO COMP-4-WS
     END-COMPUTE.
     MOVE COMP-4-WS      TO EDIT-ZZZ9.
     DISPLAY "ZZZ9 edited output from 9(4) COMP  : " EDIT-ZZZ9.

     MOVE 700000001234   TO PIC-9-12-WS.

     DISPLAY "Test conditional breakpoint on PIC-9-12-WS " PIC-9-12-WS.

     DISPLAY "Test conditional breakpoint on PIC-9-12-WS " PIC-9-12-WS.

     DISPLAY "Test conditional breakpoint on PIC-9-12-WS " PIC-9-12-WS.

     STOP RUN.

 0010-MAIN-EXIT.
    EXIT.

*****************************************************************************
*                    E N D   O F   S O U R C E   C O D E                    *
*****************************************************************************
