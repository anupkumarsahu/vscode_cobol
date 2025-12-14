*******************************************************************************
*
* Program: XCB57P0
*
* Purpose: This program has a main program with a number of declared string
*          data items.
*
?symbols
 identification division.
 program-id.  main-program.

 data division.
 working-storage section.

 01 MAIN-ALPHA-A              PIC A VALUE "A".
 01 MAIN-ALPHA-A26            PIC A(26) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".

 01 MAIN-ALPHANUMERIC-X       PIC X VALUE "X".
 01 MAIN-ALPHANUMERIC-X37     PIC X(37) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890 ".

 01 MAIN-REC-01-LEVEL-01.
   05 ALPHA-A              PIC A VALUE "z".
   05 ALPHA-A26            PIC A(26) VALUE "abcdefghijklmnopqrstuvwxyz".
   05 REC-01-LEVEL-02.
     10 ALPHANUMERIC-X       PIC X VALUE "9".
     10 ALPHANUMERIC-X37     PIC X(37) VALUE "abcdefghijklmnopqrstuvwxyz 1234567890".

 procedure division.
 begin-main-program.

    DISPLAY "Main-Program".
    DISPLAY MAIN-ALPHA-A26.
    STOP RUN.

 end program main-program.
******************************************************************************
