       IDENTIFICATION DIVISION.
       PROGRAM-ID. TANDEM-HIGHLIGHTING-FIXED.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.
           IBM-370.
       OBJECT-COMPUTER.
           TANDEM T16.
       
       OBJECT-COMPUTER.            TANDEM T16.
       
       OBJECT-COMPUTER. TANDEM T16.
       
       * The line above should show TANDEM T16 in normal computer name highlighting
       * not as a comment!
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COMPUTER-NAME PIC X(20) VALUE "TANDEM T16".
       
       PROCEDURE DIVISION.
       MAIN-START.
           DISPLAY "Computer: " WS-COMPUTER-NAME.
           STOP RUN.