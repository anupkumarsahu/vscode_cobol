       IDENTIFICATION DIVISION.
       PROGRAM-ID. SECTION-TEST.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-VAR PIC X(10).
       
       PROCEDURE DIVISION.
       MAIN SECTION.
       MAIN-START.
           DISPLAY "Starting main section".
           PERFORM GET-SCHLUESSEL.
       
       MAIN-EXIT.
           STOP RUN.
       
       GET-SCHLUESSEL SECTION.
       GET-SCHLUESSEL-START.
           DISPLAY "Getting Schluessel".
       
       GET-SCHLUESSEL-EXIT.
           EXIT.