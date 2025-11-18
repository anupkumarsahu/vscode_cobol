       IDENTIFICATION DIVISION.
       PROGRAM-ID. LINTER-TEST.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-VAR PIC X(10).
       
       PROCEDURE DIVISION.
       MAIN SECTION.
           PERFORM TEST-SECTION.
           STOP RUN.
       
       TEST-SECTION SECTION.
           DISPLAY "This is referenced".
       
       UNREFERENCED-SECTION SECTION.
           DISPLAY "This should be flagged as unreferenced".
       
       ANOTHER-UNREFERENCED SECTION.
           DISPLAY "This should also be flagged".