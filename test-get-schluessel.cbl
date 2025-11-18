       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-HIGHLIGHTING.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PARAM-FELD.
          05 DATUM      PIC X(8).
          05 ZEIT-SYS   PIC X(8).
       01 T-IDENT       PIC X(10).
       01 T-STATUS      PIC 9(4).
       01 RECH01-F1     PIC 9(4).
       01 RECH01-F2     PIC 9(4).
       01 CLUSTER-ID    PIC X(8).
       01 CPU           PIC X(4).
       01 RECHF12       PIC X(8).
       01 RECHF22       PIC X(4).
       01 NODE-ID       PIC 9(4).
       01 CPU-NR        PIC 9(4).
       01 TRANSACTION-ID PIC X(10).
       01 TRANS-IDENT   PIC X(10).
       01 WS-DELAY-TIME PIC 9(4).
       
       PROCEDURE DIVISION.
       MAIN SECTION.
       MAIN-START.
           DISPLAY "Starting main section".
           PERFORM GET-SCHLUESSEL.
       
       MAIN-EXIT.
           STOP RUN.
       
       GET-SCHLUESSEL SECTION.
       * --- * ------------------------------------------------
           MOVE FUNCTION CURRENT-DATE (1 : 8) TO DATUM OF PARAM-FELD.

           ENTER TAL GETTRANSID USING T-IDENT GIVING T-STATUS.

           MOVE 0          TO RECH01-F1, RECH01-F2.

           MOVE CLUSTER-ID TO RECHF12.
           MOVE CPU        TO RECHF22.

           IF RECH01-F1 NOT = 0
             COMPUTE NODE-ID = RECH01-F1 - 1
           ELSE
             MOVE RECH01-F1 TO NODE-ID
           END-IF.

           MOVE RECH01-F2 TO CPU-NR.

           MOVE TRANSACTION-ID TO TRANS-IDENT.

           ENTER TAL "DELAY" USING WS-DELAY-TIME
           ACCEPT ZEIT-SYS OF PARAM-FELD FROM TIME.

       GET-SCHLUESSEL-EXIT.