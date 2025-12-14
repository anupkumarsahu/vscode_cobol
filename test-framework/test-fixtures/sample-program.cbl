       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAMPLE-PROGRAM.
       AUTHOR. TEST-AUTHOR.
       DATE-WRITTEN. 2025-12-01.
      *****************************************************************
      * Sample COBOL Program for Testing
      * Tests: Syntax highlighting, navigation, IntelliSense
      *****************************************************************
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. TANDEM/16.
       OBJECT-COMPUTER. TANDEM/16.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO "CUSTDATA"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CUST-ID
               FILE STATUS IS WS-FILE-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-FILE.
       01  CUSTOMER-RECORD.
           05  CUST-ID             PIC 9(6).
           05  CUST-NAME           PIC X(30).
           05  CUST-BALANCE        PIC S9(7)V99 COMP-3.
           05  CUST-STATUS         PIC X.
               88  ACTIVE-CUSTOMER VALUE 'A'.
               88  INACTIVE-CUSTOMER VALUE 'I'.
       
       WORKING-STORAGE SECTION.
      * TODO: Add more test variables
       01  WS-FILE-STATUS          PIC XX.
           88  FILE-OK             VALUE '00'.
           88  FILE-NOT-FOUND      VALUE '35'.
       
       01  WS-COUNTERS.
           05  WS-TOTAL-RECORDS    PIC 9(6) VALUE ZERO.
           05  WS-ACTIVE-COUNT     PIC 9(6) VALUE ZERO.
           05  WS-INACTIVE-COUNT   PIC 9(6) VALUE ZERO.
       
       01  WS-AMOUNTS.
           05  WS-TOTAL-BALANCE    PIC S9(9)V99 COMP-3 VALUE ZERO.
           05  WS-AVG-BALANCE      PIC S9(7)V99 COMP-3 VALUE ZERO.
       
       01  WS-FLAGS.
           05  WS-EOF-FLAG         PIC X VALUE 'N'.
               88  END-OF-FILE     VALUE 'Y'.
           05  WS-ERROR-FLAG       PIC X VALUE 'N'.
               88  ERROR-OCCURRED  VALUE 'Y'.
       
      * Copybook includes
       COPY COPYLIB-STANDARD-DATA IN B30QALIB.
       COPY COPYLIB-OPEN-FILE.
       
      * ! IMPORTANT: This is a critical section
      * ? QUESTION: Should we add validation here?
       01  WS-DISPLAY-LINE         PIC X(80).
       
       LINKAGE SECTION.
       01  LS-PARM-AREA.
           05  LS-RETURN-CODE      PIC S9(4) COMP.
           05  LS-MESSAGE          PIC X(50).
       
       PROCEDURE DIVISION USING LS-PARM-AREA.
       
       MAIN-LOGIC SECTION.
       0000-MAIN-PARA.
      * Main entry point
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-FILE
               UNTIL END-OF-FILE
           PERFORM 3000-FINALIZE
           GOBACK.
       
       1000-INITIALIZE SECTION.
       1100-OPEN-FILES.
      * Open customer file
           OPEN INPUT CUSTOMER-FILE
           IF NOT FILE-OK
               DISPLAY 'ERROR OPENING CUSTOMER FILE: ' WS-FILE-STATUS
               SET ERROR-OCCURRED TO TRUE
               MOVE 8 TO LS-RETURN-CODE
               GOBACK
           END-IF.
       
       1200-INIT-VARIABLES.
           INITIALIZE WS-COUNTERS
           INITIALIZE WS-AMOUNTS
           MOVE 'N' TO WS-EOF-FLAG
           MOVE 'N' TO WS-ERROR-FLAG.
       
       2000-PROCESS-FILE SECTION.
       2100-READ-RECORD.
           READ CUSTOMER-FILE NEXT RECORD
               AT END
                   SET END-OF-FILE TO TRUE
               NOT AT END
                   PERFORM 2200-PROCESS-RECORD
           END-READ.
       
       2200-PROCESS-RECORD.
           ADD 1 TO WS-TOTAL-RECORDS
           
           IF ACTIVE-CUSTOMER
               ADD 1 TO WS-ACTIVE-COUNT
               ADD CUST-BALANCE TO WS-TOTAL-BALANCE
           ELSE
               ADD 1 TO WS-INACTIVE-COUNT
           END-IF
           
      * SQL block for testing
           EXEC SQL
               UPDATE CUSTOMER_TABLE
                  SET LAST_ACCESSED = CURRENT_TIMESTAMP
                WHERE CUSTOMER_ID = :CUST-ID
           END-EXEC
           
           PERFORM 2300-DISPLAY-INFO.
       
       2300-DISPLAY-INFO.
           STRING 'Customer: ' CUST-ID ' - ' CUST-NAME
               DELIMITED BY SIZE
               INTO WS-DISPLAY-LINE
           END-STRING
           DISPLAY WS-DISPLAY-LINE.
       
       3000-FINALIZE SECTION.
       3100-CALCULATE-AVERAGE.
           IF WS-ACTIVE-COUNT > ZERO
               DIVIDE WS-TOTAL-BALANCE BY WS-ACTIVE-COUNT
                   GIVING WS-AVG-BALANCE
                   ON SIZE ERROR
                       DISPLAY 'ERROR CALCULATING AVERAGE'
                       SET ERROR-OCCURRED TO TRUE
               END-DIVIDE
           END-IF.
       
       3200-DISPLAY-SUMMARY.
           DISPLAY 'Processing Summary:'
           DISPLAY 'Total Records: ' WS-TOTAL-RECORDS
           DISPLAY 'Active Customers: ' WS-ACTIVE-COUNT
           DISPLAY 'Inactive Customers: ' WS-INACTIVE-COUNT
           DISPLAY 'Total Balance: ' WS-TOTAL-BALANCE
           DISPLAY 'Average Balance: ' WS-AVG-BALANCE.
       
       3300-CLOSE-FILES.
           CLOSE CUSTOMER-FILE.
       
       3999-EXIT.
           MOVE 0 TO LS-RETURN-CODE
           MOVE 'PROCESSING COMPLETE' TO LS-MESSAGE
           EXIT SECTION.
