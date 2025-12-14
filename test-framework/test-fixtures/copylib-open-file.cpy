      *****************************************************************
      * File Open Copybook
      * Standard file opening procedures
      *****************************************************************
       01  FILE-OPEN-STATUS.
           05  FO-STATUS-CODE      PIC XX.
               88  FO-SUCCESS      VALUE '00'.
               88  FO-EOF          VALUE '10'.
               88  FO-NOT-FOUND    VALUE '35'.
               88  FO-LOCKED       VALUE '48'.
           05  FO-FILE-NAME        PIC X(30).
           05  FO-OPEN-MODE        PIC X(10).
