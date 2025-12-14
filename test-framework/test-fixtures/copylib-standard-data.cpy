      *****************************************************************
      * Standard Data Copybook
      * Contains common data definitions
      *****************************************************************
       01  STANDARD-DATE-FIELDS.
           05  STD-CURRENT-DATE.
               10  STD-YEAR        PIC 9(4).
               10  STD-MONTH       PIC 99.
               10  STD-DAY         PIC 99.
           05  STD-CURRENT-TIME.
               10  STD-HOUR        PIC 99.
               10  STD-MINUTE      PIC 99.
               10  STD-SECOND      PIC 99.
       
       01  STANDARD-CONSTANTS.
           05  STD-MAX-RECORDS     PIC 9(6) VALUE 999999.
           05  STD-COMPANY-NAME    PIC X(30) VALUE 'TEST COMPANY INC'.
           05  STD-VERSION         PIC X(10) VALUE 'V1.0.0'.
