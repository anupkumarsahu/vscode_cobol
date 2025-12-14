?symbols
?MAIN sizeof
 IDENTIFICATION DIVISION.
 PROGRAM-ID.  sizeof.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

 01 comp1 PIC 9(5)V99 USAGE IS COMP.
 01 comp3 PIC 9(5)V99 USAGE IS COMP-3.
 01 comp5 USAGE IS COMP-5.
 01 native2  USAGE IS NATIVE-2.
 01 native4  USAGE IS NATIVE-4.
 01 native8  USAGE IS NATIVE-8.
 01 IdxItem  USAGE IS INDEX.
 01 packed PIC 9(10) USAGE IS PACKED-DECIMAL.
 01 ptr PIC 9(5) USAGE IS POINTER.

 01  Result  PIC 999 VALUE ZEROS.
 01  Result1 PIC 9999 VALUE ZEROS.
 01  Result2 PIC 99v9 VALUE ZEROS.

 77 pic1     PIC X(100).
 77 pic2     PIC 9(5)V99.
 77 pic3     PIC X(100).
 77 pic4     PIC 9(2).
 77 pic5     PIC 99.

 01 arrays.
      03 firs occurs 5 times.
          05 sec pic 99 occurs 10 times.

 01  employee-record-in.
      02  employee-name-in.
         10 firs pic 99 VALUE ZEROS.
         10 second pic 999 VALUE ZEROS.
      02  employee-rate-in        pic 9(3)v99.
      02  employee-hours-in       pic 9(3)v99.
      02  line-feed-in            pic x(1).

 01 simpletable.
      05 item1 occurs 15 times.
         10 item11 pic 99 VALUE ZEROS.
         10 item12 pic 999 VALUE ZEROS.
      05 item2 occurs 20 times.
         10 item21 pic 9(5) VALUE ZEROS.
         10 item22 pic x(10) VALUE ZEROS.
      05 flags PICTURE 9.
         88 BAD VALUE 0.
         88 GOOD VALUE 1.

 01 ACTIVITY-TABLE-RECORD.
    03 ACTIVITY-COUNT PICTURE 99.
    03 ACTIVITY-TABLE OCCURS 10  TO 20 TIMES DEPENDING ON ACTIVITY-COUNT.
        05 ACTIVITY-ENTRY PICTURE 999.

 PROCEDURE DIVISION.
 CalculateResult.

   DISPLAY "length of ACTIVITY-TABLE-RECORD. = ", function length (ACTIVITY-TABLE-RECORD).
   DISPLAY "length of ACTIVITY-COUNT  = ", function length (ACTIVITY-COUNT ).
   DISPLAY "length of ACTIVITY-TABLE = ", function length (ACTIVITY-TABLE(1)).
   DISPLAY "length of ACTIVITY-ENTRY = ", function length (ACTIVITY-ENTRY(1)).

   MOVE 15 TO ACTIVITY-COUNT.

   DISPLAY "length of ACTIVITY-TABLE-RECORD. = ", function length (ACTIVITY-TABLE-RECORD).
   DISPLAY "length of ACTIVITY-COUNT  = ", function length (ACTIVITY-COUNT ).
   DISPLAY "length of ACTIVITY-TABLE = ", function length (ACTIVITY-TABLE(1)).
   DISPLAY "length of ACTIVITY-ENTRY = ", function length (ACTIVITY-ENTRY(1)).


   MOVE 5 TO ACTIVITY-COUNT.

   DISPLAY "length of ACTIVITY-TABLE-RECORD. = ", function length (ACTIVITY-TABLE-RECORD).
   DISPLAY "length of ACTIVITY-COUNT  = ", function length (ACTIVITY-COUNT ).
   DISPLAY "length of ACTIVITY-TABLE = ", function length (ACTIVITY-TABLE(1)).
   DISPLAY "length of ACTIVITY-ENTRY = ", function length (ACTIVITY-ENTRY(1)).

   MOVE 25 TO ACTIVITY-COUNT.

   DISPLAY "length of ACTIVITY-TABLE-RECORD. = ", function length (ACTIVITY-TABLE-RECORD).
   DISPLAY "length of ACTIVITY-COUNT  = ", function length (ACTIVITY-COUNT ).
   DISPLAY "length of ACTIVITY-TABLE = ", function length (ACTIVITY-TABLE(1)).
   DISPLAY "length of ACTIVITY-ENTRY = ", function length (ACTIVITY-ENTRY(1)).

   DISPLAY "length of comp1 = ", function length (comp1).
   DISPLAY "length of comp3 = ", function length (comp3).
   DISPLAY "length of comp5 = ", function length (comp5).
   DISPLAY "length of native2 = ", function length (native2).
   DISPLAY "length of native4 = ", function length (native4).
   DISPLAY "length of native8 = ", function length (native8).
   DISPLAY "length of IdxItem = ", function length (IdxItem ).
   DISPLAY "length of packed = ", function length (packed).
   DISPLAY "length of ptr = ", function length (ptr).

   DISPLAY "length of Result = ",  function length (Result).
   DISPLAY "length of Result1 = ", function length (Result1).
   DISPLAY "length of Result2 = ", function length (Result2).

   DISPLAY "length of pic1 = ", function length (pic1).
   DISPLAY "length of pic2 = ", function length (pic2).
   DISPLAY "length of pic3 = ", function length (pic3).
   DISPLAY "length of pic4 = ", function length (pic4).
   DISPLAY "length of pic5 = ", function length (pic5).

   DISPLAY "length of employee-record-in = ", function length (employee-record-in).
   DISPLAY "length of employee-record-in.employee-name-in = ", function length (employee-name-in of employee-record-in).
   DISPLAY "length of employee-record-in.employee-rate-in  = ", function length (employee-rate-in of employee-record-in).
   DISPLAY "length of employee-record-in.employee-hours-in = ", function length (employee-hours-in of employee-record-in).
   DISPLAY "length of arrays.firs   == ", function length (arrays).
   DISPLAY "length of second   == ", function length (second ).

   DISPLAY "length of simpletable = ", function length (simpletable).
   DISPLAY "length of item1 = ", function length (item1(1)).
   DISPLAY "length of item11 = ", function length (item11(1)).
   DISPLAY "length of item12 = ", function length (item12(1)).
   DISPLAY "length of item2 = ", function length (item2(1)).
   DISPLAY "length of item21 = ", function length (item21(1)).
   DISPLAY "length of item22 = ", function length (item22 (1)).
   DISPLAY "length of flags = ", function length (flags).
*DISPLAY "length of bad = ", function length (bad in flags in simpletable ).
*DISPLAY "length of good = ", function length (good).

 STOP RUN.
 END PROGRAM sizeof.
