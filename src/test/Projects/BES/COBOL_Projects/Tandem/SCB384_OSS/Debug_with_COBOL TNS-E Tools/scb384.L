COBOL - T0356H01 - (03MAY2018)
(C) 1997-2016 Hewlett Packard Enterprise Development LP.  
2018 June 22, 11:48:55

     Directives: -Wsystype=oss;-g;-Wsuppress;-Wcall_shared

       1.       ?symbols
       2.       ?MAIN sizeof
       3.        IDENTIFICATION DIVISION.
       4.        PROGRAM-ID.  sizeof.
       5.        DATA DIVISION.
       6.        WORKING-STORAGE SECTION.
       7.       
       8.        01 comp1 PIC 9(5)V99 USAGE IS COMP.
       9.        01 comp3 PIC 9(5)V99 USAGE IS COMP-3.
      10.        01 comp5 USAGE IS COMP-5.
      11.        01 native2  USAGE IS NATIVE-2.
      12.        01 native4  USAGE IS NATIVE-4.
      13.        01 native8  USAGE IS NATIVE-8.
      14.        01 IdxItem  USAGE IS INDEX.
      15.        01 packed PIC 9(10) USAGE IS PACKED-DECIMAL.
      16.        01 ptr PIC 9(5) USAGE IS POINTER.
      17.       
      18.        01  Result  PIC 999 VALUE ZEROS.
      19.        01  Result1 PIC 9999 VALUE ZEROS.
      20.        01  Result2 PIC 99v9 VALUE ZEROS.
      21.       
      22.        77 pic1     PIC X(100).
      23.        77 pic2     PIC 9(5)V99.
      24.        77 pic3     PIC X(100).
      25.        77 pic4     PIC 9(2).
      26.        77 pic5     PIC 99.
      27.       
      28.        01 arrays.
      29.             03 firs occurs 5 times.
      30.                 05 sec pic 99 occurs 10 times.
      31.       
      32.        01  employee-record-in.
      33.             02  employee-name-in.
      34.                10 firs pic 99 VALUE ZEROS.
      35.                10 second pic 999 VALUE ZEROS.
      36.             02  employee-rate-in        pic 9(3)v99.
      37.             02  employee-hours-in       pic 9(3)v99.
      38.             02  line-feed-in            pic x(1).
      39.       
      40.        01 simpletable.
      41.             05 item1 occurs 15 times.
      42.                10 item11 pic 99 VALUE ZEROS.
      43.                10 item12 pic 999 VALUE ZEROS.
      44.             05 item2 occurs 20 times.
      45.                10 item21 pic 9(5) VALUE ZEROS.
      46.                10 item22 pic x(10) VALUE ZEROS.
      47.             05 flags PICTURE 9.
      48.                88 BAD VALUE 0.
      49.                88 GOOD VALUE 1.
      50.       
      51.        01 ACTIVITY-TABLE-RECORD.
      52.           03 ACTIVITY-COUNT PICTURE 99.
      53.           03 ACTIVITY-TABLE OCCURS 10  TO 20 TIMES DEPENDING ON ACTIVITY-COUNT.
      54.               05 ACTIVITY-ENTRY PICTURE 999.
      55.       
      56.        PROCEDURE DIVISION.
      57.        CalculateResult.
      58.       
      59.          DISPLAY "length of ACTIVITY-TABLE-RECORD. = ", function length (ACTIVITY-TABLE-RECORD).
      60.          DISPLAY "length of ACTIVITY-COUNT  = ", function length (ACTIVITY-COUNT ).
      61.          DISPLAY "length of ACTIVITY-TABLE = ", function length (ACTIVITY-TABLE(1)).
      62.          DISPLAY "length of ACTIVITY-ENTRY = ", function length (ACTIVITY-ENTRY(1)).
      63.       
      64.          MOVE 15 TO ACTIVITY-COUNT.
      65.       
      66.          DISPLAY "length of ACTIVITY-TABLE-RECORD. = ", function length (ACTIVITY-TABLE-RECORD).
      67.          DISPLAY "length of ACTIVITY-COUNT  = ", function length (ACTIVITY-COUNT ).
      68.          DISPLAY "length of ACTIVITY-TABLE = ", function length (ACTIVITY-TABLE(1)).
      69.          DISPLAY "length of ACTIVITY-ENTRY = ", function length (ACTIVITY-ENTRY(1)).
      70.       
      71.       
      72.          MOVE 5 TO ACTIVITY-COUNT.
      73.       
      74.          DISPLAY "length of ACTIVITY-TABLE-RECORD. = ", function length (ACTIVITY-TABLE-RECORD).
      75.          DISPLAY "length of ACTIVITY-COUNT  = ", function length (ACTIVITY-COUNT ).
      76.          DISPLAY "length of ACTIVITY-TABLE = ", function length (ACTIVITY-TABLE(1)).
      77.          DISPLAY "length of ACTIVITY-ENTRY = ", function length (ACTIVITY-ENTRY(1)).
      78.       
      79.          MOVE 25 TO ACTIVITY-COUNT.
      80.       
      81.          DISPLAY "length of ACTIVITY-TABLE-RECORD. = ", function length (ACTIVITY-TABLE-RECORD).
      82.          DISPLAY "length of ACTIVITY-COUNT  = ", function length (ACTIVITY-COUNT ).
      83.          DISPLAY "length of ACTIVITY-TABLE = ", function length (ACTIVITY-TABLE(1)).
      84.          DISPLAY "length of ACTIVITY-ENTRY = ", function length (ACTIVITY-ENTRY(1)).
      85.       
      86.          DISPLAY "length of comp1 = ", function length (comp1).
      87.          DISPLAY "length of comp3 = ", function length (comp3).
      88.          DISPLAY "length of comp5 = ", function length (comp5).
      89.          DISPLAY "length of native2 = ", function length (native2).
      90.          DISPLAY "length of native4 = ", function length (native4).
      91.          DISPLAY "length of native8 = ", function length (native8).
      92.          DISPLAY "length of IdxItem = ", function length (IdxItem ).
      93.          DISPLAY "length of packed = ", function length (packed).
      94.          DISPLAY "length of ptr = ", function length (ptr).
      95.       
      96.          DISPLAY "length of Result = ",  function length (Result).
      97.          DISPLAY "length of Result1 = ", function length (Result1).
      98.          DISPLAY "length of Result2 = ", function length (Result2).
      99.       
     100.          DISPLAY "length of pic1 = ", function length (pic1).
     101.          DISPLAY "length of pic2 = ", function length (pic2).
     102.          DISPLAY "length of pic3 = ", function length (pic3).
     103.          DISPLAY "length of pic4 = ", function length (pic4).
     104.          DISPLAY "length of pic5 = ", function length (pic5).
     105.       
     106.          DISPLAY "length of employee-record-in = ", function length (employee-record-in).
     107.          DISPLAY "length of employee-record-in.employee-name-in = ", function length (employee-name-in of employee-record-
                in).
     108.          DISPLAY "length of employee-record-in.employee-rate-in  = ", function length (employee-rate-in of employee-
                record-in).
     109.          DISPLAY "length of employee-record-in.employee-hours-in = ", function length (employee-hours-in of employee-
                record-in).
     110.          DISPLAY "length of arrays.firs   == ", function length (arrays).
     111.          DISPLAY "length of second   == ", function length (second ).
     112.       
     113.          DISPLAY "length of simpletable = ", function length (simpletable).
     114.          DISPLAY "length of item1 = ", function length (item1(1)).
     115.          DISPLAY "length of item11 = ", function length (item11(1)).
     116.          DISPLAY "length of item12 = ", function length (item12(1)).
     117.          DISPLAY "length of item2 = ", function length (item2(1)).
     118.          DISPLAY "length of item21 = ", function length (item21(1)).
     119.          DISPLAY "length of item22 = ", function length (item22 (1)).
     120.          DISPLAY "length of flags = ", function length (flags).
     121.       *DISPLAY "length of bad = ", function length (bad in flags in simpletable ).
     122.       *DISPLAY "length of good = ", function length (good).
     123.       
     124.        STOP RUN.
     125.        END PROGRAM sizeof.

