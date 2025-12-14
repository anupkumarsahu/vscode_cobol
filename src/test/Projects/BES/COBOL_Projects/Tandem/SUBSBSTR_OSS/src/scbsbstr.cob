 IDENTIFICATION DIVISION.
 PROGRAM-ID. main.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

   01 rec.
     02 arr occurs 4 times.
       03 arr2 occurs 3 times.
          04 int2 Native-2.
            88 nata value 1.
          04 int5 Native-2.
       03 another Native-2.
       03 oneMore Native-2.
   01 rec3.
     02 array Native-2 occurs 10 times.
   01 rec4.
  02 val1 native-2.
      02 val2 native-2.
      02 val3 native-2.

  01 rec5.
    02 char1 pic X(10).
    02 char2 pic X(10).
    02 rec7.
      03 val4 pic x(10).
      03 val5 pic x(10).
      03 val6 pic x(10).

  01 rec15.
    02 val7 native-2.
    02 val8 native-2.
    02 rec17.
      03 val9 native-2.
      03 val10 native-2.
      03 val11 native-2.

  01 H PIC X(10).
  01 rec9.
    02 string-array occurs 3 times.
      03 X pic X(10).
      03 another-array occurs 3 times.
        04 Y pic X(10).

 01 rec6.
  02 array Native-2 occurs 10 times.
  02 another2 Native-2.
 PROCEDURE DIVISION.
 main-start.

   Display "Start main".
   Move "12" to val4(1:2).
   Move "45" to val4(4:5).
  Move "hi there" to H.
  Display H(1:4).
   Move 1 to int2 of arr2 of arr of rec(2, 3).
   Move 2 to int2 (1, 2).
   Display "End main".

 END PROGRAM main.
