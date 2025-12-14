* one 3rd level routine: main.dog.cat
* has some global and non-global decs.
* Has some globals that are referenced in all routines or only in some.
 IDENTIFICATION DIVISION.
 PROGRAM-ID. main.
 DATA DIVISION.
 FILE SECTION.
 WORKING-STORAGE SECTION.

 01  em PIC x(4)  VALUE "Emma".

* signed short
  01  ms  NATIVE-2 VALUE 22.
  01  mg  NATIVE-2 VALUE 23 GLOBAL.

* unreferenced in main but referenced in dog and/or cat
  01  mgur  NATIVE-2 VALUE 30 GLOBAL.
  01  mgurd NATIVE-2 VALUE 31 GLOBAL.
  01  mgurc NATIVE-2 VALUE 32 GLOBAL.

* this global has the same name as a non-global in dog
* when within cat, this variable should be visible.
  01  maindog NATIVE-2 VALUE 24 GLOBAL.

* Test GLOBAL with records.
* GLOBAL keyword allowed on rec definition only.
* It applies to all record items.
 01  recg GLOBAL.
     02 b   PIC 99 VALUE 01.
     02 r11 PIC X(4) VALUE "Emma".
     02 r12 PIC X(4) VALUE "Sara".
     02 r13 NATIVE-2 VALUE 17.
     02 r14.
        03 r141 PIC X(4) VALUE "Lisa".

 01  rec2.
     02 b   PIC 99 VALUE 01.
     02 r21 PIC X(3) VALUE "r21".
     02 r22 PIC X(3) VALUE "r22".
     02 r23 NATIVE-2 VALUE 17.
     02 r24.
        03 r241 PIC X(4) VALUE "r241".

 01  stmain.
     02 stmi NATIVE-2.
     02 stmj NATIVE-2.
     02 stmsub.
        03 stmsi NATIVE-2.
        03 stmsj NATIVE-2.

* check that visibility attribute = local is placed on conditions
* No visibility attr implies condition is GLOBAL
 77  L77local  NATIVE-2 VALUE  17.
     88 conda       VALUE  1.
     88 condb       VALUE  2.
     88 condc       VALUE  17.

 01  L1local  NATIVE-2 VALUE  17.
     88 na       VALUE  1.
     88 nb       VALUE  2.
     88 nc       VALUE  17.

* GLOBAL not allowed on 77
* 77  L88glob NATIVE-2 VALUE  22 GLOBAL.
 01  L1global  NATIVE-2 VALUE  17 GLOBAL.
     88 nag       VALUE  3.
     88 nbg       VALUE  4.
     88 ncg       VALUE  22.


 PROCEDURE DIVISION.
 start-main.

   DISPLAY "MAIN: Congratulations!".
   Move 17 to ms.
   Move 18 to mg.

*  ENTER TAL DEBUG.

   Move "asil" to r141.
   PERFORM perf-main.

 Call-Dog.
   Call "dog".
   DISPLAY "Returned from dog".
   DISPLAY "Exiting MAIN".
   STOP RUN.

 perf-main.
     display "perf-main".
     add 1 to ms.
     display "perf-main-end".
 perf-one-end.
     Display "Shouldn't be here".

*   Note that there is no END PROGRAM here - that makes the
*   following program a contained program
 IDENTIFICATION DIVISION.
 PROGRAM-ID.  dog.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

* signed short
  01  ds  NATIVE-2 VALUE 122.
  01  dg  NATIVE-2 VALUE 123 GLOBAL.

  01  maindog NATIVE-2 VALUE 124.

* unreferenced in dog but referenced in cat
  01  dgurc  NATIVE-2 VALUE 40 GLOBAL.

  01  stdog.
      02 stdi NATIVE-2.
      02 stdj NATIVE-2.
      02 stdsub.
         03 stdsi NATIVE-2.
         03 stdsj NATIVE-2.

 PROCEDURE DIVISION.
 Start-Dog.

   DISPLAY "In dog".
   Move 117 to ds.
   Move 118 to dg.
   Move 119 to mg.
   Move 120 to mgur.
   Move 121 to mgurd.

*  ENTER TAL DEBUG.

   PERFORM perf-dog.

 Call-Cat.
   Call "cat".
   DISPLAY "Returned from cat".
   DISPLAY "Leaving dog".

 perf-dog.
     display "perf-dog".
     add 1 to ds.
     display "perf-dog-end".
 perf-dog-end.
     Display "Shouldn't be here".

*   Note that there is no END PROGRAM here - that makes the
*   following program a contained program
 IDENTIFICATION DIVISION.
 PROGRAM-ID.  cat.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

* signed short
  01  cs  NATIVE-2 VALUE 222.

  01  stcat.
      02 stci NATIVE-2.
      02 stcj NATIVE-2.
      02 stcsub.
         03 stcsi NATIVE-2.
         03 stcsj NATIVE-2.

 PROCEDURE DIVISION.
 Start-Cat.

   DISPLAY "In cat".
   Move 217 to cs.
   Move 218 to dg.
   Move 219 to mg.
   Move 220 to maindog.
   Move 221 to mgur.
   Move 222 to mgurc.
   Move 223 to dgurc.

   PERFORM perf-cat.

*  ENTER TAL DEBUG.
   DISPLAY "Leaving cat".

 perf-cat.
     display "perf-cat".
     add 1 to cs.
     display "perf-cat-end".
 perf-cat-end.
     Display "Shouldn't be here".

 END PROGRAM cat.
 END PROGRAM dog.
 END PROGRAM main.
