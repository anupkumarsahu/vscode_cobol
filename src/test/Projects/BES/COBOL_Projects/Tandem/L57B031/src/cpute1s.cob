?errorfile errs
******************************************************************************
*                                                                            *
* PURPOSE: Various Compute Tests for Cobol85/Nmcobol from the Mexico         *
*          Inverlat source listings and other Genesis cases                  *
*                                                                            *
* TPR #:                                                                     *
* ASSOCIATED FILES         : CPUTE1Sr (Output Reference)                     *
* LOCAL & GLOBAL PARAMETERS: None                                            *
* EXECUTION THREADS        : All                                             *
* COMPILATION INSTRUCTIONS : None                                            *
*                                                                            *
* EXECUTION INSTRUCTIONS   : None                                            *
*                                                                            *
* CLEANUP                  : None                                            *
*                                                                            *
* MODIFIER       |   DATE     |  DESCRIPTION OF CHANGE                       *
*----------------------------------------------------------------------------*
* Sushil Thampy    01/06/2000   Initial release.                             *
*                                                                            *
******************************************************************************
?SYMBOLS
 IDENTIFICATION DIVISION.
 PROGRAM-ID. CPUTE1s.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER.  ABD.
 OBJECT-COMPUTER.  ABD.
 INPUT-OUTPUT SECTION.
 FILE-CONTROL.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

 01  T01-F1   PIC S9(11)V9(07).
 01  T01-F2   PIC S9(5)V9(4) COMP VALUE  253.0214.
 01  T01-F3   PIC 9(4) COMP       VALUE  13.

 01  T02-F1   PIC S9(16)V9(02).
 01  T02-F2.
     03 T02-F3 OCCURS 4 TIMES.
        05 T02-F4   PIC 9(15)V9(02).
 01  T02-F5   PIC 9(2) COMP.

 01  T03-F1   PIC 9(05)V9(06) VALUE ZEROES.
 01  t03-f2   PIC 9(05)V9(06) VALUE 2897.094254.

 01  t04-f1   PIC 9(10)V9(02)  value 98525.57.
 01  t04-f2   PIC  9(02)V9(03) value 53.013.

 01  t05-f1   PIC S9(12)V9(06)      value 12.000012.
 01  t05-f2   pic 9(6)              value 37.
 01  t05-f3   pic 9(6)              value 8.
 01  t05-f4   pic S9(14)V9(4) comp  value 23.0023.
 01  t05-f5   pic 9(6)              value 15.
 01  t05-f6   PIC S9(14)V9(4) COMP  value 16.0016.

 77 t06-f1    PIC S9(08)V9(06)      VALUE ZEROES.
 77 t06-f2    PIC S9(16)V9(02) COMP VALUE -654892459.77.
 77 t06-f3    PIC S9(16)V9(02) COMP VALUE 1458972035.21.

 77 t07-f1    PIC  9(08)V9(10) COMP value zeroes.
 77 t07-f2    PIC  9(05)V9(04) COMP value 0.2564.
 77 t07-f3    PIC  9(04)       COMP value 1569.

 77 t08-f1    PIC  9(08)V9(10) COMP value 0.3568784524.
 77 t08-f2    PIC  9(04)       COMP value 1568.
 77 t08-f3    PIC  9(08)V9(10) COMP value 0.3568784524.
 77 t08-f4    PIC  9(04)       COMP value 1568.


 01 t09-f1    PIC S9(12)V9(06) COMP VALUE ZEROS.
 01 t09-f2    PIC S9(14)V9(04) COMP VALUE 101.0011.
 01 t09-f3    PIC 9(03)V9(03)       value 101.101.


 01 t10-f1    PIC S9(12)V9(06) COMP VALUE ZEROES.
 01 t10-f2    PIC S9(07)V9(09) COMP VALUE -3654.010901091.

 01 t11-f1    PIC S9(16)V9(02)      VALUE ZEROES.
 01 t11-f2    PIC 9(04)V9(09)       VALUE 237.000000019.
 01 t11-f3    PIC S9(6)V9(6)   COMP value -101010.000009.
 01 t11-f4    PIC 9(04)             VALUE 282.

 01 t12-f1    PIC S9(10)V9(08) COMP value zeroes.
 01 t12-f2    PIC S9(04)V9(08) COMP value 1000.5.
 01 t12-f3    PIC 9(04)        COMP value 47.
 01 t12-f4    PIC S9(10)V9(08) COMP value 100.6.

 01 t13-f1    PIC S9(12)V9(06) COMP value zeroes.
 01 t13-f2    PIC S9(12)V9(06) COMP value 999.999999.
 01 t13-f3    PIC S9(05)V9(04) COMP value -29.1119.
 01 t13-f4    PIC S9(04)            value -3.

 01 t14-f1    PIC 9(04)V9(05)       VALUE ZEROS.
 01 t14-f2    PIC S9(11)V9(07) COMP value 555555555.5555555.
 01 t14-f3    PIC S9(11)V9(07) COMP value 666666666.2222222.
 01 t14-f4    PIC S9(04)            value 482.

 01 t15-f1    PIC S9(18)       COMP value zeroes.
 01 t15-f2    PIC S9(18)       comp value 287.
 01 t15-f3    PIC S9(11)V9(7)  COMP value 27.0000879.
 01 t15-f4    PIC 9(02)V9(03)       VALUE 0.24.
 01 t15-f5    PIC 9(04)V9(05)       value 24.24001.
 01 t15-f6    PIC S9(04)            value 17.

 01 t16-f1    PIC S9(18)       COMP value zeroes.
 01 t16-f2    PIC S9(18)       comp value 287.
 01 t16-f3    PIC 99                value 17.
 01 t16-f4    PIC s9(4)        comp VALUE 17.
 01 t16-f5    PIC s9(04)            value 117.
 01 t16-f6    PIC S9(4)V9(5)   COMP value 117.00117.

 01 t17-f1    PIC S9(4)V9(8)   COMP value zeroes.
 01 t17-f2    PIC S9(16)V9(2)  COMP value -178891.91.
 01 t17-f3    PIC S9(16)V9(2)  COMP value 78891.36.
 01 t17-f4    PIC S9(4)        COMP value 101.

 01 t18-f1    PIC S9(08)V9(08) COMP VALUE ZEROS.
 01 t18-f2    PIC S9(4)V9(8)   COMP value 802.00000001.
 01 t18-f3    pic 9(4)         comp value 17.

 01 t19-f1    PIC S9(10)V9(8) COMP value 2.
 01 t19-f2    PIC S9(10)      COMP value 1.
 01 t19-f3    PIC S9(8)V9(10) COMP value 1.
 01 t19-f4    PIC S9(8)V9(10) COMP value 1.

 01 t19-f5    pic s9(8)       comp value 1.


 01 t20-f1    pic S9(16)V9(2) COMP value zeroes.
 01 t20-f2-top.
 02 t20-f2-table occurs 2 times.
    03 t20-f2 pic s9(18)      comp.
 01 t20-f3    PIC S9(4)V9(8)  COMP value 363.90000009.
 01 t20-f4    PIC S9(10)V9(8) COMP value 909.3000002.
 01 t20-f5    pic s9(4)       comp value 3.

 01 t21-f1    PIC S9(10)V9(8) COMP value 9063.20000008.
 01 t21-f2    PIC S9(18)      COMP value 8008.
 01 t21-f3    PIC S9(10)V9(8) COMP value -3003.00000003.
 01 t21-f4    PIC S9(18)      COMP value -8011.

 01 t22-f1    PIC S9(9)V9(9)  COMP value zeroes.
 01 t22-f2    pic s9(18)           value 365.
 01 t22-f3    PIC 9(08)V9(04)      value 4.3.
 01 t22-f4    PIC 9(06)V9(06)      value 2.4.
 01 t22-f5    pic s9(4)            value 9000.

 01 t23-f1    PIC S9(09)V9(9) COMP value zeroes.
 01 t23-f2    pic s9(18)           value 1024.
 01 t23-f3    PIC S9(3)V9(10) COMP value -101.0000000001.
 01 t23-f4    PIC S9(3)V9(10) COMP value -102.0000000002.
 01 t23-f5    PIC S9(3)V9(10) COMP value  103.0000000003.

 01 t24-f1    PIC S9(10)V9(06) SIGN LEADING SEPARATE
                                   value 2200.000003.
 01 t24-f2    PIC 9(09)V9(06)      value 3600012.019107.
 01 t24-f3    pic s9(4)       comp value -323.

 01 t25-f1    PIC S9(16)V9(02)     VALUE ZEROES.
 01 t25-f2    PIC S9(10)V9(08)     VALUE 100.
 01 t25-f3    PIC S9(10)V9(8) COMP value 100.
 01 t25-f4    PIC S9(16)V9(02)     VALUE 12.

 01 t26-f1    pic 9(18).
 01 t26-f2    pic 9(3)             value 999.
 01 t26-f3    pic 9(15)            value 900000000000001.

 01 t27-f1    pic 9(8).
 01 t27-f2    pic 9(9)             value 15632.
 01 t27-f3    pic 9(9)             value 75632.

 01 t28-f1    PIC 9(06)V9(12) comp VALUE 0.
 01 t28-f2    PIC 9(05)       COMP VALUE 13382.
 01 t28-f3    PIC 9V9(08)     COMP VALUE .08.
 01 t28-f4    PIC 9(04)       COMP VALUE 1.
 01 t28-f5    PIC 9(04)       COMP VALUE 19.
 01 t28-f6    PIC 9(04)       COMP VALUE 366.

 01 t29-f1    PIC s9(10)V9(8) comp VALUE 0.
 01 t29-f2    PIC s9(4)       COMP VALUE 1338.
 01 t29-f3    PIC s9(4)       COMP VALUE 1339.
 01 t29-f4    PIC s9(4)       COMP VALUE 3.

 01 t30-f1    pic s9(4)v9(14) comp value 0.
 01 t30-f2    PIC s9(10)V9(8) comp VALUE 16.5.
 01 t30-f3    PIC s9(4)       COMP VALUE 1.
 01 t30-f4    PIC s9(4)       COMP VALUE 5.
 01 t30-f5    PIC s9(4)       COMP VALUE 200.
 01 t30-f6    PIC s9(4)       COMP VALUE 2.

 01 t31-f1    PIC s9(10)V9(8) comp VALUE 0.
 01 t31-f2    PIC s9(04)V9(8) comp VALUE 100.000289.
 01 t31-f3    PIC s9(4)       COMP VALUE 7.
 01 t31-f4    PIC s9(04)V9(8) comp VALUE 439.00289.

 01 t32-f1    PIC S9(6)V9(12) COMP value 0.
 01 t32-f2    PIC S9(10)V9(8) COMP VALUE 10.000289.
 01 t32-f3    PIC S9(10)V9(8) COMP VALUE 1947.00747.
 01 t32-f4    PIC s9(04)      comp VALUE 439.

 01 t33-f1    PIC S9(10)V9(8) COMP value 0.
 01 t33-f2    PIC S9(10)V9(8) COMP VALUE 200108.010828.
 01 t33-f3    PIC S9(10)V9(8) COMP VALUE 1947.00747.
 01 t33-f4    PIC s9(04)      comp VALUE 4393.

 01 t34-f1    pic s9(10)v9(8) comp value 0.
 01 t34-f2    pic s9(10)v9(8) comp value 101.101.

 01 t35-f1    PIC S9(2)V9(16) COMP value 0.
 01 t35-f2    pic s9(2)v9(16) comp value 10.
 01 t35-f3    pic s9(5)v9(13) comp value 7.
 01 t35-f4    pic s9(2)v9(16) comp value 1.
 01 t35-f5    pic s9(3)       comp value 2.

 01 t36-f2    PIC S9(13)V9(2) COMP value 1500.
 01 t36-f3    PIC S9(11)V9(6) COMP value 217.
 01 t36-f4    PIC S9(11)V9(6) COMP value 0.
 01 t36-f5    PIC S9(11)V9(6) COMP value 20000.
 01 t36-f6    PIC S9(11)V9(6) COMP value 5000.
 01 t36-f7    PIC S9(11)V9(6) COMP value 0.
 01 t36-f8    PIC S9(11)V9(6) COMP value 20000.
 01 t36-f9    PIC S9(11)V9(6) COMP value 5000.
 01 t36-f10   PIC S9(11)V9(6) COMP value 217.
 01 t36-f1    PIC S9(11)V9(6) COMP value 0.

 01 t37-f1    PIC S9(9)       COMP value 164.
 01 t37-f2    pic s9(6)v9(4)  comp value 11.1645.
 01 t37-f3    pic 99               value 9.
 01 t37-f4    pic s9(9)       comp value 180.

 01 t38-f1    PIC S9(10)v9(8) COMP value 0.
 01 t38-f2    PIC S9(10)v9(8) comp value 2.00000002.
 01 t38-f3    PIC S9(10)v9(8)      value 1.00000001.
 01 t38-f4    pic s9(4)       comp value 180.

 01 t39-f1    pic s9(4)       comp value 100.
 01 t39-f2    pic 9(4)        comp value 50.
 01 t39-f3    pic 9(5)        comp value 50.
 01 t39-f4    pic 9(10)       comp value 50.

 01 t40a-res  PIC 9(4)        COMP VALUE 0.
 01 t40a-f1   PIC 9(4)        COMP VALUE 102.
 01 t40a-f2   PIC 9(4)        COMP VALUE 0.
 01 t40b-res  PIC 9(5)        COMP VALUE 0.
 01 t40b-f1   PIC 9(5)        COMP VALUE 102.
 01 t40b-f2   PIC 9(5)        COMP VALUE 0.
 01 t40c-res  PIC 9(10)       COMP VALUE 0.
 01 t40c-f1   PIC 9(10)       COMP VALUE 102.
 01 t40c-f2   PIC 9(10)       COMP VALUE 0.

 01 t41-f1    pic 9(6)  comp.
 01 t41-f2    pic 9     comp.

 01 t42-f1       pic 9(6) comp.
 01 t42-table-var.
    05 t42-table-x occurs 5 times.
       10 t42-table-y occurs 5 times.
          15 t42-f2-subs  pic 9(4) comp.
          15 t42-f3       pic 9(4) comp.
          15 t42-f4       pic 9(4) comp.

 01 t43-f1                     Pic S99(4) SIGN LEADING.
 01 t43-f2                     Pic 9(3).
 01 t43-f2a                    pic 9(5)  comp.
 01 t43-f2b                    pic 9(10) comp.
 01 t43-f2c                    pic s9(5) comp.
 01 t43-f2d                    Pic 9(5).
 01 t43-f2e                    Pic 9(10).

 01 t44-f1                     pic s9(5)v9(2) comp value 9810.00.

 01 t45-main.
    02 t45-tab occurs 10.
       05 t45-f1               pic 9(15)v9(3) comp.
       05 t45-f2               pic 9(15)v9(3).

 01 t46-f1                     pic s9(4) comp.
 01 t46-f2                     native-4  value 1234.
 01 t46-f3                     native-4  value 2000.
 01 t46-f4                     native-4  value 2000.
 01 t46-f5                     native-2  value 1234.
 01 t46-f6                     native-2  value 2000.
 01 t46-f7                     native-2  value 2000.
 01 t46-f8                     pic s9(4) comp value 1234.
 01 t46-f9                     pic s9(4) comp value 2000.
 01 t46-f10                    pic s9(4) comp value 2000.

 01 t47-f1                     PIC S9(4) COMP.
 01 t47-f2.
     02  t47-f3     PIC S9(15)V9(3) COMP.
     02  t47-f4     PIC S9(4) COMP.
     02  t47-f5     PIC S9(4) COMP.

 01 t48-f1a                   PIC 9(13)V9(04).



 01 ws-i      pic 9(4) comp.
 01 ws-j      pic 9(4) comp.

 01 wf-test-pass pic x    value "y".
    88 wf-test-pass-true  value "y".
    88 wf-test-pass-false value "f".

 EXTENDED-STORAGE SECTION.
 PROCEDURE DIVISION.

 START-IT.

     perform TEST-01
     perform test-02
     perform test-03
     perform test-04
     perform test-05
     perform test-06
     perform test-07
     perform test-08
     perform test-09
     perform test-10
     perform test-11
     perform test-12
     perform test-13
     perform test-14
     perform test-15
     perform test-16
     perform test-17
     perform test-18
     perform test-19
     perform test-20
     perform test-21
     perform test-22
     perform test-23
     perform test-24
     perform test-25
     perform test-26
     perform test-27
     perform test-28
     perform test-29
     perform test-30
     perform test-31
     perform test-32
     perform test-33
     perform test-34
     perform test-35
     perform test-36
     perform test-37
     perform test-38
     perform test-39
     perform test-40
     perform test-41
     perform test-42
     perform test-43
     perform test-44
     perform test-45
     perform test-46
     perform test-47

     if wf-test-pass-true
        display "PASS"
     else
        display "FAIL"
     end-if

     STOP RUN
     .

*
* siintr - line #2156
*
 TEST-01.
     COMPUTE T01-F1 = (T01-F2 * T01-F3) / 36000

     if t01-f1 not = 00000000000.0913688
        display "test-01:fails:expected:00000000000.0913688 actual:" t01-f1
        set wf-test-pass-false to true
     end-if

     .

*
* siintr - line #2405
 TEST-02.
     MOVE 2 TO T02-F5
     MOVE 123456789012345.09 TO T02-F4 (T02-F5)
     COMPUTE T02-F1 ROUNDED = 1.00 * T02-F4 (T02-F5)

     if t02-f1 not = 123456789012345.09
        display "test-02:fails:expected:123456789012345.09 actual:" t02-f1
        set wf-test-pass-false to true
     end-if
     .

*
* s8orden - line #2011
 TEST-03.
     COMPUTE T03-F1 = t03-f2 * ( 1 - .15 )
     if t03-f1 not = 2462.530115
        display "test-03:fails:expected:2462.530115 actual:" t03-f1
        set wf-test-pass-false to true
     end-if
     .

*
* s8orden - line #2287
 TEST-04.
     COMPUTE T04-f1 = t04-f1 * ( 1 + t04-f2 / 100 )

     if t04-f1 not = 150756.93
        display "test-04:fails:expected:150756.93 actual:" t04-f1
        set wf-test-pass-false to true
     end-if
     .
*
* s8orden - line #2162
 TEST-05.
     COMPUTE t05-f1 = t05-f1 + ((t05-f2 + t05-f3) * t05-f4) +
                               ((t05-f5 * 2 ) * t05-f6)

     if t05-f1 not = 1527.151512
        display "test-05:fails:expected:1527.151512 actual:" t05-f1
        set wf-test-pass-false to true
     end-if
     .

*
* s8iposi - line 13280
  test-06.

     compute t06-f1 = (t06-f2 * 100.00000) / t06-f3
        on size error
          display "test-06 failed"
     end-compute

     if t06-f1 not = -44.887252
        display "test-06:fails:expected:-44.887252 actual:" t06-f1
        set wf-test-pass-false to true
     end-if

  .

*
* s8iposi - line 13600
  test-07.

     compute t07-f1 = (t07-f2 * t07-f3 / 36000) + 1

     if t07-f1 not = 1.0111747666
        display "test-07:fails:expected:1.0111747666 actual:" t07-f1
        set wf-test-pass-false to true
     end-if
  .

*
* s8iposi - line 13690
  test-08.

     compute t08-f1 = (t08-f1 - 1) * (36000 / t08-f2)

     if t08-f1 not = 14.7655457357
        display "test-08:fails:expected:14.7655457357 actual:" t08-f1
        set wf-test-pass-false to true
     end-if

     compute t08-f3 = (36000 / t08-f4) * (t08-f3 - 1)

     if t08-f1 not = t08-f3
        display "test-08b:fails:different results:" t08-f1 ":" t08-f3
        set wf-test-pass-false to true
     end-if

  .

*
* s8confir - line 1948
  test-09.

     compute t09-f1 = (t09-f2 / ( 1 + t09-f3 / 100 ))

     if t09-f1 not = 50.224066
        display "test-09:fails:expected:50.224066 actual:" t09-f1
        set wf-test-pass-false to true
     end-if
  .

*
* s8confir - line 2273
  test-10.

     compute t10-f1 = (t10-f2 * 1000 * 91 ) / 360

     if t10-f1 not = -923652.755553
        display "test-02:fails:expected:-923652.755553 actual:" t10-f1
        set wf-test-pass-false to true
     end-if
  .

*
* s8confir - line 2373
  test-11.

     compute t11-f1 = ((( t11-f2 - t11-f3 ) * 10 * 91 ) / 360 ) * t11-f4

     if t11-f1 not = 72172236.50
        display "test-11:fails:expected:72172236.50 actual:" t11-f1
        set wf-test-pass-false to true
     end-if
  .

*
* cwhats - line 1240
  test-12.

     compute t12-f1 rounded = ( 1 + (( t12-f2 * t12-f3 ) / 36000 )) * t12-f4

     if t12-f1 not = 232.00455833
        display "test-12:fails:expected:232.00455833:actual:" t12-f1
        set wf-test-pass-false to true
     end-if
  .

*
* cvalor - line 1143
  test-13.

     compute t13-f1 rounded = t13-f2 * ( 1 + t13-f3 * t13-f4 / 36000 )
     if t13-f1 not = 1002.425991
        display "test-13:fails:expected:1002.425991 actual:" t13-f1
        set wf-test-pass-false to true
     end-if
  .

*
* cservprc - line 2706
  test-14.

     compute t14-f1 = (( t14-f2 - t14-f3 ) / t14-f3 ) * 36000 / t14-f4
     if t14-f1 not = 12.44813
        display "test-14:fails:expected:12.44813 actual:" t14-f1
        set wf-test-pass-false to true
     end-if
  .

*
* cservprc - line 2722
  test-15.

     compute t15-f1 = t15-f2 * t15-f3 * t15-f4 * t15-f5 / 36000 * t15-f6
     if t15-f1 not = 21
        display "test-15:fails:expected:21 actual:" t15-f1
        set wf-test-pass-false to true
     end-if
  .

*
* cservprc - line 3067
  test-16.

     compute t16-f1 = t16-f2 * (30 * t16-f3 - (t16-f4 - t16-f5) * t16-f6 / 36000 )

     if t16-f1 not = 146463
        display "test-16:fails:expected:146463 actual:" t16-f1
        set wf-test-pass-false to true
     end-if
  .

*
* clsocext - line 12200
  test-17.

     compute t17-f1 = ( ( t17-f2 * 36000 ) / (( t17-f3 - t17-f2) * t17-f4 ))

     if t17-f1 not = -247.35295300
        display "test-17:fails:expected:-247.35295300 actual:" t17-f1
        set wf-test-pass-false to true
     end-if
  .

*
* clorden - line 2031
  test-18.

     compute t18-f1 rounded =  1 + ( t18-f2 * t18-f3 ) / 36000

     if t18-f1 not = 1.37872222
        display "test-18:fails:expected:1.37872222 actual:" t18-f1
        set wf-test-pass-false to true
     end-if
  .

*
* clcordp - line 677
  test-19.

*    this one always gave an error (except if t19-f2 is s9(8) comp )
     compute t19-f1 = t19-f1 / ( t19-f2 + ( t19-f2 / t19-f3 * t19-f4 ) )
     if t19-f1 not = 1
        display "test-19a:fails:expected:1 actual:" t19-f1
        set wf-test-pass-false to true
     end-if

*    this one gave a run time error
     move 2 to t19-f1
     compute t19-f1 = t19-f1 / ( t19-f2 / t19-f3 * t19-f4  )
     if t19-f1 not = 2
        display "test-19b:fails:expected:2 actual:" t19-f1
        set wf-test-pass-false to true
     end-if

*    this one does not ( when the division is done last )
     move 2 to t19-f1

     compute t19-f1 = t19-f1 / ( t19-f4 * t19-f2 / t19-f3 )
     if t19-f1 not = 2
        display "test-19c:fails:expected:2 actual:" t19-f1
        set wf-test-pass-false to true
     end-if

*    this one always gave an error (except if t19-f5 is > s9(8) comp )
     move 2 to t19-f1
     compute t19-f1 = t19-f1 / ( t19-f5 + ( t19-f5 / t19-f3 * t19-f4 ))
       if t19-f1 not = 1
        display "test-19d:fails:expected:1 actual:" t19-f1
        set wf-test-pass-false to true
     end-if
  .

*
* clcordp - line 892
  test-20.
     move 2 to ws-i
     move 469 to t20-f2(ws-i)

     compute t20-f1 rounded =  t20-f2(ws-i) * t20-f3 / 100 * t20-f4 / 360 * t20-f5

     if t20-f1 not = 12932.45
        display "test-20:fails:expected:12932.45 actual:" t20-f1
        set wf-test-pass-false to true
     end-if
  .

*
* cfasgemm - line 1124
  test-21.

     compute t21-f1 = ((t21-f1 * t21-f2) + (t21-f3 * t21-f4)) /
                       (t21-f2 + t21-f4)

     if t21-f1 not = -32211712.86696032
        display "test-21:fails:expected:-32211712.86696032 actual:" t21-f1
        set wf-test-pass-false to true
     end-if
  .

*
* cedonima - line 8719
  test-22.

     compute t22-f1 rounded  = t22-f2 * ((( t22-f3 / t22-f4) - 1 ) /
                       ( 1 + (( t22-f3 * t22-f5) / 36000 )))

     if t22-f1 not = 139.257028112
        display "test-22:fails:expected: 139.257028112:actual:" t22-f1
        set wf-test-pass-false to true
     end-if
  .

*
* cedonima - line 8767
  test-23.

     compute t23-f1 = t23-f2 * ((t23-f3 / t23-f4) / t23-f5)

     if t23-f1 not = 9.844279459
        display "test-23:fails:expected:9.844279459 actual:" t23-f1
        set wf-test-pass-false to true
     end-if
  .

*
* cboprep1 - line 799
  test-24.

     compute t24-f1 rounded = t24-f1 / (1 + t24-f2 / 3600 * t24-f3 )

     if t24-f1 not = -0.006811
        display "test-24:fails:expected:-0.006811 actual:" t24-f1
        set wf-test-pass-false to true
     end-if
  .

*
* cbcobcom - line 785
  test-25.

  compute t25-f1 = (((t25-f2 * (t25-f3 / 100)) / 100) * (1 / 12)) * t25-f4

  if t25-f1 not = 1
     display "test-25:fails:expected:1 actual:" t25-f1
     set wf-test-pass-false to true
  end-if
  .

*
* b8saldop - line 8010
  test-26.

     compute t26-f1  = ((t26-f2 + t26-f3 / 1000000000000000)) / 100

     if t26-f1 not = 9
        display "test-26:fails:expected:9 actual:" t26-f1
        set wf-test-pass-false to true
     end-if
  .

*
* soln# 10-990420-0660
  test-27.

     compute t27-f1  rounded  = ( t27-f2 / t27-f3 ) * 100
     if t27-f1 not = 21
        display "test-27:fails:expected:21 actual::" t27-f1
        set wf-test-pass-false to true
     end-if
  .

*
* soln# 10-000828-9990
  test-28.

     COMPUTE t28-f1 = (t28-f2 / ((1 + t28-f3) ** t28-f4))
        * (1 / (1 + t28-f5 * t28-f3 / t28-f6))

     IF t28-f1 NOT = 12339.494751608378
        display "test-28:fails:expected:12339.494751608378:actual:" t28-f1
        set wf-test-pass-false to true
     end-if
  .

*
* cfcalcmd - line 1746
  test-29.

     compute t29-f1 = (t29-f2 - ( t29-f3 / 6 * t29-f4 )) * (12 / 365 )

     IF t29-f1 NOT = 21.97808219
        display "test-29:fails:expected:21.97808219:actual:" t29-f1
        set wf-test-pass-false to true
     end-if
  .

*
* cfcalcmd - line 3138
  test-30.

     compute t30-f1 = t30-f2 ** ( 1 - ( t30-f3 / t30-f4 ))

     IF t30-f1 NOT = 9.41861739748447
        display "test-30:fails:expected:9.41861739748447:actual:" t30-f1
        set wf-test-pass-false to true
     end-if
*
*    A modification of the above test

     move zeroes to t30-f1

     compute t30-f1 = t30-f5 / t30-f2 ** ( 1 -  t30-f3 / t30-f4 * t30-f6 )

     IF t30-f1 NOT = 37.19971641525147
        display "test-30b:fails:expected:37.19971641525147:actual" t30-f1
        set wf-test-pass-false to true
     end-if
  .

*
* cfcalcmd - line 4050
  test-31.
     compute t31-f1 = t31-f2 / ( 1 + (( t31-f3 * t31-f4 ) / 36000 ))

     IF t31-f1 NOT = 92.13545261
        display "test-31:fails:expected:92.13545261:actual:" t31-f1
        set wf-test-pass-false to true
     end-if
  .

*
* cfcalcmd - line 4041
  test-32.
     compute t32-f1 = ( 1 - t32-f2 / t32-f3 ) / (t32-f4 / 36000)

     IF t32-f1 NOT = 81.583361093155
        display "test-32:fails:expected:81.583361093155:actual:" t32-f1
        set wf-test-pass-false to true
     end-if
  .

*
* cfcalcmd - line 553
  test-33.
     compute t33-f1 = ( t33-f2 / t33-f3 - 1 ) /  (t33-f4 / 36000)

     IF t33-f1 NOT = 834.04958827
        display "test-33:fails:expected:834.04958827:actual:" t33-f1
        set wf-test-pass-false to true
     end-if

  .

*
* Soln # 10-010614-1104, Case # 10-010719-9977
* This one used to cause compiler logic error
  test-34.

     compute t34-f1 rounded =
             t34-f2 - (((( t34-f2 * t34-f2 ) / t34-f2 ) * t34-f2 ) +
             (( t34-f2 * t34-f2 ) / t34-f2 ))

     IF t34-f1 NOT = -10221.412201
        display "test-34:fails:expected:-10221.412201:actual:" t34-f1
        set wf-test-pass-false to true
     end-if

  .

*
* Soln # 10-010614-1104, Case # 10-010719-9977
* This one used to cause compiler logic error
  test-35.

     compute t35-f1 = t35-f2 + t35-f3 + ( 1 - t35-f3 ) / t35-f4 ** t35-f5

     IF t35-f1 >= 11 and
        t35-f1 <= 11.0000000000000004
        continue
     else
        display "test-35:fails:expected:11:actual:" t35-f1
        set wf-test-pass-false to true
     end-if
  .

*
* Soln # 10-010614-1104, Case # 10-010719-9977
* This one used to cause compiler logic error
  test-36.

      compute t36-f1 ROUNDED =
        (t36-f2 - t36-f3) * ((t36-f4 + t36-f5 - t36-f6 ) /
          (t36-f7 + t36-f8 - t36-f9)   ) + t36-f10

      IF t36-f1 NOT = 1500
        display "test-36:fails:expected:1500:actual:" t36-f1
        set wf-test-pass-false to true
     end-if
  .

*
* Soln # 10-000817-9795
* This one used to cause precision error
  test-37.

      compute t37-f1 = (t37-f1 * t37-f2 / t37-f3) * (t37-f1 / t37-f4)

      IF t37-f1 NOT = 185
        display "test-37:fails:expected:185:actual:" t37-f1
        set wf-test-pass-false to true
     end-if
  .

*
* cfcalcmd - line 1135
  test-38.
     compute t38-f1 = (( t38-f2 ** t38-f3 ) - 1 ) * (36000 / t38-f4)

     IF t38-f1 NOT = 200.00000677
        display "test-38:fails:expected:200.00000677:actual:" t38-f1
        set wf-test-pass-false to true
     end-if

  .

*  Test 39 - Multiply by negative power of 10
*  CASE 10-011123-2237, SOLN 10-011126-4903
*  (Some more tests in addition to test-48 in tprt8s)
 Test-39.
     move 100 to t39-f1
     move 50  to t39-f2
     move 50  to t39-f3
     move 50  to t39-f4

*                    = 100    + ( -10 * 50 )
     COMPUTE t39-f1  = t39-f1 + ( -10 *  t39-f2 )

*    t39-f1 should be -400
     IF t39-f1 > 0
        display "test-39a:fails:expected:-400:actual:" t39-f1
        set wf-test-pass-false to true
     END-IF

     move 100 to t39-f1
     COMPUTE t39-f1  = t39-f1 + (  -1 *  t39-f3 )
     IF t39-f1 NOT = 50
        display "test-39b:fails:expected:50:actual:" t39-f1
        set wf-test-pass-false to true
     END-IF

     move 100 to t39-f1
     COMPUTE t39-f1  = t39-f1 + (  t39-f4 * -1 )
     IF t39-f1 NOT = 50
        display "test-39c:fails:expected:50:actual:" t39-f1
        set wf-test-pass-false to true
     END-IF

     .

*  Test 40 - DIVIDE w/REMANIDER and power of 10
*  CASE 10-011123-2244, SOLN 10-011126-4904
*  (some more tests in addition to test-49 in tprt8s)
  Test-40.
      move 0   to t40a-res
      move 0   to t40a-f2
      move 102 to t40a-f1

      DIVIDE t40a-f1 BY 10 GIVING t40a-res REMAINDER t40a-f2
      IF t40a-res NOT = 10   and
         t40a-f2  not = 2
         display "test-40a:fails:expected dividend 10:divisor 2:actual:"
                 t40a-res ":" t40a-f2
         set wf-test-pass-false to true
      END-IF

      move 0   to t40b-res
      move 0   to t40b-f2
      move 102 to t40b-f1

      DIVIDE t40b-f1 BY 10 GIVING t40b-res REMAINDER t40b-f2
      IF t40b-res NOT = 10   and
         t40b-f2  not = 2
         display "test-40b:fails:expected dividend 10:divisor 2:actual:"
                 t40b-res ":" t40b-f2
         set wf-test-pass-false to true
      END-IF

      move 0   to t40c-res
      move 0   to t40c-f2
      move 102 to t40c-f1

      DIVIDE t40c-f1 BY 100 GIVING t40c-res REMAINDER t40c-f2
      IF t40c-res NOT = 1   and
         t40c-f2  not = 2
         display "test-40c:fails:expected dividend 10:divisor 2:actual:"
                 t40c-res ":" t40c-f2
         set wf-test-pass-false to true
      END-IF


      .

*  Test 41 - compute with exponentiation causes failure 0
*  SOLN 10-011213-5398
  Test-41.
      move 1   to t41-f1
      move 1   to t41-f2

      compute  t41-f1 = (t41-f2 * 2) * (10 ** t41-f2)
      IF t41-f1  NOT = 20
         display "test-41:fails:expected 20:actual:" t41-f1
         set wf-test-pass-false to true
      END-IF

      compute  t41-f1 = (t41-f2 * 2) / (10 ** t41-f2)
      compute  t41-f1 = (10 ** t41-f2) / (t41-f2 * 2)
      compute  t41-f1 = (10 ** t41-f2) * (10 ** t41-f2)
      compute  t41-f1 = 10 / 10 ** t41-f2 / 10
      compute  t41-f1 = 1 / ( 10 ** t41-f2 ) * 2
      compute  t41-f1 = 10 * ( 10 ** t41-f2 / 10 ** t41-f2 / 10 )
      compute  t41-f1 = 10 *  10 ** t41-f2 ** t41-f2
      compute  t41-f1 = (10 ** t41-f2)/ ( 10 / (1  ** t41-f2 / (1  ** t41-f2 + 1 )))
      compute  t41-f1 = 10 * 10 ** t41-f2 ** 1
      move 1 to t41-f1
      compute  t41-f1 = (10 ** t41-f1)
      move 1 to t41-f1
      compute  t41-f1 = (100 * t41-f1 * (10 ** t41-f1))
      move 1 to t41-f1
      compute  t41-f1 rounded = (t41-f1 * t41-f1) / (10 ** t41-f1)

      .

*  Test 42 - DIVIDE literal INTO item subscripted variable caused failure
*  SOLN 10-010907-2898
  Test-42.

*
*     Divide A into B

      move 2   to ws-j
      move 2   to ws-i
      move 10  to t42-f1
      move 100 to t42-f2-subs (ws-i, ws-j)

      divide 10 into t42-f2-subs (2, ws-j)
      if t42-f2-subs(2, 2) not = 10
         display "test-42a:fails:expected dividend 3:actual:" t42-f2-subs(2, 2)
         set wf-test-pass-false to true
      end-if

      move 10  to t42-f1
      move 100 to t42-f2-subs (ws-i, ws-j)

      divide t42-f1 into t42-f2-subs (2 , 2)
      if t42-f2-subs(2, 2) not = 10
         display "test-42b:fails:expected dividend 3:actual:" t42-f2-subs(2, 2)
         set wf-test-pass-false to true
      end-if

*
*     Divide A into B giving C

      move 2   to ws-j
      move 2   to ws-i
      move 10  to t42-f1
      move 101 to t42-f2-subs (ws-i, ws-j)

      divide 10 into t42-f2-subs (ws-i, ws-j) giving t42-f3 (2, ws-j)
      if t42-f3 (2, 2) not = 10
         display "test-42c:fails:expected dividend 10:actual:" t42-f3 (2, 2)
         set wf-test-pass-false to true
      end-if

      move 2   to ws-j
      move 2   to ws-i
      move 10  to t42-f1
      move 101 to t42-f2-subs (ws-i, ws-j)

      divide t42-f1 into t42-f2-subs (ws-i, ws-j) giving t42-f3 (2, ws-j)
      if t42-f3 (2, 2) not = 10
         display "test-42d:fails:expected dividend 10:actual:" t42-f3 (2, 2)
         set wf-test-pass-false to true
      end-if

*
*     Divide B by A giving C

      move 2   to ws-j
      move 2   to ws-i
      move 10  to t42-f1
      move 101 to t42-f2-subs (ws-i, ws-j)

      divide t42-f2-subs (ws-i, ws-j) by 10  giving t42-f3 (2, ws-j)
      if t42-f3 (2, 2) not = 10
         display "test-42e:fails:expected dividend 10:actual:" t42-f3 (2, 2)
         set wf-test-pass-false to true
      end-if

      move 2   to ws-j
      move 2   to ws-i
      move 10  to t42-f1
      move 101 to t42-f2-subs (ws-i, ws-j)

      divide t42-f2-subs (ws-i, 2) by t42-f1 giving t42-f3 (2, ws-j)
      if t42-f3 (2, 2) not = 10
         display "test-42f:fails:expected dividend 10:actual:" t42-f3 (2, 2)
         set wf-test-pass-false to true
      end-if

*
*     Divide A into B giving C remainder D

      move 2   to ws-j
      move 2   to ws-i
      move 10  to t42-f1
      move 101 to t42-f2-subs (ws-i, ws-j)
      move 0   to t42-f3 (2, 2)
      move 0   to t42-f4 (2, 2)

      divide 10 into t42-f2-subs (ws-i, ws-j) giving t42-f3 (2, ws-j)
                                              remainder t42-f4 (ws-i, 2)

      if not ( t42-f3 (2, 2) = 10 and t42-f4 (2, 2) = 1 )
         display "test-42g fails:expected dividend 10:actual:" t42-f3 (2, 2)
         set wf-test-pass-false to true
      end-if

      move 2   to ws-j
      move 1   to ws-i
      move 10  to t42-f1
      move 101 to t42-f2-subs (ws-i, ws-j)
      move 0   to t42-f3 (2, 2)
      move 0   to t42-f4 (2, 2)

      divide t42-f1 into t42-f2-subs (ws-i, ws-j) giving t42-f3 (2, ws-j)
                                              remainder t42-f4 (2, ws-i + 1)

      if not ( t42-f3 (2, 2) = 10  AND  t42-f4 (2, 2) = 1 )

         display "test-42h fails:expected dividend 10:actual:" t42-f3 (2, 2)
         set wf-test-pass-false to true
      end-if

*
*     Divide B by A giving C remainder D

      move 2   to ws-j
      move 2   to ws-i
      move 10  to t42-f1
      move 101 to t42-f2-subs (ws-i, ws-j)

      divide t42-f2-subs (ws-i, ws-j) by 10  giving t42-f3 (2, ws-j)
                          remainder t42-f4  (ws-i, 2)

      if not ( t42-f3 (2, 2) = 10 and t42-f4 (2, 2) = 1 )
         display "test-42i:fails:expected dividend 10:actual:" t42-f3 (2, 2)
         set wf-test-pass-false to true
      end-if

      move 2   to ws-j
      move 2   to ws-i
      move 10  to t42-f1
      move 101 to t42-f2-subs (ws-i, ws-j)
      move 0   to t42-f3 (2, 2)
      move 0   to t42-f4 (2, 2)

      divide t42-f2-subs (ws-i, 2) by t42-f1 giving t42-f3 (2, ws-j)
                         remainder t42-f4 (ws-i, 2)

      if not ( t42-f3 (2, 2) = 10 and t42-f4 (2, 2) = 1 )
         display "test-42j:fails:expected dividend 10:actual:" t42-f3 (2, 2)
         set wf-test-pass-false to true
      end-if

      .


*  Test 43 - compiler logic error during compute r = -1 * p - q
*  SOLN 10-020218-6758
  Test-43.
      move zeroes to t43-f1
      move 1      to t43-f2

      compute  t43-f1 =  -1 * t43-f2 - 30
      IF t43-f1  NOT = -31
         display "test-43:fails:expected -29:actual:" t43-f1
         set wf-test-pass-false to true
      END-IF

      move zeroes to t43-f1
      move 1      to t43-f2a

      compute  t43-f1 =  -1 * t43-f2a - 30
      IF t43-f1  NOT = -31
         display "test-43a:fails:expected -31:actual:" t43-f1
         set wf-test-pass-false to true
      END-IF

      move zeroes to t43-f1
      move 1     to t43-f2b

      compute  t43-f1 =  -1 * t43-f2b - 30
      IF t43-f1  NOT = -31
         display "test-43b:fails:expected -31:actual:" t43-f1
         set wf-test-pass-false to true
      END-IF

      move zeroes to t43-f1
      move -1     to t43-f2c

      compute  t43-f1 =   ( t43-f2c *  -1 ) - 30
      IF t43-f1  NOT = -29
         display "test-43c:fails:expected -29:actual:" t43-f1
         set wf-test-pass-false to true
      END-IF

      move zeroes to t43-f1
      move 1     to t43-f2d

      compute  t43-f1 =  -1 / t43-f2d - 30
      IF t43-f1  NOT = -31
         display "test-43d:fails:expected -31:actual:" t43-f1
         set wf-test-pass-false to true
      END-IF

      move zeroes to t43-f1
      move 1     to t43-f2e

      compute  t43-f1 =  -1 * t43-f2e  - 1 ** -1
      IF t43-f1  NOT = -2
         display "test-43e:fails:expected -2:actual:" t43-f1
         set wf-test-pass-false to true
      END-IF


   .

*  Test 44
  Test-44.

      divide 100 into t44-f1

      IF t44-f1  NOT = 98.10
         display "test-44:fails:expected -31:actual:" t44-f1
         set wf-test-pass-false to true
      END-IF


   .

*
*  Solution# 10-020711-0312
*
  Test-45.

      move 3 to ws-i

      MOVE 12345.67 TO t45-f1 (ws-i)

      compute t45-f1 (ws-i) = t45-f1 (ws-i) / 1000

      IF t45-f1 (ws-i) NOT = 12.345
         display "test-45:fails:expected -31:actual:" t45-f1 (ws-i)
         set wf-test-pass-false to true
      END-IF

   .

*
* Soln# 10-020612-9682
* Compute with ON SIZE ERROR phrase took the size error branch
* even though there was no size error

 Test-46.

   compute t46-f1 = t46-f2 * t46-f3 * t46-f4 / 1000000
      on size error move 1234 to t46-f1
   end-compute

   IF t46-f1  = 1234
      display "test-46a:fails:size error not expected"
      set wf-test-pass-false to true
   END-IF

   compute t46-f1 = t46-f5 * t46-f6 * t46-f7 / 1000000
      on size error move 1234 to t46-f1
   end-compute

   IF t46-f1  = 1234
      display "test-46b:fails:size error not expected"
      set wf-test-pass-false to true
   END-IF

   compute t46-f1 = t46-f8 * t46-f9 * t46-f10 / 1000000
      on size error move 1234 to t46-f1
   end-compute

   IF t46-f1  = 1234
      display "test-46b:fails:size error not expected"
      set wf-test-pass-false to true
   END-IF

   .

*
* Soln# 10-020530-9403
* Compute gives incorrect results

 Test-47.

    compute t47-f1 = 12 + (1 / 2)

    IF t47-f1 not = 12
      display "test-47a:fails:" t47-f1 ":expected:12"
      set wf-test-pass-false to true
    END-IF

    compute t47-f1 = 12 * 1 / 2

    IF t47-f1 not = 6
      display "test-47b:fails:" t47-f1 ":expected:6"
      set wf-test-pass-false to true
    END-IF


    compute t47-f1 = (1 / 2) + 12
    IF t47-f1 not = 12
      display "test-47c:fails:" t47-f1 ":expected:12"
      set wf-test-pass-false to true
    END-IF

    compute t47-f1 = (1 / 2) +  function length (t47-f2)

    IF t47-f1 not = 12
      display "test-47d:fails:" t47-f1 ":expected:12"
      set wf-test-pass-false to true
    END-IF

    compute t47-f1 = 1 / 2 +  function length (t47-f2) ** 0
    IF t47-f1 not = 1
      display "test-47e:fails:" t47-f1 ":expected:12"
      set wf-test-pass-false to true
    END-IF


 .
