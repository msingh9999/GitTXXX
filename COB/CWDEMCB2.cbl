       ID DIVISION.
       PROGRAM-ID.  CWDEMCB2.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  FILLER               PIC X(12)  VALUE 'CWDEMCB2 WS:'.
       77  PAYMAP1-LEN          PIC S9(4)  COMP    VALUE +80.
       77  PAYMAP1A-LEN         PIC S9(4)  COMP    VALUE +1053.
       77  PAYMAP2-LEN          PIC S9(4)  COMP    VALUE +800.
       77  EMP-REC-LEN          PIC S9(4)  COMP    VALUE +80.
       77  EMP-KEY-LEN          PIC S9(4)  COMP    VALUE +5.
       77  CURR-PAY             PIC  9(5)V99       VALUE ZERO.
       77  CURR-TAXES           PIC  9(5)V99       VALUE ZERO.
       77  EMP-TBL-SUB          PIC S9(3)          VALUE ZERO.
       77  LS-INITIAL-IMAGE     PIC X              VALUE '$'.
       77  LS-SUBSCRIPT         PIC S9(3)  COMP-3.
       77  DUMMY-LEN            PIC S9(4)  COMP    VALUE +8.
       77  WS-SYSID             PIC X(4).

       01  DUMMY-EMP.
           05  FILLER           PIC X(3).
           05  DUMMY-PAYEMP1    PIC X(5).

       01  WS-130               PIC S9(3) COMP-3 VALUE +130.
       01  WS-130-X REDEFINES WS-130.
           05  WS-13            PIC X.
           05  WS-0C            PIC X.

       01  PAYROLL-DATA-EMP001.
           05  PAY001-TYPE     PIC X        VALUE 'N'.
           05  PAY001-NAME     PIC X(15)    VALUE 'MR. DAVID ABEND'.
           05  PAY001-ADDRESS.
               10  PAY001-STREET   PIC X(12) VALUE '456 MAIN ST.'.
               10  PAY001-CITY     PIC X(8)  VALUE 'HOMETOWN'.
               10  PAY001-STATE    PIC XX    VALUE 'MI'.
               10  PAY001-ZIP      PIC X(5)  VALUE '48010'.
           05  PAY001-RATE         PIC  9(3)V99 VALUE  009.50.
           05  PAY001-DATE-EFF.
               10  PAY001-DTEFF-MM PIC XX    VALUE '01'.
               10  PAY001-DTEFF-DD PIC XX    VALUE '01'.
               10  PAY001-DTEFF-YY PIC XX    VALUE '84'.
           05  PAY001-LST-PCT  PIC 9(3)V9   VALUE 011.0.
           05  PAY001-TAX-RAT  PIC 9(3)V9   VALUE 020.0.
           05  PAY001-YTD-GRS  PIC S9(5)V99 VALUE +15000.00.
           05  PAY001-YTD-TAX  PIC S9(5)V99 VALUE +03000.00.
           05  PAY001-HOURS    PIC XXX      VALUE '$$$'.
           05  PAY001-MSG      PIC X(26)    VALUE SPACES.

       01  PAYROLL-DATA-EMP999.
           05  PAY999-TYPE     PIC X        VALUE 'I'.
           05  PAY999-NAME     PIC X(15)    VALUE 'MR. JOHN DOE   '.
           05  PAY999-ADDRESS.
               10  PAY999-STREET   PIC X(12) VALUE '897 TULIP   '.
               10  PAY999-CITY     PIC X(8)  VALUE 'CITYTOWN'.
               10  PAY999-STATE    PIC XX    VALUE 'MI'.
               10  PAY999-ZIP      PIC X(5)  VALUE '48011'.
           05  PAY999-RATE     PIC  9(3)V99 VALUE  850.00.
           05  PAY999-DATE-EFF.
               10  PAY999-DTEFF-MM PIC XX    VALUE '01'.
               10  PAY999-DTEFF-DD PIC XX    VALUE '01'.
               10  PAY999-DTEFF-YY PIC XX    VALUE '84'.
           05  PAY999-LST-PCT  PIC 9(3)V9   VALUE 011.0.
           05  PAY999-TAX-RAT  PIC 9(3)V9   VALUE 020.0.
           05  PAY999-YTD-GRS  PIC S9(5)V99 VALUE +15000.00.
           05  PAY999-YTD-TAX  PIC S9(5)V99 VALUE +03000.00.
           05  PAY999-HOURS    PIC 999      VALUE 040.
           05  PAY999-MSG      PIC X(26)    VALUE SPACES.

       01  WORK-AREA.
           05  WA-TYPE     PIC X.
           05  WA-NAME     PIC X(15).
           05  WA-ADDRESS.
               10  WA-STREET   PIC X(12).
               10  WA-CITY     PIC X(8).
               10  WA-STATE    PIC XX.
               10  WA-ZIP      PIC X(5).
           05  WA-RATE     PIC  9(3)V99.
           05  WA-DATE-EFF.
               10  WA-DTEFF-MM PIC XX.
               10  WA-DTEFF-DD PIC XX.
               10  WA-DTEFF-YY PIC XX.
           05  WA-LST-PCT  PIC 9(3)V9.
           05  WA-TAX-RAT  PIC 9(3)V9.
           05  WA-YTD-GRS  PIC S9(5)V99.
           05  WA-YTD-TAX  PIC S9(5)V99.
           05  WA-HOURS    PIC 999.
           05  WA-MSG      PIC X(26).

       01  VSAM-EMP-RECORD.
           05  EMP-NUM-KEY     PIC X(5).
           05  EMP-NAME        PIC X(15).
           05  EMP-HOURS       PIC 999.
           05  EMP-TOTPAY      PIC 9(5)V99.
           05  FILLER          PIC X(50).

       01  EMP-RECORD-TABLE.
         03  EMP-RECORD-TBL     OCCURS 5 TIMES.
           05  EMP-NUM-KEY-TBL  PIC X(5).
           05  EMP-NAME-TBL     PIC X(15).
           05  EMP-HOURS-TBL    PIC 999.
           05  EMP-TOTPAY-TBL   PIC 9(5)V99.

       01  EMP-RECORD-LIST.
           05  EMP-NUM-LIST     PIC X(5).
           05  EMP-HOURS-LIST   PIC 999.
           05  EMP-RATE-LIST    PIC 9(3)V99.
           05  EMP-TOTPAY-LIST  PIC 9(5)V99.

       01  PAYMAP1.
           05  MAP1-LINE1.
               10 FILLER        PIC S9(7) COMP-3 VALUE -1140401.
               10 FILLER        PIC X(1)         VALUE '0'.
               10 LINE1-TRAN    PIC X(4).
               10 FILLER        PIC S9(4) COMP   VALUE +4416.
               10 FILLER        PIC X(1)         VALUE 'E'.
               10 FILLER        PIC S9(1) COMP-3 VALUE -1.
               10 FILLER        PIC X(1)         VALUE 'J'.
               10 PAY13         PIC X.
               10 PAYEMP1       PIC X(5).
               10 FILLER        PIC S9(4) COMP   VALUE +4416.
               10 FILLER        PIC X(1)         VALUE '.'.
               10 FILLER        PIC S9(1) COMP-3 VALUE -1.
               10 FILLER        PIC X(1)         VALUE '0'.
               10 PAYPROMPT     PIC X(64) VALUE SPACE.
               10 PAYSID1       PIC X(4)  VALUE SPACE.
           05  MAP1-LINE2.
               10 FILLER        PIC X(3)  VALUE SPACE.
               10 PAYMSG2       PIC X(29) VALUE SPACE.
               10 FILLER        PIC X(48) VALUE SPACE.
           05  MAP1-LINE3.
               10 FILLER        PIC X(3)  VALUE SPACE.
               10 PAYMSG3       PIC X(29)
                  VALUE '*** COMPUWARE CORPORATION ***'.
               10 FILLER        PIC X(48) VALUE SPACE.
           05  MAP1-LINE4.
               10 FILLER        PIC X(3)  VALUE SPACE.
               10 PAYMSG4       PIC X(29)
                  VALUE '  DEMONSTRATION TRANSACTION  '.
               10 FILLER        PIC X(48) VALUE SPACE.
           05  MAP1-LINE5.
               10 FILLER        PIC X(3)  VALUE SPACE.
               10 PAYMSG5       PIC X(29) VALUE SPACE.
               10 FILLER        PIC X(48) VALUE SPACE.
           05  MAP1-LINE6.
               10 FILLER        PIC X(3)  VALUE SPACE.
               10 PAYMSG6       PIC X(29)
                  VALUE 'ENTER DESIRED EMPLOYEE ABOVE:'.
               10 FILLER        PIC X(48) VALUE SPACE.
           05  MAP1-LINE7.
               10 FILLER        PIC X(3)  VALUE SPACE.
               10 PAYMSG7       PIC X(29)
                  VALUE '00001 - CAUSES ASRA ABEND    '.
               10 FILLER        PIC X(48) VALUE SPACE.
           05  MAP1-LINE8.
               10 FILLER        PIC X(3)  VALUE SPACE.
               10 PAYMSG8       PIC X(38)
                  VALUE '00002 - CAUSES AEIM (AND OTHER ABENDS)'.
               10 FILLER        PIC X(39) VALUE SPACE.
           05  MAP1-LINE9.
               10 FILLER        PIC X(3)  VALUE SPACE.
               10 PAYMSG9       PIC X(43)
                  VALUE '00003 - CAUSES A WRITE TO TEMPORARY STORAGE'.
               10 FILLER        PIC X(34) VALUE SPACE.
           05  MAP1-LINE10.
               10 FILLER        PIC X(3)  VALUE SPACE.
               10 PAYMSG10A     PIC X(18)
                 VALUE '00004 - STARTS UP '.
               10 PAYMSG10-TRAN PIC X(4).
               10 PAYMSG10B     PIC X(24)
                 VALUE ' AS AN ASYNCHRONOUS TASK'.
               10 FILLER        PIC X(31) VALUE SPACE.
           05  MAP1-LINE11.
               10 FILLER        PIC X(3)  VALUE SPACE.
               10 PAYMSG11      PIC X(46)
                 VALUE '00005 - USED TO SHOW MULTIPLE CSECT SUPPORT'.
               10 FILLER        PIC X(31) VALUE SPACE.
           05  MAP1-LINE12.
               10 FILLER        PIC X(3)  VALUE SPACE.
               10 PAYMSG12      PIC X(43)
                  VALUE '00333 - CAUSES A STORAGE VIOLATION OF A SAA'.
               10 FILLER        PIC X(34) VALUE SPACE.
           05  MAP1-LINE13.
               10 FILLER        PIC X(3)  VALUE SPACE.
               10 PAYMSG13      PIC X(29)
                  VALUE '00999 - ENDS NORMALLY        '.
               10 FILLER        PIC X(48) VALUE SPACE.

       01  PAYMAP2.
           05  MAP2-LINE1.
               10 FILLER        PIC X(29)
                     VALUE '*** COMPUWARE CORPORATION ***'.
               10 FILLER        PIC X(47) VALUE SPACE.
               10 PAYSID2       PIC X(4)  VALUE SPACE.
           05  MAP2-LINE2.
               10 FILLER        PIC X(29)
                     VALUE '  DEMONSTRATION TRANSACTION  '.
               10 FILLER        PIC X(51) VALUE SPACE.
           05  MAP2-LINE3.
               10 FILLER        PIC X(80) VALUE SPACE.
           05  MAP2-LINE4.
               10 FILLER        PIC X(18)
                     VALUE ' EMPLOYEE NUMBER: '.
               10 EMPNUMB       PIC X(5).
               10 FILLER        PIC X(57) VALUE SPACE.
           05  MAP2-LINE5.
               10 FILLER        PIC X(18)
                     VALUE ' EMPLOYEE NAME:   '.
               10 EMPNAME       PIC X(15).
               10 FILLER        PIC X(47) VALUE SPACE.
           05  MAP2-LINE6.
               10 FILLER        PIC X(18)
                     VALUE ' HOURS WORKED:    '.
               10 HRSWRKD       PIC 999.
               10 FILLER        PIC X(59) VALUE SPACE.
           05  MAP2-LINE7.
               10 FILLER        PIC X(18)
                     VALUE ' HOURLY RATE:     '.
               10 HRLYRAT       PIC ZZZ.99.
               10 FILLER        PIC X(56) VALUE SPACE.
           05  MAP2-LINE8.
               10 FILLER        PIC X(18)
                     VALUE ' GROSS PAY:       '.
               10 GROSPAY       PIC ZZZZZZZ.99.
               10 FILLER        PIC X(52) VALUE SPACE.
           05  MAP2-LINE9.
               10 FILLER        PIC X(80) VALUE SPACES.
           05  MAP2-LINE10.
               10 PAYMSG        PIC X(28) VALUE SPACE.
               10 FILLER        PIC X(52) VALUE SPACE.

       01  EMPLOYEE-INFORMATION.                                        01830002
           05  FILLERA PIC X(67)  VALUE 'JOHN T DOE1 TELEGRAPH RDDETROIT01840002
      -        ', MI987-6789123-45-678908-10-6035000'.                  01850002
           05  FILLERB PIC X(67)  VALUE 'JOE SCHMOE2 TELEGRAPH RDDETROIT01860002
      -        ', MI777-8765313-77-778801-19-5520000'.                  01870002
           05  FILLERC PIC X(67)  VALUE 'CLARK KENT3 TELEGRAPH RDDETROIT01880002
      -        ', MI540-0400765-43-210912-13-5715000'.                  01890002
           05  FILLERD PIC X(67)  VALUE 'MARY LAMB 4 TELEGRAPH RDDETROIT01900002
      -        ', MI545-4444123-98-765407-03-5925000'.                  01910002
           05  FILLERE PIC X(67)  VALUE 'THOM THUMB5 TELEGRAPH RDDETROIT01920002
      -        ', MI555-5551366-24-362009-02-6515000'.                  01930002
       01  EMPLOYEE-INFO REDEFINES EMPLOYEE-INFORMATION.                01940002
         03  E-DETAILED-INFO OCCURS 5 TIMES.                            01950002
           05  E-NAME        PIC X(10).                                 01960002
           05  E-ADDRESS     PIC X(14).                                 01970002
           05  E-CITY        PIC X(11).                                 01980002
           05  E-PHONE       PIC X(8).                                  01990002
           05  E-SOC-SEC     PIC X(11).                                 02000002
           05  E-BIRTH-DATE  PIC X(8).                                  02010002
           05  E-SALARY      PIC 9(5).                                  02020002

       01  MESSAGES.
           05  MAP-MSG                 PIC X(80)  VALUE SPACES.
           05  STOR-VIOLATION-MSG      PIC X(47)  VALUE
           '*** CWDEMCB2 HAS CAUSED A STORAGE VIOLATION ***'.
           05  NO-STOR-VIOLATION-MSG   PIC X(55)  VALUE
           '*** XPEDITER/CICS HAS PREVENTED A STORAGE VIOLATION ***'.
           05  TEMP-STORAGE-MSG        PIC X(58)  VALUE
           '*** CWDEMCB2 HAS WRITTEN A RECORD TO TEMPORARY STORAGE ***'.
           05  ASYNCH-TASK-MSG.
               10  FILLER              PIC X(5)   VALUE '*** "'.
               10  ATM-TRAN            PIC X(4).
               10  FILLER              PIC X(46)  VALUE
           '" HAS BEEN STARTED AS AN ASYNCHRONOUS TASK ***'.

       01  TEMP-STORAGE-RECORD  PIC X(50)  VALUE
           '<THIS TEMPORARY STORAGE QUEUE BELONGS TO CWDEMCB2>'.

       01  DUMMY-COMMAREA              PIC X(80)  VALUE
           'Commarea: Text:TEST Char:Test Ascii:<(>? Hex: Test'.

       01  WS-COBOL-88-DEMO.
           05  CBL88-PARENT-A          PIC X(1)      VALUE 'A'.
               88  CBL88-A             VALUE 'A'.
               88  CBL88-AA            VALUE '5'.
               88  CBL88-AAA           VALUE 'Y'.

           05  CBL88-PARENT-B          PIC X(2)      VALUE '55'.
               88  CBL88-B             VALUE '44' '45' '55'.

           05  CBL88-PARENT-C          PIC X(2)      VALUE '55'.
               88  CBL88-C             VALUE '01' THRU '99'.

           05  CBL88-PARENT-D          PIC 9(2)      VALUE 11.
               88  CBL88-D             VALUE 22.

           05  CBL88-PARENT-D-SIGNED   PIC S9(2)     VALUE +22.
               88  CBL88-D-SIGNED      VALUE +22.

       01  CWCDWRKA             PIC X(256).

       LINKAGE SECTION.
       01  DFHCOMMAREA          PIC X(80).

       01  LS-FIELD-WITH-16-CHARS.
           05  LS-FIELD-WITH-16  PIC X(16).
           05  LS-FIELD-WITH-1   REDEFINES  LS-FIELD-WITH-16
                                 PIC X(1)  OCCURS 16 TIMES.

       PROCEDURE DIVISION.
       000-BEGIN-PROGRAM.
           EXEC CICS HANDLE AID
                     CLEAR (800-RETURN-TO-CICS)
           END-EXEC.
           EXEC CICS ASSIGN
                 SYSID(WS-SYSID)
                 NOHANDLE
           END-EXEC.

           IF EIBCALEN EQUAL ZERO
               NEXT SENTENCE
           ELSE
               GO TO 200-RECEIVE-INPUT.

       100-SEND-INITIAL-SCREEN.
           MOVE WS-13                     TO PAY13.
           MOVE '_____'                   TO PAYEMP1.
           MOVE '- ENTER EMPLOYEE NUMBER' TO PAYPROMPT.
           MOVE EIBTRNID                  TO LINE1-TRAN
                                             PAYMSG10-TRAN.
           MOVE WS-SYSID TO PAYSID1.
           EXEC CICS SEND
                     FROM   (PAYMAP1)
                     LENGTH (PAYMAP1A-LEN)
                     ERASE
           END-EXEC.
           GO TO 700-RETURN-TO-TRAN.

       200-RECEIVE-INPUT.
           EXEC CICS HANDLE CONDITION
                     LENGERR (500-MAPERR)
           END-EXEC.
           EXEC CICS RECEIVE
                     INTO   (DUMMY-EMP)
                     LENGTH (DUMMY-LEN)
           END-EXEC.
           MOVE DUMMY-PAYEMP1 TO PAYEMP1.
           IF PAYEMP1 EQUAL '00001'
                MOVE PAYROLL-DATA-EMP001 TO WORK-AREA
                GO TO 300-EMPLOYEE-PAY-RTN.
           IF PAYEMP1 EQUAL '00002'
                GO TO 900-PROCESS-00002-SELECTION.
           IF PAYEMP1 EQUAL '00003'
                GO TO 950-PROCESS-00003-SELECTION.
           IF PAYEMP1 EQUAL '00004'
                GO TO 960-PROCESS-00004-SELECTION.
           IF PAYEMP1 EQUAL '00005'
                GO TO 970-PROCESS-00005-SELECTION.
           IF PAYEMP1 EQUAL '00333'
                GO TO 1000-PROCESS-00333-SELECTION.
           IF PAYEMP1 EQUAL '00999'
                MOVE PAYROLL-DATA-EMP999 TO WORK-AREA
                GO TO 300-EMPLOYEE-PAY-RTN.
           MOVE '*** EMPLOYEE NOT ON FILE ****' TO PAYPROMPT.
           GO TO 600-SEND-PAY-MAP.

       300-EMPLOYEE-PAY-RTN.
           IF WA-TYPE EQUAL 'N' OR 'I' OR 'S'
                COMPUTE CURR-PAY   EQUAL WA-HOURS * WA-RATE
                COMPUTE CURR-TAXES EQUAL CURR-PAY * WA-TAX-RAT
                ADD CURR-PAY   TO WA-YTD-GRS
                ADD CURR-TAXES TO WA-YTD-TAX.

           IF PAYEMP1 EQUAL '00001'
                MOVE WORK-AREA TO PAYROLL-DATA-EMP001.

           IF PAYEMP1 EQUAL '00999'
                MOVE WORK-AREA TO PAYROLL-DATA-EMP999.

      ** EXAMPLES OF COBOL 88 LEVEL FIELDS.....
           IF CBL88-A
               MOVE 'Y' TO CBL88-PARENT-A.
           IF CBL88-B
               MOVE '88' TO CBL88-PARENT-B.
           IF CBL88-C
               MOVE '88' TO CBL88-PARENT-C.
           IF CBL88-D
               MOVE 88 TO CBL88-PARENT-D.
           IF CBL88-D-SIGNED
               MOVE +88 TO CBL88-PARENT-D-SIGNED.

      ** MULTIPLE COBOL 88 LEVEL FIELDS FROM SAME PARENT....
           IF CBL88-A  OR  CBL88-AA  OR  CBL88-AAA
               MOVE 'Z' TO CBL88-PARENT-A.

      ** MULTIPLE COBOL 88 LEVEL FIELDS FROM DIFFERENT PARENTS....
           IF CBL88-C  OR
              CBL88-D  OR
              CBL88-D-SIGNED
               MOVE '00' TO CBL88-PARENT-C.

       400-TRANSACTION-COMPLETE.
           MOVE PAYEMP1   TO  EMPNUMB.
           MOVE WA-NAME   TO  EMPNAME.
           MOVE WA-HOURS  TO  HRSWRKD.
           MOVE WA-RATE   TO  HRLYRAT.
           MOVE CURR-PAY  TO  GROSPAY.
           MOVE '*** TRANSACTION COMPLETE ***' TO PAYMSG.
           MOVE WS-SYSID TO PAYSID2.
           EXEC CICS SEND
                     FROM   (PAYMAP2)
                     LENGTH (PAYMAP2-LEN)
                     ERASE
           END-EXEC.
           GO TO 800-RETURN-TO-CICS.

       500-MAPERR.
           MOVE '**** MAPFAIL ON RECEIVE *****'  TO PAYPROMPT.

       600-SEND-PAY-MAP.
           MOVE WS-13    TO PAY13.
           MOVE EIBTRNID TO LINE1-TRAN
                            PAYMSG10-TRAN.
           MOVE WS-SYSID TO PAYSID1.
           EXEC CICS SEND   ERASE
                     FROM   (PAYMAP1)
                     LENGTH (PAYMAP1-LEN)
           END-EXEC.

       700-RETURN-TO-TRAN.
           EXEC CICS RETURN
                     TRANSID  (EIBTRNID)
                     COMMAREA (DUMMY-COMMAREA)
                     LENGTH   (80)
           END-EXEC.

       800-RETURN-TO-CICS.
           EXEC CICS RETURN END-EXEC.

       900-PROCESS-00002-SELECTION.
      ** READ VSAM FILE FOR RECORD.....
           MOVE PAYEMP1 TO EMP-NUM-KEY.
           EXEC CICS READ INTO (VSAM-EMP-RECORD)
                     DATASET   ('DBUGEMP')
                     RIDFLD    (EMP-NUM-KEY)
                     LENGTH    (EMP-REC-LEN)
                     KEYLENGTH (EMP-KEY-LEN)
           END-EXEC.

      ** INITIALIZE WORKING STORAGE TABLE WITH ZEROS......
           MOVE ZEROS TO EMP-RECORD-TABLE.

      ** STORE RECORD INTO WORKING STORAGE TABLE.....
           ADD +1 TO EMP-TBL-SUB.
           MOVE EMP-NUM-KEY TO EMP-NUM-KEY-TBL (EMP-TBL-SUB).
           MOVE EMP-NAME    TO EMP-NAME-TBL (EMP-TBL-SUB).
           MOVE EMP-HOURS   TO EMP-HOURS-TBL (EMP-TBL-SUB).

      ** CALCULATED TOTAL PAY - ADD TO WORKING STORAGE TABLE.....
           COMPUTE EMP-TOTPAY = EMP-HOURS * 10.
           ADD EMP-TOTPAY TO EMP-TOTPAY-TBL (EMP-TBL-SUB).

      ** READ VSAM FILE FOR UPDATE AND THEN REWRITE THE RECORD....
           EXEC CICS READ INTO (VSAM-EMP-RECORD)
                     DATASET   ('DBUGEMP')
                     RIDFLD    (EMP-NUM-KEY)
                     LENGTH    (EMP-REC-LEN)
                     KEYLENGTH (EMP-KEY-LEN)
                     UPDATE
           END-EXEC.
           MOVE EMP-TOTPAY-TBL (EMP-TBL-SUB) TO EMP-TOTPAY.
           EXEC CICS REWRITE DATASET ('DBUGEMP')
                     FROM   (VSAM-EMP-RECORD)
                     LENGTH (EMP-REC-LEN)
           END-EXEC.

      ** SEND SCREEN AND RETURN CONTROL TO CICS.....
           MOVE EMP-NUM-KEY   TO EMPNUMB.
           MOVE EMP-NAME      TO EMPNAME.
           MOVE EMP-HOURS     TO HRSWRKD.
           MOVE 10            TO HRLYRAT.
           MOVE EMP-TOTPAY    TO GROSPAY.
           MOVE '*** TRANSACTION COMPLETE ***' TO PAYMSG.
           MOVE WS-SYSID TO PAYSID2.
           EXEC CICS SEND
                     FROM   (PAYMAP2)
                     LENGTH (PAYMAP2-LEN)
                     ERASE
           END-EXEC.
           EXEC CICS RETURN END-EXEC.

       950-PROCESS-00003-SELECTION.
           EXEC CICS WRITEQ TS
                     QUEUE  ('CWCDTEMP')
                     FROM   (TEMP-STORAGE-RECORD)
                     LENGTH (50)
                END-EXEC.
           MOVE SPACES TO MAP2-LINE4  MAP2-LINE5  MAP2-LINE6
                          MAP2-LINE7  MAP2-LINE8.
           MOVE TEMP-STORAGE-MSG TO MAP2-LINE5.
           MOVE '*** TRANSACTION COMPLETE ***' TO PAYMSG.
           MOVE WS-SYSID TO PAYSID2.
           EXEC CICS SEND
                     FROM   (PAYMAP2)
                     LENGTH (PAYMAP2-LEN)
                     ERASE
           END-EXEC.
           EXEC CICS RETURN END-EXEC.

       960-PROCESS-00004-SELECTION.
           EXEC CICS START TRANSID  (EIBTRNID)
                           INTERVAL (0)
                END-EXEC.
           MOVE SPACES TO MAP2-LINE4  MAP2-LINE5  MAP2-LINE6
                          MAP2-LINE7  MAP2-LINE8.
           MOVE EIBTRNID        TO ATM-TRAN.
           MOVE ASYNCH-TASK-MSG TO MAP2-LINE5.
           MOVE '*** TRANSACTION COMPLETE ***' TO PAYMSG.
           MOVE WS-SYSID TO PAYSID2.
           EXEC CICS SEND
                     FROM   (PAYMAP2)
                     LENGTH (PAYMAP2-LEN)
                     ERASE
           END-EXEC.
           EXEC CICS RETURN END-EXEC.

       970-PROCESS-00005-SELECTION.
      ** STORE DATA INTO PARAMETER LIST AREA.....
           MOVE '00050' TO EMP-NUM-LIST.
           MOVE 040     TO EMP-HOURS-LIST.

      ** CALL THE ASSEMBLER SUBROUTINE TO CALCULATE TOTAL PAY.....
           CALL 'CWCDSUBA' USING EMP-RECORD-LIST CWCDWRKA.

      ** SEND SCREEN AND RETURN CONTROL TO CICS.....
           MOVE EMP-NUM-LIST    TO EMPNUMB.
           MOVE 'JOHN SMITH'    TO EMPNAME.
           MOVE EMP-HOURS-LIST  TO HRSWRKD.
           MOVE EMP-RATE-LIST   TO HRLYRAT.
           MOVE EMP-TOTPAY-LIST TO GROSPAY.
           MOVE '*** TRANSACTION COMPLETE ***' TO PAYMSG.
           MOVE WS-SYSID TO PAYSID2.
           EXEC CICS SEND
                     FROM   (PAYMAP2)
                     LENGTH (PAYMAP2-LEN)
                     ERASE
           END-EXEC.
           EXEC CICS RETURN END-EXEC.

       1000-PROCESS-00333-SELECTION.
      *****************************************************************
      ** THIS PROGRAM HAS A FIELD CALLED LS-FIELD-WITH-16-CHARS DEFINED
      ** IN THE LINKAGE SECTION.  THE FOLLOWING ROUTINE WILL DO A CICS
      ** GETMAIN FOR A LENGTH OF 16 AND THEN INITIALIZE THE FIELD WITH
      ** ALL V'S.  THE PROBLEM IS THE COBOL PROGRAMMER HAS CODED THE
      ** INITIALIZATION LOOP ROUTINE INCORRECTLY AND OVER-RUNS THE 16
      ** CHARACTER FIELD BY 1 BYTE CAUSING A STORAGE VIOLATION.
      *****************************************************************
       1020-GETMAIN-STORAGE.
           EXEC CICS GETMAIN
                     SET     (ADDRESS OF LS-FIELD-WITH-16-CHARS)
                     LENGTH  (16)
                     INITIMG (LS-INITIAL-IMAGE)
           END-EXEC.

       1040-INITIALIZE-STORAGE-WITH-V.
           MOVE +1 TO LS-SUBSCRIPT.
       1060-INITIALIZE-STORAGE-LOOP.
           MOVE 'V' TO LS-FIELD-WITH-1 (LS-SUBSCRIPT).
           IF LS-SUBSCRIPT > +16
               GO TO 1080-INITIALIZATION-DONE.
           ADD +1 TO LS-SUBSCRIPT.
           GO TO 1060-INITIALIZE-STORAGE-LOOP.

       1080-INITIALIZATION-DONE.
           IF LS-SUBSCRIPT > +16 AND
              LS-FIELD-WITH-1 (LS-SUBSCRIPT) = 'V'
               MOVE STOR-VIOLATION-MSG    TO MAP2-LINE5
           ELSE
               MOVE NO-STOR-VIOLATION-MSG TO MAP2-LINE5.

       1100-SEND-MAP.
           MOVE SPACES TO MAP2-LINE4  MAP2-LINE6
                          MAP2-LINE7  MAP2-LINE8.
           MOVE '*** TRANSACTION COMPLETE ***' TO PAYMSG.
           MOVE WS-SYSID TO PAYSID2.
           EXEC CICS SEND
                     FROM   (PAYMAP2)
                     LENGTH (PAYMAP2-LEN)
                     ERASE
           END-EXEC.
           EXEC CICS RETURN END-EXEC.
           GOBACK.