       IDENTIFICATION DIVISION.
       PROGRAM-ID. MORTGAGE.
       AUTHOR. DANIEL THOMPSON.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-DATA ASSIGN TO INDATA
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL.
           SELECT REPORT-FILE ASSIGN TO REOUPUT
           ACCESS MODE IS SEQUENTIAL.
       DATA DIVISION.
       LINKAGE SECTION.
       01 PARM-GRP.
           05 PARM-LENGTH PIC S9(04) COMP.
           05 PARM-DATA     PIC X(4).

       FILE SECTION.
       FD INPUT-DATA RECORDING MODE F.
       01 INPUT-RECORD.
           05 INPUT-LOAN-NAME PIC X(40).
           05 UNUSED        PIC X(1).
           05 LOAN-AMOUNT PIC 9(9)V99.
           05 UNUSED        PIC X(1).
           05 ANNUAL-INTEREST PIC 999V9999.
           05 UNUSED        PIC X(1).
           05 LOAN-TERM PIC 9(3).
           05 UNUSED        PIC X(1).
           05 ADDITONAL-PAYMENTS PIC 9(9)V99.
           05 UNUSED       PIC X(4).
       FD REPORT-FILE RECORDING MODE F
          RECORD CONTAINS 176 CHARACTERS.
       01 REPORT-RECORD.
           05 RECORD-OUTPUT PIC X(176).

       WORKING-STORAGE SECTION.
       01 TOTAL-LOAN       PIC 9(9)V999.
       01 TOTAL-LOAN-COPY  PIC 9(9)V999.
       01 CURRENT-PRINCIPAL PIC 9(9)V999.
       01 ACCELERATED-PAYMENT PIC 9(9)V99 VALUE ZERO.
       01 ACCELERATED-PAYMENT-D PIC $$$,$$$,$$9.99.
       01 TOTAL-LOAN-D  PIC $$$,$$$,$$9.99.
       01 INTEREST-RATE  PIC 999V9999 VALUE ZERO.
       01 INTEREST-RATEP PIC ZZ9.99 VALUE ZERO.
       01 OUTPUT-TEXT     PIC X(176).
       01 PAYMENT-TOTAL PIC 9(9)V99.
       01 PAYMENT-TOTAL-D PIC $$$,$$$,$$9.99.
       01 TOTAL-INTEREST-SAVED PIC 9(9)V99 VALUE ZERO.
       01 MONTHS           PIC 99999.
       01 MONTHS-D         PIC ZZZ99.
       01 TEMP             PIC X(12).
       01 LOAN-NAME       PIC X(40).
       01 YEARS            PIC 999.
       01 YEARS-D          PIC ZZ9.
      *    From IBM Z COBOL Documentation
       01  WS-CURRENT-DATE-FIELDS.
           05  WS-CURRENT-DATE.
                     10  WS-CURRENT-YEAR    PIC  9(4).
                     10  WS-CURRENT-MONTH   PIC  9(2).
                     10  WS-CURRENT-DAY     PIC  9(2).
           05  WS-CURRENT-TIME.
                     10  WS-CURRENT-HOUR    PIC  9(2).
                     10  WS-CURRENT-MINUTE  PIC  9(2).
                     10  WS-CURRENT-SECOND  PIC  9(2).
                     10  WS-CURRENT-MS      PIC  9(2).
           05  WS-DIFF-FROM-GMT       PIC S9(4).
       01 MONTHLY-PAYMENT-D  PIC $$$,$$$,$$9.99.
       01 EXTRA-PAYMENT-TOTAL PIC 9(9)V99.
       01 INTEREST-PAYMENT PIC 9(9)V999.
       01 INTEREST-PAYMENT-D  PIC $$$,$$$,$$9.99.
       01 CURRENT-PRINCIPAL-D  PIC $$$,$$$,$$9.99.
       01 TOTAL-INTEREST PIC 9(9)V99 VALUE ZERO.
       01 TOTAL-INTEREST-D  PIC $$$,$$$,$$9.99.
       01 BASE-TOTAL-INTEREST PIC 9(9)V99 VALUE ZERO.
       01 MONTHLY-PAYMENT PIC 9(9)V99.
       01 MONTHY-RATE     PIC 99V9999999 VALUE ZERO.
       01 STEPCOMP        PIC 9       VALUE ZERO.
       01 ROI             PIC 99V999 VALUE ZERO.
       01 ROI-D           PIC Z9.999 VALUE ZERO.
       01 I               PIC 9(9) VALUE 1.
       01 OUTPUT-MODE     PIC X(1).
       PROCEDURE DIVISION USING PARM-GRP.
           DISPLAY PARM-DATA.
       OPEN-FILE.
              OPEN INPUT INPUT-DATA.
              OPEN OUTPUT REPORT-FILE.
       OPEN-FILE-END.
      * Read record
           READ INPUT-DATA.
           MOVE LOAN-AMOUNT TO TOTAL-LOAN.
           MOVE INPUT-LOAN-NAME TO LOAN-NAME.
           MOVE ANNUAL-INTEREST TO INTEREST-RATE.
           MOVE ADDITONAL-PAYMENTS TO ACCELERATED-PAYMENT.
           MOVE LOAN-TERM TO YEARS.
           MOVE TOTAL-LOAN TO TOTAL-LOAN-COPY
           DISPLAY TOTAL-LOAN.
           CLOSE INPUT-DATA.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FIELDS.
           IF PARM-DATA = 'HTML'
                MOVE 1 TO OUTPUT-MODE
           END-IF
           IF PARM-DATA = 'TEXT'
                MOVE 0 TO OUTPUT-MODE
           END-IF

           DISPLAY OUTPUT-MODE.
      *    Calculate payments
           DISPLAY TOTAL-LOAN.
           DISPLAY INTEREST-RATE.
           DISPLAY ACCELERATED-PAYMENT.
           DISPLAY LOAN-NAME.

           COMPUTE MONTHS = YEARS * 12.
           COMPUTE MONTHY-RATE = INTEREST-RATE / 12.
           COMPUTE MONTHLY-PAYMENT = TOTAL-LOAN * (FUNCTION ANNUITY
      -    (MONTHY-RATE, MONTHS)).

      *    Write HTML header for HTML output mode
           IF OUTPUT-MODE = 1
           MOVE "<html><head><title>Mortgage</title></head><body>"
      -    TO RECORD-OUTPUT
           WRITE REPORT-RECORD
           MOVE SPACES TO OUTPUT-TEXT
           STRING "<p>"LOAN-NAME"</p>" DELIMITED BY SIZE
           INTO OUTPUT-TEXT
           END-STRING
           MOVE SPACES TO RECORD-OUTPUT
           MOVE OUTPUT-TEXT TO RECORD-OUTPUT
           WRITE REPORT-RECORD
           MOVE "<table>" TO RECORD-OUTPUT
           WRITE  REPORT-RECORD
           MOVE SPACES TO OUTPUT-TEXT
           STRING "<tr>" DELIMITED BY SIZE
           "<th>Date</th>" DELIMITED BY SIZE
           "<th>Principal</th>" DELIMITED BY SIZE
           "<th>Additonal Payment</th>" DELIMITED BY SIZE
           "<th>Interest</th>" DELIMITED BY SIZE
           "<th>Payment Total</th>" DELIMITED BY SIZE
           "<th>Balance</th>" DELIMITED BY SIZE
           "<th>Total Interest</th>" DELIMITED BY SIZE
           "</tr>" DELIMITED BY SIZE
           INTO OUTPUT-TEXT
           END-STRING
           MOVE SPACES TO RECORD-OUTPUT
           MOVE OUTPUT-TEXT TO RECORD-OUTPUT
           WRITE REPORT-RECORD
           END-IF
      *    Calculate payments
           IF OUTPUT-MODE = 0
           MOVE LOAN-NAME TO RECORD-OUTPUT
           WRITE REPORT-RECORD
           MOVE SPACES TO OUTPUT-TEXT
           STRING "Date          " DELIMITED BY SIZE
           SPACE "Principal    " DELIMITED BY SIZE
           SPACE "Additonal Payment" DELIMITED BY SIZE
           SPACE "Interest    " DELIMITED BY SIZE
           SPACE "Payment Total" DELIMITED BY SIZE
           SPACE "Balance    " DELIMITED BY SIZE
           SPACE "Total Interest" DELIMITED BY SIZE
           INTO REPORT-RECORD
           END-STRING
           WRITE REPORT-RECORD
           END-IF
           MOVE ACCELERATED-PAYMENT TO ACCELERATED-PAYMENT-D.
           PERFORM UNTIL TOTAL-LOAN = 0
           COMPUTE INTEREST-PAYMENT ROUNDED = MONTHY-RATE * TOTAL-LOAN
           COMPUTE CURRENT-PRINCIPAL ROUNDED = ACCELERATED-PAYMENT +
      -         (MONTHLY-PAYMENT - INTEREST-PAYMENT)
                IF CURRENT-PRINCIPAL > TOTAL-LOAN
                MOVE TOTAL-LOAN TO CURRENT-PRINCIPAL
                END-IF
                COMPUTE TOTAL-LOAN = TOTAL-LOAN - CURRENT-PRINCIPAL
                ADD ACCELERATED-PAYMENT TO EXTRA-PAYMENT-TOTAL
                MOVE TOTAL-LOAN TO TOTAL-LOAN-D
                MOVE INTEREST-PAYMENT TO INTEREST-PAYMENT-D
                MOVE TOTAL-INTEREST TO TOTAL-INTEREST-D
                IF ACCELERATED-PAYMENT > 0
                SUBTRACT ACCELERATED-PAYMENT FROM CURRENT-PRINCIPAL
                END-IF
                MOVE CURRENT-PRINCIPAL TO CURRENT-PRINCIPAL-D

           COMPUTE PAYMENT-TOTAL = CURRENT-PRINCIPAL + INTEREST-PAYMENT
           ADD ACCELERATED-PAYMENT TO PAYMENT-TOTAL
           MOVE PAYMENT-TOTAL TO PAYMENT-TOTAL-D

                MOVE SPACES TO OUTPUT-TEXT
      *         Text output
                IF OUTPUT-MODE = 0
                STRING WS-CURRENT-MONTH "/" WS-CURRENT-DAY "/"
                WS-CURRENT-YEAR
                SPACE CURRENT-PRINCIPAL-D SPACE
                ACCELERATED-PAYMENT-D SPACE
                INTEREST-PAYMENT-D SPACE
                PAYMENT-TOTAL-D SPACE
                TOTAL-LOAN-D SPACE
                TOTAL-INTEREST-D
                DELIMITED BY SIZE
                INTO OUTPUT-TEXT
                END-STRING
                MOVE SPACES TO RECORD-OUTPUT
                MOVE OUTPUT-TEXT TO RECORD-OUTPUT
                WRITE REPORT-RECORD
                END-IF

      *         HTML output
                IF OUTPUT-MODE = 1
                STRING "<tr>" DELIMITED BY SIZE
                "<td>" WS-CURRENT-MONTH "/" WS-CURRENT-DAY "/"
                WS-CURRENT-YEAR "</td>" DELIMITED BY SIZE
                "<td>"CURRENT-PRINCIPAL-D"</td>" DELIMITED BY SIZE
                "<td>"ACCELERATED-PAYMENT-D"</td>" DELIMITED BY SIZE
                "<td>"INTEREST-PAYMENT-D"</td>" DELIMITED BY SIZE
                "<td>"PAYMENT-TOTAL-D"</td>" DELIMITED BY SIZE
                "<td>"TOTAL-LOAN-D"</td>" DELIMITED BY SIZE
                "<td>"TOTAL-INTEREST-D"</td>" DELIMITED BY SIZE
                "</tr>" DELIMITED BY SIZE
                INTO OUTPUT-TEXT
                END-STRING
                MOVE SPACES TO RECORD-OUTPUT
                MOVE OUTPUT-TEXT TO RECORD-OUTPUT
                WRITE REPORT-RECORD
                END-IF


                ADD 1 TO WS-CURRENT-MONTH
      *         Increment Date
                IF WS-CURRENT-MONTH = 13
                   MOVE 1 TO WS-CURRENT-MONTH
                   ADD 1 TO WS-CURRENT-YEAR
                END-IF
                DISPLAY INTEREST-PAYMENT-D ' ' CURRENT-PRINCIPAL-D ' '
      -         TOTAL-LOAN-D ' ' I
                ADD 1 TO I
                ADD INTEREST-PAYMENT TO TOTAL-INTEREST
                IF TOTAL-LOAN = 0
                     EXIT PERFORM
                END-IF
           END-PERFORM.

           IF ACCELERATED-PAYMENT > 0
      *    Calculate ROI

           MOVE TOTAL-LOAN-COPY TO TOTAL-LOAN

           MOVE 1 TO I
           PERFORM UNTIL TOTAL-LOAN = 0
           COMPUTE INTEREST-PAYMENT ROUNDED = MONTHY-RATE * TOTAL-LOAN
           COMPUTE CURRENT-PRINCIPAL ROUNDED =
      -         (MONTHLY-PAYMENT - INTEREST-PAYMENT)
                IF CURRENT-PRINCIPAL > TOTAL-LOAN
                MOVE TOTAL-LOAN TO CURRENT-PRINCIPAL
                END-IF
                COMPUTE TOTAL-LOAN = TOTAL-LOAN - CURRENT-PRINCIPAL
                MOVE TOTAL-LOAN TO TOTAL-LOAN-D
                MOVE INTEREST-PAYMENT TO INTEREST-PAYMENT-D
                MOVE CURRENT-PRINCIPAL TO CURRENT-PRINCIPAL-D
                ADD 1 TO I
                ADD INTEREST-PAYMENT TO BASE-TOTAL-INTEREST
                IF TOTAL-LOAN = 0
                     EXIT PERFORM
                END-IF
           END-PERFORM

           COMPUTE TOTAL-INTEREST-SAVED =
      -     (BASE-TOTAL-INTEREST - TOTAL-INTEREST)
           DISPLAY EXTRA-PAYMENT-TOTAL
           COMPUTE ROI = TOTAL-INTEREST-SAVED / EXTRA-PAYMENT-TOTAL
           DISPLAY ROI
           MOVE ROI TO ROI-D
           DISPLAY TOTAL-INTEREST
           IF OUTPUT-MODE = 0

           WRITE REPORT-RECORD
           MOVE SPACES TO OUTPUT-TEXT
           STRING "ROI: " ROI-D
           DELIMITED BY SIZE INTO OUTPUT-TEXT
           END-STRING
           MOVE SPACES TO RECORD-OUTPUT
           MOVE OUTPUT-TEXT TO RECORD-OUTPUT
           WRITE REPORT-RECORD
           END-IF

           IF OUTPUT-MODE = 1
                MOVE SPACES TO OUTPUT-TEXT
                STRING "<p>ROI: " ROI-D "</p>" DELIMITED BY SIZE
                INTO OUTPUT-TEXT
                END-STRING
                MOVE SPACES TO RECORD-OUTPUT
                MOVE OUTPUT-TEXT TO RECORD-OUTPUT
                WRITE REPORT-RECORD
           END-IF
           END-IF
           IF OUTPUT-MODE = 1
            MOVE "</table></body></html>" TO RECORD-OUTPUT
           DISPLAY "Written footer"
           WRITE REPORT-RECORD
           CLOSE REPORT-FILE
           STOP RUN.
