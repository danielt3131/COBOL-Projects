       IDENTIFICATION DIVISION.
       PROGRAM-ID. PANNUITY.
       AUTHOR. DANIEL THOMPSON.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 TOTAL-LOAN       PIC 9(9)V99.
       01 TOTAL-LOAN-D  PIC $$$,$$$,$$9.99.
       01 INTEREST-RATE  PIC 999V9999 VALUE ZERO.
       01 INTEREST-RATEP PIC ZZ9.99 VALUE ZERO.
       01 MONTHS           PIC 99999.
       01 MONTHS-D         PIC ZZZ99.
       01 TEMP             PIC X(12).
       01 YEARS            PIC 999.
       01 YEARS-D          PIC ZZ9.
       01 MONTHLY-PAYMENT  PIC $$$,$$$,$$9.99.
       01 STEPCOMP        PIC 9       VALUE ZERO.
       PROCEDURE DIVISION.
           DISPLAY "HELLO I'M ZOS LET ME TO HELP YOU".
      *    Get the number of years from the user
           PERFORM UNTIL STEPCOMP = 1
           DISPLAY "PLEASE ENTER THE NUMBER OF YEARS"
           ACCEPT TEMP
           IF TEMP IS NOT ALPHABETIC THEN
              COMPUTE YEARS = FUNCTION NUMVAL(TEMP)
                  IF YEARS > 0 AND YEARS < 999 THEN
                         MOVE 1 TO STEPCOMP
                  ELSE
                         DISPLAY "INVALID VALUE TRY AGAIN"
                  END-IF
           ELSE
                  DISPLAY "NOT NUMERIC TRY AGAIN"
           END-IF
           END-PERFORM
           MOVE ZERO TO STEPCOMP

      *    Get the interest rate of the loan
           PERFORM UNTIL STEPCOMP = 1
           DISPLAY "PLEASE ENTER THE INTEREST RATE IN DECIMAL"
           ACCEPT TEMP
           IF TEMP IS NOT ALPHABETIC
                  COMPUTE INTEREST-RATE = FUNCTION NUMVAL(TEMP)
                  IF INTEREST-RATE > 0 AND INTEREST-RATE < 999.99 THEN
                         MOVE 1 TO STEPCOMP
                  ELSE
                         DISPLAY "INVALID VALUE TRY AGAIN"
                  END-IF
           ELSE
                  DISPLAY "NOT NUMERIC TRY AGAIN"
           END-IF
           END-PERFORM
           MOVE ZERO TO STEPCOMP

      *    Get the loan amount amount
           PERFORM UNTIL STEPCOMP = 1
           DISPLAY "PLEASE ENTER THE LOAN AMOUNT"
           ACCEPT TEMP
           IF TEMP IS NOT ALPHABETIC THEN
                  COMPUTE TOTAL-LOAN = FUNCTION NUMVAL(TEMP)
                  IF TOTAL-LOAN > 0 AND TOTAL-LOAN < 999999999.99 THEN
                         MOVE 1 TO STEPCOMP
                  ELSE
                         DISPLAY "INVALID VALUE TRY AGAIN"
                  END-IF
           ELSE
                  DISPLAY "NOT NUMERIC TRY AGAIN"
           END-IF
           END-PERFORM
           MOVE ZERO TO STEPCOMP

      *    Compute Annuity
           COMPUTE INTEREST-RATEP = INTEREST-RATE * 100
           COMPUTE TOTAL-LOAN-D = TOTAL-LOAN
           DISPLAY 'TOTAL-LOAN: ' TOTAL-LOAN-D ' USD - INTEREST-RATE: '
      -    INTEREST-RATEP '%'.
           DISPLAY ' Y   M        AMOUNT '
           DISPLAY '--- --- -------------'
           PERFORM YEARS TIMES
             ADD 12 TO MONTHS
             COMPUTE MONTHLY-PAYMENT = TOTAL-LOAN * FUNCTION ANNUITY
      -      ((INTEREST-RATE / 12), MONTHS)
             COMPUTE YEARS = MONTHS / 12
             COMPUTE YEARS-D = YEARS  *> year formatted
             COMPUTE MONTHS-D = MONTHS *> months formatted
             DISPLAY YEARS-D ' ' MONTHS-D ' ' MONTHLY-PAYMENT ' USD'
           END-PERFORM
           GOBACK.
