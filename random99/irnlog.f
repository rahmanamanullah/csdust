CDECK  ID>, IRNLOG.
      FUNCTION IRNLOG ( Q )
C-----------------------------------------------------------------------
C
C...       FUNCTION IRNLOG ( Q )                            Ch.Walck    860422
C.
C.    Random number from logarithmic distribution.
C.
C.    P(R;Q) = - ALPHA * Q**R/R with ALPHA = 1/LN(1-Q)
C.
C.    Variable:          R, 1 <= R        (integer)
C.    Parameters:        Q, 0 <= Q < 1    (real)
C.
C.    Expectation value: E(R) = -ALPHA*Q/P                  with P = 1-Q
C.    Variance:          V(R) = -ALPHA*Q*(1+ALPHA*Q)/P**2
C.
C.----------------------------------------------------------------------
      SAVE NCOUNT
      DATA NCOUNT / 0 /
      IF ( Q .LE. 0.0 ) THEN
         IRN = 1
      ELSE IF ( Q .GE. 1.0 ) THEN
         NCOUNT = NCOUNT + 1
         IF ( NCOUNT .LE. 25 ) WRITE (6,1000)
 1000    FORMAT (' IRNLOG called with illegal argument')
         IRN = 1
      ELSE
         ALPHA = 1.0 / LOG(1.0-Q)
         IRN  = 1
         R    = RNUNIF(Q)
         PN   = - ALPHA * Q
         S    = PN
         SOLD = -1.0
   10    IF ( R.LE.S .OR. SOLD.EQ.S ) GO TO 20
         IRN  = IRN + 1
         PN   = PN * Q
         SOLD = S
         S    = S + PN / FLOAT(IRN)
         GO TO 10
      END IF
   20 IRNLOG = IRN
      RETURN
      END
