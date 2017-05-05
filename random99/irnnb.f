CDECK  ID>, IRNNB.
      FUNCTION IRNNB ( AV, MREAL )
C-----------------------------------------------------------------------
C
C...       FUNCTION IRNNB ( AV, MREAL )                     Ch.Walck    860424
C.
C.    Pseudorandom number from negative binomial distribution.
C.
C.    In this version, as opposed to function IRNNBL, M may be a real
C.    number! Also this routine describes the number of failures
C.    occuring while waiting for M successes rather than, as IRNNBL, the
C.    number of trials one has to wait until M successes has occurred.
C.
C.    P(R;M,P) = (R+M-1)! P**M (1-P)**R / (M-1)! R! with P = 1/(1+AV/M)
C.
C.    Variable:          R >= 0      (integer)
C.    Parameters:        M >  0      (real)
C.                      AV >  0      (real)
C.
C.    Expectation value: E(R) = M*(1-P)/P
C.    Variance:          V(R) = M*(1-P)/P**2
C.    Skewness:         G1(R) = (2-P)/SQRT(M*(1-P))
C.    Kurtosis:         G2(R) = (P**2-6*P+6)/(M*(1-P))
C.
C.----------------------------------------------------------------------
      REAL MREAL
      R  = RNUNIF(AV)
      IR = 0
      P  = 1.0 / ( 1.0 + AV/MREAL )
      Q  = 1.0 - P
      PN = P**MREAL
      S  = PN
      SOLD = -1.0
   10 IF ( R.LE.S .OR. S.EQ.SOLD ) GO TO 20
      PN = PN * Q * (MREAL+FLOAT(IR)) / FLOAT(IR+1)
      IR = IR + 1
      SOLD = S
      S  = S + PN
      GO TO 10
   20 IRNNB = IR
      RETURN
      END
