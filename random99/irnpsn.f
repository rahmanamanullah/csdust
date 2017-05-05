CDECK  ID>, IRNPSN.
      FUNCTION IRNPSN ( A )
C-----------------------------------------------------------------------
C
C...       FUNCTION IRNPSN ( A )                            Ch.Walck    841008
C.
C.    Random number from Poisson distribution with mean A.
C.
C.    P(R;A) = A**R * EXP(-A) / R!
C.
C.    Variable:          R >= 0 (integer)
C.    Parameter:         A > 0  (real)
C.
C.    Expectation value: E(R) = A
C.    Variance:          V(R) = A
C.    Skewness:         G1(R) = 1/SQRT(A)
C.    Kurtosis:         G2(R) = 1/A
C.
C.----------------------------------------------------------------------
      SAVE AMAX, AREF, PI
C     AMAX set to 88 since EXP(-X)=0 if X>88.5 on a VAX! / CHW 860804
      DATA AREF/-1.0/, AMAX/88.0/
C--
C--   Normal approximation for large means
C--
      IF ( A .GT. AMAX ) THEN
         IRN = RNNRMT ( A, SQRT(A) ) + 0.5
C--
C--   Multiply uniform r.n. until product below exp(-mu)
C--
      ELSE
         IF ( A .EQ. AREF ) GO TO 10
         AREF = A
         PI   = EXP(-A)
   10    IRN  = -1
         P    = 1.0
   20    IRN  = IRN + 1
         P    = P * RNUNIF(IR)
         IF ( P .GT. PI ) GO TO 20
      END IF
      IRNPSN = IRN
      RETURN
      END
