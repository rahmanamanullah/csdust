CDECK  ID>, IRNPOI.
      FUNCTION IRNPOI ( A )
C-----------------------------------------------------------------------
C
C...       FUNCTION IRNPOI ( A )                            Ch.Walck    841008
C.                                                          Bug fixed   850604
C.                                                          Last mod    860804
C.    Random number from Poisson distribution with mean A.
C.    Note the double precison version IRDPOI in patch RDLIB
C.    using double precision arithmetics to avoid tail-effects
C.    from using random numbers with only 24-bit precision.
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
      SAVE AMAX, AREF, EXPA
C     AMAX set to 88 since EXP(-X)=0 if X>88.5 on a VAX! / CHW 860804
      DATA AREF/-1.0/, AMAX/88.0/
C--
C--   Normal approximation for large means
C--
      IF ( A .GT. AMAX ) THEN
         IRN = RNNRMT ( A, SQRT(A) ) + 0.5
C--
C--   Get uniform r.n. and loop until Poisson cumulant above
C--
      ELSE
         R  = RNUNIF(DUMMY)
         I  = 0
         P  = 0.0
         IF ( A .EQ. AREF ) GO TO 10
         AREF = A
         EXPA = EXP(-A)
   10    PI = EXPA
   20    P  = P + PI
         IF ( R.LT.P .OR. PI.EQ.0.0 ) THEN
            IRN = I
            GO TO 30
         ELSE
            I  = I + 1
            PI = PI * A / FLOAT(I)
            GO TO 20
         END IF
      END IF
   30 IRNPOI = IRN
      RETURN
      END
