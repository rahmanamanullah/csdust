CDECK  ID>, IRNBNL.
      FUNCTION IRNBNL ( N, P )
C-----------------------------------------------------------------------
C
C...       FUNCTION IRNBNL ( N, P )                         Ch.Walck    841115
C.
C.    Pseudorandom number from binomial distribution
C.    (N trials, probability of success P)
C.
C.    P(R;N,P) = (N OVER R) * P**R * (1-P)**(N-R)
C.
C.    Variable:          R, 0 <= R <= N   (integer)
C.    Parameters:        N > 0            (integer)
C.                       P, 0 <= P <= 1   (real)
C.
C.    Expectation value: E(R) = N*P
C.    Variance:          V(R) = N*P*(1-P)
C.    Skewness:         G1(R) = (1-2*P)/SQRT(N*P*(1-P))
C.    Kurtosis:         G2(R) = (1-6*P*(1-P))/(N*P*(1-P))
C.
C.    Use the normal approximation
C.       IR = RNNRMT ( N*P, SQRT(N*P*(1.0-P)) ) + 0.5
C.    for big values of N.
C.
C.----------------------------------------------------------------------
      IF ( P .LE. 0.0 ) THEN
         IR = 0
      ELSE IF ( P .GE. 1.0 ) THEN
         IR = N
      ELSE
         R  = RNUNIF(P)
         IR = 0
         Q  = 1.0 - P
         PQ = P / Q
         PN = Q ** N
         S  = PN
   10    IF ( R.LE.S .OR. IR.EQ.N ) GO TO 20
         IR = IR + 1
         PN = PN * FLOAT(N-IR+1) * PQ / FLOAT(IR)
         S  = S + PN
         GO TO 10
      END IF
   20 IRNBNL = IR
      RETURN
      END
