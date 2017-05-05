CDECK  ID>, IRNBIN.
      FUNCTION IRNBIN ( N, P )
C-----------------------------------------------------------------------
C
C...       FUNCTION IRNBIN ( N, P )                         Ch.Walck    841008
C.
C.    Random number from binomial distribution
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
C.       IR = RNNRMT ( N*P, SQRT(N*P*(1.-P)) ) + 0.5
C.    for big values of N.
C.
C.----------------------------------------------------------------------
      IR = 0
      DO 10 I = 1, N
         IF ( RNUNIF(I) .LE. P ) IR = IR + 1
   10 CONTINUE
      IRNBIN = IR
      RETURN
      END
