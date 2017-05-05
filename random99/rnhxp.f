CDECK  ID>, RNHXP.
      FUNCTION RNHXP ( N, P, SL )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNHXP ( N, P, SL )                      Ch.Walck    860806
C.
C.    Pseudorandom number from hyperexponential distribution
C.    (with N exponential processes in parallel).
C.
C.    F(X;N,P,SL) = SUM P(I)*SL(I)*EXP(-SL(I)*X)
C.
C.    Where the sum goes from 1 to N and where sum P(I) = 1.
C.    Use this routine for N>2 (for N=1 use RNEXP and for N=2
C.    use RNHEXP)!
C.
C.    Variable:          X > 0            (real)
C.    Parameters:        SL(I) > 0        (real)
C.                       0 <= P(I) <= 1   (real)
C.
C.    Expectation value: E(X) = SUM P(I)/SL(I)
C.    Algebraic moments: MU'(K) = K! ( SUM P(I)/SL(I)**K )
C.
C.----------------------------------------------------------------------
      DIMENSION P(N), SL(N)
      R = RNUNIF(N)
      PSUM = 0.0
      DO 1 I = 1, N
      PSUM = PSUM + P(I)
         IF ( R .LE. PSUM ) THEN
            K = I
            GO TO 2
         END IF
    1 CONTINUE
      K = N
    2 RNHXP = - LOG(RNUNIF(K)) / SL(K)
      RETURN
      END
