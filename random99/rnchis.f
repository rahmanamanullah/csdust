CDECK  ID>, RNCHIS.
      FUNCTION RNCHIS ( NDF )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNCHIS ( NDF )                          Ch.Walck    841102
C.
C.    Pseudorandom number from chisquared distribution with NDF degrees
C.    of freedom.
C.
C.    F(X;NDF) = (X/2)**(NDF/2-1) * EXP(-X/2) / GAMMA(NDF/2) / 2
C.
C.    Variable:          X >= 0  (real)
C.    Parameter:         NDF > 0 (integer)
C.
C.    Expectation value: E(X) = NDF
C.    Variance:          V(X) = 2*NDF
C.    Skewness:         G1(X) = 2*SQRT(2/NDF)
C.    Kurtosis:         G2(X) = 12/NDF
C.
C.    For NDF greater than 100 a normal approximation is used.
C.
C.----------------------------------------------------------------------
      IF ( NDF .LE. 0 ) THEN
         R = 0.0
      ELSE IF ( NDF .LE. 100 ) THEN
         R = 0.0
C        WARNING: This loop could cause compiler optimization problems
         DO 10 I = 1, NDF
            R = R + RNNRMT(0.0,1.0)**2
   10    CONTINUE
      ELSE
         R = ( RNNRMT(0.0,1.0) + SQRT(FLOAT(2*NDF-1)) ) ** 2 / 2.0
      END IF
      RNCHIS = R
      RETURN
      END
