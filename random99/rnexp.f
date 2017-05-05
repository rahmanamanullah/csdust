CDECK  ID>, RNEXP.
      FUNCTION RNEXP ( A )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNEXP ( A )                             Ch.Walck    841008
C.
C.    Random number from exponential distribution with mean A.
C.
C.    F(X;A) = EXP(-X/A) / A
C.
C.    Variable:          X > 0    (real)
C.    Parameter:         A > 0    (real)
C.
C.    Expectation value: E(X) = A
C.    Variance:          V(X) = A**2
C.    Skewness:         G1(X) = 2
C.    Kurtosis:         G2(X) = 6
C.
C.----------------------------------------------------------------------
      RNEXP = - A * LOG(RNUNIF(A))
      RETURN
      END
