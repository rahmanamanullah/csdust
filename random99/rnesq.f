CDECK  ID>, RNESQ.
      FUNCTION RNESQ ( AV )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNESQ ( AV )                            Ch.Walck    841008
C.
C.    Random number from DN/D(X**2) = EXP(-A*X) with mean AV
C.    (=> slope A = 2/AV ) i.e. a gamma distribution with B=2.
C.
C.              F(X;A) = A**2 * X * EXP(-A*X) =
C.
C.                     = 4 * X * EXP(-2*X/AV) / AV**2
C.
C.    Variable:          X > 0     (real)
C.    Parameter:         AV > 0    (real)
C.
C.    Expectation value: E(X) = 2/A     = AV
C.    Variance:          V(X) = 2/A**2  = AV**2/2
C.    Skewness:         G1(X) = SQRT(2) = SQRT(2)
C.    Kurtosis:         G2(X) = 3       = 3
C.
C.    This distribution is used e.g. for transverse momentum
C.    distributions.
C.
C.----------------------------------------------------------------------
      RNESQ = - AV * LOG(RNUNIF(AV)*RNUNIF(DUM)) / 2.0
      RETURN
      END
