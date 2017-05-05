CDECK  ID>, RNEXPN.
      FUNCTION RNEXPN ( A )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNEXPN ( A )                            Ch.Walck    861010
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
C.    Generate p.r.n. from exponential distribution by the method of
C.    von Neumann (1951) as given in the article "Computer Methods for
C.    Sampling From the Exponential and Normal Distributions" by J. H.
C.    Ahrens and U.Dieter in Communications of the ACM Vol.15 (1972) 873
C.
C.----------------------------------------------------------------------
      X = 0.0
    2 U = RNUNIF(DUM)
      U0 = U
    3 USTAR = RNUNIF(DUM)
      IF ( U .LT. USTAR ) GO TO 6
      U = RNUNIF(DUM)
      IF ( U .LT. USTAR ) GO TO 3
      X = X + 1.0
      GO TO 2
    6 RNEXPN = A * ( X + U0 )
      RETURN
      END
