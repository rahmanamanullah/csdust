CDECK  ID>, RNTRIA.
      FUNCTION RNTRIA ( X, G )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNTRIA ( X, G )                         Ch.Walck    841109
C.
C.    Triangular pseudorandom number in the range [X-G,X+G] with maximum
C.    at X.
C.
C.    F(Z;X,G) = - ABS(Z-X)/G**2 + 1/G
C.
C.    Variable:          Z, X-G < Z < X+G  (real)
C.    Parameters:        X                 (real)
C.                       G > 0             (real)
C.
C.    Expectation value: E(Z) = X
C.    Variance:          V(Z) = G**2/6
C.    Skewness:         G1(Z) = 0
C.    Kurtosis:         G2(Z) = -0.6
C.
C.----------------------------------------------------------------------
C
C     Could have used RNTRIA = X + ( RNUNIF(X) - RNUNIF(G) ) * G instead
C
      RNTRIA = X + ( RNUNIF(X) + RNUNIF(G) - 1.0 ) * G
      RETURN
      END
