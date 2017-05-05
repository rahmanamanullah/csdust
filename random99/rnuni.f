CDECK  ID>, RNUNI.
      FUNCTION RNUNI ( A, B )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNUNI ( A, B )                          Ch.Walck    841008
C.
C.    Uniform random number in the range [A,B].
C.
C.    F(X;A,B) = 1/(B-A)
C.
C.    Variable:          X, A < X < B       (real)
C.    Parameters:        A and B, A < B     (real)
C.
C.    Expectation value: E(X) = (A+B)/2
C.    Variance:          V(X) = (B-A)**2/12
C.    Skewness:         G1(X) = 0
C.    Kurtosis:         G2(X) = -1.2
C.
C.----------------------------------------------------------------------
      RNUNI = A + RNUNIF(DUMMY) * ( B - A )
      RETURN
      END
