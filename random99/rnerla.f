CDECK  ID>, RNERLA.
      FUNCTION RNERLA ( AV, N )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNERLA ( AV, N )                        Ch.Walck    841121
C.
C.    Random number from Erlangian distribution with mean AV
C.    i.e. a gamma distribution where A=N/AV and B=N (integer).
C.
C.    F(X;A,B) = A * (AX)**(B-1) * EXP(-A*X) / GAMMA(B)
C.             = N * (N*X/AV)**(N-1) * EXP(-N*X/AV) / (N-1)! / AV
C.
C.    Variable:          X >= 0    (real)
C.    Parameters:        AV > 0    (real)
C.                       B = N > 0 (integer)
C.
C.    Expectation value: E(X) = B/A       = AV
C.    Variance:          V(X) = B/A**2    = AV**2/N
C.    Skewness:         G1(X) = 2/SQRT(B) = 2/SQRT(N)
C.    Kurtosis:         G2(X) = 6/B       = 6/N
C.
C.----------------------------------------------------------------------
      X = 1.0
      DO 10 I = 1, N
         X = X * RNUNIF(I)
   10 CONTINUE
      RNERLA = - AV * LOG(X) / FLOAT(N)
      RETURN
      END
