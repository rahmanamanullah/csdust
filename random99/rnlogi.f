CDECK  ID>, RNLOGI.
      FUNCTION RNLOGI ( A, K )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNLOGI ( A, K )                         Ch.Walck    961120
C.
C.    Pseudorandom number from logistic distribution with location 
C.    parameter A (the mode, median, and mean) and scale parameter
C.    K (related to the standard deviation which is K*PI/SQRT(3) ).
C.    Note that K is declared real!
C.
C.                   exp(z)                        x - a
C.    f(x;a,k) = ---------------      with     z = -----
C.               k (1+exp(z))**2                     k
C.
C.    Variable:          X     (real)
C.    Parameters:        A     (real)
C.                       K > 0 (real)
C.
C.    Expectation value: E(Z) = A
C.    Variance:          V(Z) = K**2*PI**2/3
C.    Skewness:         G1(Z) = 0
C.    Kurtosis:         G2(Z) = 4.2
C.
C.----------------------------------------------------------------------
      REAL K
      XI = RNUNIF(A)
      RNLOGI = A + K * LOG(XI/(1.0-XI))
      RETURN
      END
