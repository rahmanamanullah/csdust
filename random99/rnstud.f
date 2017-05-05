CDECK  ID>, RNSTUD.
      FUNCTION RNSTUD ( NDF )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNSTUD ( NDF )                          Ch.Walck    841102
C.
C.    Pseudorandom number from students t-distribution with NDF degrees
C.    of freedom.
C.
C.    F(T;NDF) = GAMMA((NDF+1)/2) / SQRT(NDF*PI) / GAMMA(NDF/2)
C.             / (1+T**2/NDF)**((NDF+1)/2)
C.
C.    Variable:          T       (real)
C.    Parameter:         NDF > 0 (integer)
C.
C.    Expectation value: E(T) = 0
C.    Variance:          V(T) = NDF/(NDF-2)   for   NDF > 2
C.    Skewness:         G1(T) = 0
C.    Kurtosis:         G2(T) = 6/(NDF-4)     for   NDF > 4
C.
C.----------------------------------------------------------------------
      RNSTUD = RNNRMT(0.0,1.0) / SQRT(RNCHSQ(NDF)/FLOAT(NDF))
      RETURN
      END
