CDECK  ID>, RNWBLL.
      FUNCTION RNWBLL ( ETA, SIGMA )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNWBLL ( ETA, SIGMA )                   Ch.Walck    860725
C.
C.    Pseudorandom number from a Weibull distribution
C.
C.    F(X;ETA,SIGMA) = (ETA/SIGMA) * (X/SIGMA)**(ETA-1) *
C.                     EXP(-(X/SIGMA)**ETA)
C.
C.    Variable:          X > 0      (real)
C.    Parameters:        ETA > 0    (real)
C.                       SIGMA > 0  (real)
C.
C.    Expectation value: E(X) = SIGMA * GAMMA(1/ETA+1)
C.    Variance:          V(X) = SIGMA**2 * ( GAMMA(2/ETA+1) - E(X)**2 )
C.    Algebraic moment:  MU'(K) = SIGMA**K * GAMMA(K/ETA+1)
C.
C.----------------------------------------------------------------------
      RNWBLL = SIGMA * (-LOG(RNUNIF(ETA)))**(1/ETA)
      RETURN
      END
