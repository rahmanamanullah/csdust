CDECK  ID>, RNHEXP.
      FUNCTION RNHEXP ( P1, SL1, SL2 )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNHEXP ( P1, SL1, SL2 )                 Ch.Walck    860806
C.
C.    Pseudorandom number from hyperexponential distribution
C.
C.    F(X;P1,SL1,SL2) = P1*SL1*EXP(-SL1*X) + P2*SL2*EXP(-SL2*X)
C.
C.    with P2 = 1 - P1.
C.
C.    Variable:          X > 0          (real)
C.    Parameters:        SL1 > 0        (real)
C.                       SL2 > 0        (real)
C.                       0 <= P1 <= 1   (real)
C.
C.    Expectation value: E(X) = P1/SL1 + P2/SL2
C.    Algebraic moments: MU'(N) = N! ( P1/SL1**N + P2/SL2**N )
C.
C.----------------------------------------------------------------------
      R = - LOG(RNUNIF(SL1))
      IF ( RNUNIF(P1) .LE. P1 ) THEN
         R = R / SL1
      ELSE
         R = R / SL2
      END IF
      RNHEXP = R
      RETURN
      END
