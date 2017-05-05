CDECK  ID>, RNDEXP.
      FUNCTION RNDEXP ( XMU, XLAM )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNDEXP ( XMU, XLAM )                    Ch.Walck    860725
C.
C.    Pseudorandom number from a double exponential distribution
C.
C.    F(X;XMU,XLAM) = XLAM * EXP(-XLAM*ABS(X-XMU)) / 2
C.
C.    Variable:          X         (real)
C.    Parameters:        XMU       (real)
C.                       XLAM > 0  (real)
C.
C.    Expectation value: E(X) = XMU
C.    Variance:          V(X) = 2/XLAM**2
C.    Skewness:         G1(X) = 0
C.    Kurtosis:         G2(X) = 3
C.
C.----------------------------------------------------------------------
      R = RNUNIF(XLAM)
      IF ( R .LE. 0.5 ) THEN
         RN = XMU + LOG(2.0*R)/XLAM
      ELSE
C        Should, in principle, be LOG(2(1-R)) below but trying
C        to avoid LOG(0)! (With RNUNIF replacing RNDM this is
C        not anymore a problem!)
         RN = XMU - LOG(2.0*R-1.0)/XLAM
      END IF
      RNDEXP = RN
      RETURN
      END
