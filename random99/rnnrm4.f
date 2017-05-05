CDECK  ID>, RNNRM4.
      FUNCTION RNNRM4 ( XMU, XSIG )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNNRM4 ( XMU, XSIG )                    Ch.Walck    870607
C.
C.    Gaussian random number with mean XMU and standard deviation XSIG.
C.
C.    F(X;XMU,XSIG) = EXP(-((X-XMU)/XSIG)**2/2) / SQRT(2*PI) / XSIG
C.
C.    Variable:          X        (real)
C.    Parameters:        XMU      (real)
C.                       XSIG > 0 (real)
C.
C.    Expectation value: E(Z) = XMU
C.    Variance:          V(Z) = XSIG**2
C.    Skewness:         G1(Z) = 0
C.    Kurtosis:         G2(Z) = 0
C.
C.    Technique:
C.    Composition-rejection method with one term as proposed by Butcher
C.    and Kahn (separately).
C.
C.----------------------------------------------------------------------
    1 Y = -LOG(RNUNIF(DUM))
      R = RNUNIF(DUM)
      G = EXP(-(Y-1.0)**2/2.0)
      IF ( R .GT. G ) GO TO 1
      RNNRM4 = XSIG * SIGN(Y,R-G/2.0) + XMU
      RETURN
      END
