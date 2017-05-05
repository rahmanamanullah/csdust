CDECK  ID>, RNNRM5.
      FUNCTION RNNRM5 ( XMU, XSIG )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNNRM5 ( XMU, XSIG )                    Ch.Walck    870607
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
C.    Composition-rejection method with two terms as proposed by Butcher.
C.
C.----------------------------------------------------------------------
    1 XI1 = 3.0*RNUNIF(DUM)
      XI2 = RNUNIF(DUM)
      IF ( XI1 .GT. 2.0 ) THEN
         Y = 1.0 - 0.5 * LOG(XI1-2.0)
         Z = (Y-2.0)**2/2.0
      ELSE
         Y = XI1/2.0
         Z = Y**2/2.0
      END IF
      G = EXP(-Z)
      IF ( XI2 .GT. G ) GO TO 1
      RNNRM5 = XSIG * SIGN(Y,XI2-G/2.0) + XMU
      RETURN
      END
