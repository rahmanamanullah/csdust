CDECK  ID>, RNNRM3.
      FUNCTION RNNRM3 ( XMU, XSIG )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNNRM3 ( XMU, XSIG )                    Ch.Walck    861013
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
C.    Centre/tail method of Ahrens and Dieter (1972) as given in the
C.    article "Computer Methods for Sampling From the Exponential and
C.    Normal Distributions" by J.H.Ahrens and U.Dieter in Communications
C.    of the ACM Vol.15 (1972) 873. The tail method proposed, however,
C.    has been exchanged to a faster alternative from the same paper.
C.
C.----------------------------------------------------------------------
      PARAMETER (CNST=0.842700792949715,SQRT2=1.414213562373095)
      REAL NU, NUSTAR
C
      U = RNUNIF(DUM)
      SIGN = -1.0
      U = 2.0 * U
      IF ( U .GT. 1.0 ) THEN
         SIGN = 1.0
         U = U - 1.0
      END IF
C
      IF ( U .GT. CNST ) GO TO 7
C
C     Start center method (Dieter)
C
    3 U0 = RNUNIF(DUM)
      NU = U0
C
    4 U1 = RNUNIF(DUM)
      U2 = RNUNIF(DUM)
      NUSTAR = MAX(U1,U2)
      IF ( NU .LT. NUSTAR ) GO TO 6
C
      U1 = RNUNIF(DUM)
      U2 = RNUNIF(DUM)
      NU = MAX(U1,U2)
      IF ( NU .LT. NUSTAR ) GO TO 4
      GO TO 3
C
    6 Y = U0
      GO TO 15
C
C     Start tail method (Ahrens)
C     In the paper another method by Ahrens avoiding the logarithm is
C     used. The method used as default below is, however, faster.
C
    7 Y = 1.0 - LOG(RNUNIF(DUM))
      IF ( RNUNIF(DUM)**2*Y .GT. 1.0 ) GO TO 7
      Y = SQRT(Y)
C
   15 RNNRM3 = SIGN*SQRT2*Y*XSIG + XMU
      RETURN
      END
