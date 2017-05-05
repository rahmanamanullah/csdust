CDECK  ID>, RNNRMT.
      FUNCTION RNNRMT ( XMU, XSIG )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNNRMT ( XMU, XSIG )                    Ch.Walck    861005
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
C.    Trapezoidal method of Ahrens (1972) as given in the article
C.    "Computer Methods for Sampling From the Exponential and Normal
C.    Distributions" by J.H.Ahrens and U.Dieter in Communications of
C.    the ACM Vol.15 (1972) 873. In fact this is the fastest of the
C.    routines given here! (However, the CERN routine NORRAN, coded
C.    in assembler language, is faster.)
C.
C.----------------------------------------------------------------------
      PARAMETER (   A=0.919544405706926,    A1=0.954057274493068,
     +             A2=0.978195649518392,    A3=0.993692071999222,
     +             X1=0.2897295736,         X2=1.84039874739771,
     +             X3=2.11402808333742,   X3SQ=4.46911473713927,
     +           DX12=1.55066917379771,   DX23=0.273629335939706,
     +           XINT=0.443299125820220, SLOPE=0.209694057195486,
     +            YMX=0.398942280401433,    YT=0.382544556042518,
     +             Y1=0.016397724358915,    Y2=0.015974522655238,
     +             Y3=0.042702581590795,    Q1=2.40375765693742,
     +             Q2=1.983915619969365)
C
      U  = RNUNIF(DUM)
      U0 = RNUNIF(DUM)
C
C     From maximal trapezoid inscribed in N(0,1)
C    -2.11 < X < 2.11 with P = 91.95 %
C
      IF ( U .LT. A ) THEN
         X = Q1*U0 + Q2*U - X3
         GO TO 100
C
C     From tail of N(0,1)
C     X > 2.11 with P = 3.45 % (sign decided below)
C
      ELSE IF ( U .LT. A1 ) THEN
    1    X  = X3SQ-2.0*LOG(RNUNIF(DUM))
         IF ( X*RNUNIF(DUM)**2 .GT. X3SQ ) GO TO 1
         X = SQRT(X)
C
C     From 2nd region between N(0,1) and trapezoid
C     0.29 < X < 1.84 with P = 2.41 % (sign decided below)
C
      ELSE IF ( U .LT. A2 ) THEN
    2    X = X1 + DX12 * RNUNIF(DUM)
         IF ( YMX*EXP(-X**2/2.0)-XINT+X*SLOPE.LT.Y2*RNUNIF(DUM)) GO TO 2
C
C     From 3rd region between N(0,1) and trapezoid
C     1.84 < X < 2.11 with P = 1.55 % (sign decided below)
C
      ELSE IF ( U .LT. A3 ) THEN
    3    X = X2 + DX23 * RNUNIF(DUM)
         IF ( YMX*EXP(-X**2/2.0)-XINT+X*SLOPE.LT.Y3*RNUNIF(DUM)) GO TO 3
C
C     From 1st (top) region between N(0,1) and trapezoid
C     0 < X < 0.29 with P = 0.63 % (sign decided below)
C
      ELSE
    4    X = X1 * RNUNIF(DUM)
         IF ( YMX*EXP(-X**2/2.0)-YT .LT. Y1*RNUNIF(DUM) ) GO TO 4
      END IF
C
C     Attach sign
C
      IF ( U0 .GE. 0.5 ) X = -X
C
  100 RNNRMT = X*XSIG + XMU
      RETURN
      END
