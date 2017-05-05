CDECK  ID>, RDNRMT.
      DOUBLE PRECISION FUNCTION RDNRMT ( XMU, XSIG )
C-----------------------------------------------------------------------
C
C...       FUNCTION RDNRMT ( XMU, XSIG )                    Ch.Walck    001005
C.
C.    Gaussian random number with mean XMU and standard deviation XSIG.
C.
C.    This version is using double precision arithmetics in order 
C.    to avoid the discreteness in the far tails caused by the 24
C.    bit precision of the random numbers generated by RNUNIF as
C.    used in RNNRMT. Return value is double precision and RDNRMT
C.    should be declared accordingly in the calling routine but
C.    note that the arguments are still in single precision!
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
C.    the ACM Vol.15 (1972) 873. 
C.
C.----------------------------------------------------------------------
      IMPLICIT NONE
      DOUBLE PRECISION A, A1, A2, A3, DX12, DX23, Q1, Q2, SLOPE, U, U0
     +               , X, XINT, X1, X2, X3, X3SQ, YMX, YT, Y1, Y2, Y3
     +               , RNDBLE
      REAL             DUM, XMU, XSIG
      PARAMETER (   A=0.919544405706926D0,    A1=0.954057274493068D0,
     +             A2=0.978195649518392D0,    A3=0.993692071999222D0,
     +             X1=0.2897295736D0,         X2=1.84039874739771D0,
     +             X3=2.11402808333742D0,   X3SQ=4.46911473713927D0,
     +           DX12=1.55066917379771D0,   DX23=0.273629335939706D0,
     +           XINT=0.443299125820220D0, SLOPE=0.209694057195486D0,
     +            YMX=0.398942280401433D0,    YT=0.382544556042518D0,
     +             Y1=0.016397724358915D0,    Y2=0.015974522655238D0,
     +             Y3=0.042702581590795D0,    Q1=2.40375765693742D0,
     +             Q2=1.983915619969365D0 )
C
      U  = RNDBLE(DUM)
      U0 = RNDBLE(DUM)
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
    1    X  = X3SQ-2.0D0*DLOG(RNDBLE(DUM))
         IF ( X*RNDBLE(DUM)**2 .GT. X3SQ ) GO TO 1
         X = DSQRT(X)
C
C     From 2nd region between N(0,1) and trapezoid
C     0.29 < X < 1.84 with P = 2.41 % (sign decided below)
C
      ELSE IF ( U .LT. A2 ) THEN
    2    X = X1 + DX12 * RNDBLE(DUM)
         IF ( YMX*DEXP(-X**2/2.0D0)-XINT+X*SLOPE .LT. Y2*RNDBLE(DUM) )
     +      GO TO 2
C
C     From 3rd region between N(0,1) and trapezoid
C     1.84 < X < 2.11 with P = 1.55 % (sign decided below)
C
      ELSE IF ( U .LT. A3 ) THEN
    3    X = X2 + DX23 * RNDBLE(DUM)
         IF ( YMX*DEXP(-X**2/2.0D0)-XINT+X*SLOPE .LT. Y3*RNDBLE(DUM) ) 
     +      GO TO 3
C
C     From 1st (top) region between N(0,1) and trapezoid
C     0 < X < 0.29 with P = 0.63 % (sign decided below)
C
      ELSE
    4    X = X1 * RNDBLE(DUM)
         IF ( YMX*DEXP(-X**2/2.0D0)-YT .LT. Y1*RNDBLE(DUM) ) GO TO 4
      END IF
C
C     Attach sign
C
      IF ( U0 .GE. 0.5D0 ) X = -X
C
  100 RDNRMT = X*DBLE(XSIG) + DBLE(XMU)
      RETURN
      END
