CDECK  ID>, RNNRM6.
      FUNCTION RNNRM6 ( XMU, XSIG )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNNRM6 ( XMU, XSIG )                    Ch.Walck    870612
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
C.    Technique: Method proposed by G. Marsaglia
C.
C.----------------------------------------------------------------------
C     A1 = 16*EXP(-2)/SQRT(2*PI)
      PARAMETER ( A1=0.8638554, A2=0.1108, A12= 0.9746554
     +        , A123=0.9973002039, F3MAX=0.0081, YMX=0.398942280401433 )
      U = RNUNIF(DUM)
C
C        From 2(r1+r2+r3)-3 distribution (F1) inscribed under N(0,1)
C        with a probability of 86.38554 %
C
      IF ( U .LT. A1 ) THEN
C        R = 2.0 * (RNUNIF(DUM1)+RNUNIF(DUM2)+RNUNIF(DUM3)) - 3.0
C        use U/A1 as a uniform random number to speed up routine
         R = 2.0 * (U/A1+RNUNIF(DUM1)+RNUNIF(DUM2)) - 3.0
C
C        From 1.5(r1+r2-1) distribution (F2) inscribed under
C        N(0,1)-A1*F1 with a probability of 11.08 %
C
      ELSE IF ( U .LT. A12 ) THEN
         R = 1.5 * (RNUNIF(DUM1)+RNUNIF(DUM2)-1.0)
C
C        From F3=N(0,1)-A1*F1-A2*F2 between -3 and 3 by reject-accept
C        method with a probability of 2.26448039 %
C
      ELSE IF ( U .LT. A123 ) THEN
    1    R = 6.0 * RNUNIF(DUM) - 3.0
         F3 = YMX * EXP(-R**2/2.0)
         IF ( ABS(R) .LT. 1.0 ) THEN
            F3 = F3 - A1 * (3.0-R**2) / 8.0
         ELSE
            F3 = F3 - A1 * (3.0-ABS(R))**2 / 16.0
         END IF
         IF ( ABS(R) .LT. 1.5 ) F3 = F3 - A2*(1.5-ABS(R))/2.25
         IF ( F3MAX*RNUNIF(DUM) .GT. F3 ) GO TO 1
C
C        From tail outside +-3 with a probability of 0.26997961 %
C
      ELSE
    2    R = 9.0 - 2.0 * LOG(RNUNIF(DUM))
         IF ( R*RNUNIF(DUM)**2 .GT. 9.0 ) GO TO 2
         R = SQRT(R)
         IF ( RNUNIF(DUM) .GE. 0.5 ) R = - R
      END IF
      RNNRM6 = XSIG * R + XMU
      RETURN
      END
