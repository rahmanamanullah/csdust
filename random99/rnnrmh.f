CDECK  ID>, RNNRMH.
      FUNCTION RNNRMH ( XMU, XSIG )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNNRMH ( XMU, XSIG )                    Ch.Walck    870707
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
C.    Histogram technique proposed by G.Marsaglia and optimized for
C.    a VAX-11/780 in order to be as fast as possible.
C.
C.----------------------------------------------------------------------
      SAVE CNST, CUM, FC, GMAX, H, INDEX
      PARAMETER ( KMAX=9, L=3, NCUM=2*KMAX+L+1, CINV=3.0
     +          , C=0.333333333333333, TAIL=9.0 )
      DIMENSION CUM(NCUM), FC(0:KMAX), GMAX(L), H(KMAX), INDEX(NCUM)
      DATA CUM /0.251588818461995, 0.464554155476897, 0.625867971822992
     +         ,0.735208021606988, 0.801526114135487, 0.837520091810946
     +         ,0.863345852145349, 0.888893870772532, 0.909417513150139
     +         ,0.928729253873686, 0.946210513269492, 0.960296976243304
     +         ,0.968685323486566, 0.976282647502431, 0.983468998405244
     +         ,0.987841570511490, 0.990796136119449, 0.993495932182709
     +         ,0.995838082454374, 0.997844481918334, 0.999465009449567
     +         ,1.000000000000000/
      DATA FC  /0.797884560802865, 0.754766455385986, 0.638896011044704
     +         ,0.483941449038287, 0.328020149351987, 0.198954277585497
     +         ,0.107981933026376, 0.052443778187419, 0.022791972047595
     +         ,0.008863696823876/
      DATA GMAX/0.010661582884650, 0.007391288103347, 0.002498120086450/
      DATA H   /0.043118105416879, 0.115870444341282, 0.154954562006418
     +         ,0.155921299686299, 0.129065871766490, 0.090972344559121
     +         ,0.055538154838957, 0.029651806139824, 0.013928275223719/
      DATA INDEX/ 1, 2, 3, 4, 5, 6,12,13,14,11, 7,15,16, 8,10,17, 9,22
     +          ,19,18,20,21/
C     Changed CNST to separate (saved) variable 990203 to satisfy linux f77
C     EQUIVALENCE (FC(0),CNST)
      DATA CNST /0.797884560802865 /
C
      R = RNUNIF(DUM)
C
C        Use first bit after binary point to decide on sign
C
      SIGN = -1.0
      R = 2.0 * R
      IF ( R .GE. 1.0 ) THEN
         SIGN = 1.0
         R = R - 1.0
      END IF
C
C        If NCUM small sequential search more effective than binary
C        search especially when probabilities have been sorted in
C        descending order.
C
      DO 1 I = 1, NCUM
         IF ( R .LE. CUM(I) ) GO TO 2
    1 CONTINUE
      IPRC = NCUM
      GO TO 3
    2 IPRC = I
      IPRC = INDEX(IPRC)
C
C        Histogram bin
C
    3 IF ( IPRC .LE. KMAX ) THEN
         X = (RNUNIF(DUM)+FLOAT(IPRC-1))*C
C
C        Inscribed triangle
C
      ELSE IF ( IPRC .LE. KMAX+L ) THEN
         X = (MIN(RNUNIF(DUM1),RNUNIF(DUM2))+FLOAT(IPRC-KMAX-1))*C
C
C        Subscribed triangle
C
      ELSE IF ( IPRC .LE. 2*KMAX ) THEN
         INX = IPRC - KMAX
    4    X = (MIN(RNUNIF(DUM1),RNUNIF(DUM2))+FLOAT(INX-1))*C
         GX = H(INX)*(INX-X*CINV)
         FX = CNST * EXP(-X**2/2.0) - FC(INX)
         IF ( RNUNIF(DUM)*GX .GE. FX ) GO TO 4
C
C        Remaining area above histogram bin and inscribed triangle
C
      ELSE IF ( IPRC .LE. 2*KMAX+L ) THEN
         INX = IPRC - 2*KMAX
    5    X = (RNUNIF(DUM)+FLOAT(INX-1))*C
         FX = CNST*EXP(-X**2/2.0) - FC(INX) - H(INX)*(FLOAT(INX)-X*CINV)
         IF ( GMAX(INX)*RNUNIF(DUM) .GE. FX ) GO TO 5
C
C        Tail
C
      ELSE
C        TAIL=9.0 now in parameter statement
C        TAIL = (KMAX*C)**2
    6    X = TAIL - 2.0 * LOG(RNUNIF(DUM))
         IF ( X*RNUNIF(DUM)**2 .GT. TAIL ) GO TO 6
         X = SQRT(X)
      END IF
C
C        Add sign
C
      RNNRMH = SIGN*XSIG*X + XMU
      RETURN
      END
