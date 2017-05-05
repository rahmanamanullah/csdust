CDECK  ID>, RNEXPM.
      FUNCTION RNEXPM ( A )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNEXPM ( A )                            Ch.Walck    861010
C.
C.    Random number from exponential distribution with mean A.
C.
C.    F(X;A) = EXP(-X/A) / A
C.
C.    Variable:          X > 0    (real)
C.    Parameter:         A > 0    (real)
C.
C.    Expectation value: E(X) = A
C.    Variance:          V(X) = A**2
C.    Skewness:         G1(X) = 2
C.    Kurtosis:         G2(X) = 6
C.
C.    Generate p.r.n. from exponential distribution by a method of G.
C.    Marsaglia (1961) as given in the article "Computer Methods for
C.    Sampling From the Exponential and Normal Distributions" by J. H.
C.    Ahrens and U.Dieter in Communications of the ACM Vol.15 (1972) 873
C.
C.----------------------------------------------------------------------
      SAVE INIT, P, Q
      DIMENSION P(100), Q(100)
      DATA INIT /0/
C
C     Initialize at first call:
C     Dimension needed for P- and Q-VECTOR is 18 for VAX single
C     precision (40 for VAX double precision) whereafter the
C     largest representable fraction below 1 is exceeded in both
C     vectors.
C
      IF ( INIT .EQ. 0 ) THEN
         INIT = 1
         EM1  = EXP(1.0) - 1.0
         QT   = 1.0 / EM1
         P(1) = 1.0 - EXP(-1.0)
         Q(1) = QT
         N    = 1
   10    N    = N + 1
         XN   = FLOAT(N)
         QT   = QT / XN
         Q(N) = QT + Q(N-1)
         P(N) = 1.0 - EXP(-XN)
         IF ( ( P(N).LT.1.0.OR.Q(N).LT.1.0 )
     +        .AND. ( P(N).NE.P(N-1).OR.Q(N).NE.Q(N-1) )
     +        .AND. N.LT.100 ) GO TO 10
         P(N) = 1.0
         Q(N) = 1.0
      END IF
C
      I = 0
      U = RNUNIF(DUM)
    2 IF ( U .LE. P(I+1) ) GO TO 4
      I = I + 1
      GO TO 2
    4 K = 1
      U    = RNUNIF(DUM)
      UMIN = RNUNIF(DUM)
    5 IF ( U .LE. Q(K) ) GO TO 8
      UMIN = MIN(UMIN,RNUNIF(DUM))
      K = K + 1
      GO TO 5
    8 RNEXPM = A * ( FLOAT(I) + UMIN )
      RETURN
      END
