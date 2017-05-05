CDECK  ID>, RNEXPA.
      FUNCTION RNEXPA ( A )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNEXPA ( A )                            Ch.Walck    861010
C.                                                          Last mod    891227
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
C.    Generate p.r.n. from exponential distribution by method of Ahrens
C.    as given in the article "Computer Methods for Sampling from the
C.    Exponential and Normal Distributions" by J.H.Ahrens and U.Dieter
C.    in Communications of the ACM vol.15 (1972) 873.
C.
C.----------------------------------------------------------------------
      SAVE INIT, Q
      DIMENSION Q(50)
      EQUIVALENCE (Q(1),Q1)
      DATA INIT /0/
C
C     Initialize at first call:
C     Dimension needed for Q-vector is 10 for VAX single
C     precision (16 for VAX double precision) whereafter the
C     largest representable fraction below 1 is exceeded.
C
      IF ( INIT .EQ. 0 ) THEN
         INIT = 1
         Q1   = LOG(2.0)
*        Q(1) = Q1
         QT   = Q1
         N    = 1
   10    N    = N + 1
         QT   = QT * Q1 / FLOAT(N)
         Q(N) = QT + Q(N-1)
         IF ( Q(N).LT.1.0 .AND. Q(N).NE.Q(N-1) .AND. N.LT.50 ) GO TO 10
         Q(N) = 1.0
      END IF
C
      X = 0.0
      U = RNUNIF(DUM)
    2 IF ( U .GE. 0.5 ) GO TO 4
      X = X + Q1
      U = 2.0 * U
      GO TO 2
    4 U = 2.0 * U - 1.0
      IF ( U .LE. Q1 ) GO TO 100
      I = 2
      UMIN = RNUNIF(DUM)
    7 UMIN = MIN(UMIN,RNUNIF(DUM))
      IF ( U .LE. Q(I) ) GO TO 9
      I = I + 1
      GO TO 7
    9 U = Q1 * UMIN
  100 RNEXPA = A * ( X + U )
      RETURN
      END
