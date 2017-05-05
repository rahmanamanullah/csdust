CDECK  ID>, RNBETA.
      FUNCTION RNBETA ( P, Q )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNBETA ( P, Q )                         Ch.Walck    861013
C.
C.    Pseudorandom number from beta-distribution with parameters
C.    p and q for integer values of p and q (declared integers!).
C.
C.    F(X;P,Q) = X**(P-1) * (1-X)**(Q-1) / BETA(P,Q)
C.
C.    Variable:          X, 0 <= X <= 1    (real)
C.    Parameters:        P > 0 and Q > 0   (integers)
C.
C.    Expectation value: E(X) = P/(P+Q)
C.    Variance:          V(X) = P*Q / (P+Q)**2 / (P+Q+1)
C.    Skewness:         G1(X) = 2*(Q-P)*SQRT(P+Q+1)/(P+Q+2)/SQRT(P*Q)
C.    Kurtosis:         G2(X) = 3*(P+Q+1)*(2*(P+Q)**2+P*Q*(P+Q-6))
C.                              /(P*Q*(P+Q+2)*(P+Q+3)) - 3
C.
C.    Note that the chisquare technique may be used also for M and N
C.    half-integers! This, however, is not supported by this routine.
C.
C.----------------------------------------------------------------------
      DIMENSION RN(9)
      INTEGER P, Q, PQ
      PQ = P + Q
C
C     Jump if P + Q less or equal to 4
C
      IF ( PQ .LE. 4 ) GO TO 40
C
C     Q = 1 => Use exact solution
C
      IF ( Q .EQ. 1 ) THEN
         R = RNUNIF(P)**(1.0/FLOAT(P))
         GO TO 50
C
C     P = 1 => Use exact solution
C
      ELSE IF ( P .EQ. 1 ) THEN
         R = 1.0 - RNUNIF(Q)**(1.0/FLOAT(Q))
         GO TO 50
C
C     P + Q greater or equal to 11 => Use chisquare expression
C
      ELSE IF ( PQ .GE. 11 ) THEN
C        --- Note that this expression may be used also for  ---
C        --- half-integer P and Q! (not supported here)      ---
         R1 = RNCHSQ(2*P)
         R = R1 / ( R1 + RNCHSQ(2*Q) )
         GO TO 50
      END IF
C
C     P + Q le 10 but P ne 1 and Q ne 1 => Take L:th uniform prn out of K
C
   40 K = P + Q - 1
      L = P
      CALL RNUNIV ( RN, K )
C     --- Sorting --- Maybe not smartest method but for K LE 9   ---
C     ---             seems faster than some alternatives tried  ---
      MIN = 1
      MAX = K
   42 DO 43 I = MIN, MAX
         IF ( RN(I) .LT. RN(MIN) ) THEN
            SV      = RN(MIN)
            RN(MIN) = RN(I)
            RN(I)   = SV
         ELSE IF ( RN(I) .GT. RN(MAX) ) THEN
            SV      = RN(MAX)
            RN(MAX) = RN(I)
            RN(I)   = SV
         END IF
   43 CONTINUE
      MIN = MIN + 1
      MAX = MAX - 1
      IF ( MAX-MIN .GE. 1 ) GO TO 42
      R = RN(L)
C
   50 RNBETA = R
      RETURN
      END
