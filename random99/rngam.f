CDECK  ID>, RNGAM.
      FUNCTION RNGAM ( A, B )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNGAM ( A, B )                          Ch.Walck    841121
C.                                                          Last mod    891230
C.
C.    Pseudorandom number from gamma distribution
C.
C.                     b-1  -ax
C.               a (ax)    e
C.    f(x;a,b) = --------------
C.                  Gamma(b)
C.
C.    Variable:          X > 0     (real)
C.    Parameters:        A > 0     (real)
C.                       B > 0     (real)
C.
C.    Expectation value: E(X) = B/A
C.    Variance:          V(X) = B/A**2
C.    Skewness:         G1(X) = 2/SQRT(B)
C.    Kurtosis:         G2(X) = 6/B
C.
C.    This routine is a rewritten version of the CERN library routine
C.    V109. Uses RNUNIF, RNUNIV and RNNRMT in order to have same random
C.    sequence as all other random routines on this PAM-file. Original
C.    version uses NRAN and NORRAN.
C.
C.----------------------------------------------------------------------
      DIMENSION RN(15)
      R = 0.0
      IF ( B .LE. 15.0 ) THEN
C--
C--   For B < 15 make exact calculation for the Erlangian case
C--   (B integer) and Johnk's algorithm otherwise (B non-integer).
C--
         IB = INT(B)
         FB = B - FLOAT(IB)
         IF ( IB .NE. 0 ) THEN
            CALL RNUNIV ( RN, IB )
            X = 1.0
            DO 10 I = 1, IB
               X = X * RN(I)
   10       CONTINUE
            R = - LOG(X)
         END IF
         IF ( FB .LT. 1.0E-5 ) GO TO 998
         X1 = - LOG(RNUNIF(X))
         IF ( FB .GT. 0.9999 ) THEN
            R = R + X1
            GO TO 998
         END IF
C        ..... W1 = R1**(1/FB)
   20    WLOG = LOG(RNUNIF(FB)) / FB
         IF ( WLOG .LT. -100.0 ) GO TO 998
         W1   = EXP(WLOG)
C        ..... W2 = R2**(1/(1-FB))
         WLOG = LOG(RNUNIF(W1)) / (1.0-FB)
         IF ( WLOG .LT. -100.0 ) THEN
            R = R + X1
         ELSE
            W2 = EXP(WLOG)
            W = W1 + W2
            IF ( W .GT. 1.0 ) GO TO 20
            X2 = W1 / W
            R = R + X1 * X2
         END IF
      ELSE
C--
C--   Wilson-Hilferty approximation for B > 15
C--
   40    Q = 1.0 - 1.0/9.0/B + RNNRMT(0.0,1.0)/3.0/SQRT(B)
         IF ( Q .LE. 0.0 ) GO TO 40
         R = B * Q**3
      END IF
C
  998 RNGAM = R / A
      RETURN
      END
