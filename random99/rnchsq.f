CDECK  ID>, RNCHSQ.
      FUNCTION RNCHSQ ( NDF )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNCHSQ ( NDF )                          Ch.Walck    841105
C.                                                          Modified    860918
C.
C.    Pseudorandom number from chisquared distribution with NDF degrees
C.    of freedom based on standard normal pseudorandom numbers obtained
C.    with the exact transformation technique of Box and Muller.
C.
C.    F(X;NDF) = (X/2)**(NDF/2-1) * EXP(-X/2) / GAMMA(NDF/2) / 2
C.
C.    Variable:          X >= 0  (real)
C.    Parameter:         NDF > 0 (integer)
C.
C.    Expectation value: E(X) = NDF
C.    Variance:          V(X) = 2*NDF
C.    Skewness:         G1(X) = 2*SQRT(2/NDF)
C.    Kurtosis:         G2(X) = 12/NDF
C.
C.    For NDF greater than 100 the Wilson-Hilferty approximation is used.
C.
C.    modified in september 1986 when noted that the product of many
C.    [0,1] random numbers may become zero! also normal approximation
C.    used changed to the version due to Wilson and Hilferty (1931).
C.
C.----------------------------------------------------------------------
      PARAMETER (TWOPI=6.2831853072)
      IF ( NDF .LE. 0 ) THEN
         R = 0.0
C
C     For NDF from 1 to 50 use exact technique
C
      ELSE IF ( NDF .LE. 50 ) THEN
         R = 0.0
         IF ( NDF .GE. 2 ) THEN
            K = NDF / 2
            P = 1.0
            DO 10 I = 1, K
   10       P = P * RNUNIF(R)
            R = - 2.0 * LOG(P)
         END IF
         IF ( MOD(NDF,2) .EQ. 1 )
     +      R = R - 2.0 * LOG(RNUNIF(P)) * COS(TWOPI*RNUNIF(R))**2
C
C     For NDF from 51 to 100 use exact technique but make product of
C     [0,1] pseudorandom numbers in two steps in order to avoid the
C     result becoming zero.
C
      ELSE IF ( NDF .LE. 100 ) THEN
         R = 0.0
         K = NDF / 2
         L = K / 2
         P = 1.0
         DO 11 I = 1, L
   11    P = P * RNUNIF(R)
         Q = 1.0
         DO 12 I = L+1, K
   12    Q = Q * RNUNIF(R)
         R = - 2.0 * ( LOG(P) + LOG(Q) )
         IF ( MOD(NDF,2) .EQ. 1 )
     +      R = R - 2.0 * LOG(RNUNIF(P)) * COS(TWOPI*RNUNIF(Q))**2
C
C     For NDF>100 use normal approximation (Wilson-Hilferty)
C
      ELSE
         P = 1.0 - 2.0/9.0/FLOAT(NDF)
         R = FLOAT(NDF) * RNNRMT(1.0-P,SQRT(P))**3
      END IF
      RNCHSQ = R
      RETURN
      END
