CDECK  ID>, RNNORM.
      FUNCTION RNNORM ( X, S )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNNORM ( X, S )                         Ch.Walck    841102
C.
C.    Generate normal (N(X,S)) random numbers using the polar
C.    method of G.Marsaglia.
C.
C.    F(Z;X,S) = EXP(-((Z-X)/S)**2/2) / SQRT(2*PI) / S
C.
C.    Variable:          Z     (real)
C.    Parameters:        X     (real)
C.                       S > 0 (real)
C.
C.    Expectation value: E(Z) = X
C.    Variance:          V(Z) = S**2
C.    Skewness:         G1(Z) = 0
C.    Kurtosis:         G2(Z) = 0
C.
C.    W A R N I N G: This routine is potentially dangerous when used in
C.    applications where the restart facility may be used. This because
C.    it leaves a random number pending at every second call.
C.
C.----------------------------------------------------------------------
      SAVE N, V, Z
      DATA N / 0 /
      IF ( N .EQ. 0 ) THEN
C--
C--      (I)   Generate U and V uniformly between -1 and 1
C--
   10    U = 2.0 * RNUNIF(X) - 1.0
         V = 2.0 * RNUNIF(U) - 1.0
C--
C--      (II)  Calculate W = U**2 + V**2
C--
         W = U**2 + V**2
C--
C--      (III) If W>1 go to (I) ( 1-PI/4 i.e. approx. 21% rejection )
C--
         IF ( W .GT. 1.0 ) GO TO 10
C--
C--      (IV)  UZ and VZ with Z = SQRT(-2*LOG(W)/W) is N(0,1)
C--
         Z = SQRT ( - 2.0 * LOG(W) / W )
         A = U * Z
      ELSE
         A = V * Z
      END IF
      N = 1 - N
      RNNORM = X + S * A
      RETURN
      END
