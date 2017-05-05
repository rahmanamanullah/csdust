CDECK  ID>, RNNRM1.
      FUNCTION RNNRM1 ( X, S )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNNRM1 ( X, S )                         Ch.Walck    841102
C.
C.    Generate normal (N(X,S)) random numbers using the
C.    exact transformation technique of Box and Muller.
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
      SAVE N, A, Z
      PARAMETER (TWOPI=6.2831853072)
      DATA N / 0 /
      IF ( N .EQ. 0 ) THEN
         Z = SQRT ( - 2.0 * LOG(RNUNIF(X)) )
         A = TWOPI * RNUNIF(Z)
         B = COS(A)
      ELSE
         B = SIN(A)
      END IF
      N = 1 - N
      RNNRM1 = X + S * Z * B
      RETURN
      END
