CDECK  ID>, RNNRM2.
      FUNCTION RNNRM2 ( X, S )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNNRM2 ( X, S )                         Ch.Walck    841008
C.                                                          Last mod    891230
C.
C.    Gaussian random number with mean X and standard deviation S.
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
C.    Technique:
C.    The sum of N independent uniform [0,1] distributed random numbers,
C.    RN, have expectation value E(RN)=N/2 and variance V(RN)=N/12.
C.    by the central limit theorem
C.      ZN = (RN-E(RN)/SQRT(V(RN)) = (RN-(N/2))/SQRT(N/12) -> N(0,1).
C.    a practical and for many purposes sufficient choice is N=12
C.    i.e. Z12 = R12 - 6.
C.
C.    Note, however, that this technique is neither exact nor fast!
C.    Rewritten to use RNUNIV in december 1989.
C.
C.----------------------------------------------------------------------
      DIMENSION RN(12)
      CALL RNUNIV ( RN, 12 )
      Z = 0.0
      DO 10 I= 1, 12
         Z = Z + RN(I)
   10 CONTINUE
      RNNRM2 = X + S * ( Z - 6.0 )
      RETURN
      END
