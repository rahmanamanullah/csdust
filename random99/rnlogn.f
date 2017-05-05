CDECK  ID>, RNLOGN.
      FUNCTION RNLOGN ( X, S )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNLOGN ( X, S )                         Ch.Walck    841102
C.
C.    Pseudorandom number from log-normal distribution with parameters
C.    X and S (mean and standard deviation of variable from the normal
C.    distribution u, which if exponentiated gives the required random
C.    number from a log-normal ditribution, e**u).
C.
C.    F(Z;X,S) = EXP(-(LOG(Z)-X)**2/2/S**2) / Z / SQRT(2*PI) / S
C.
C.    Variable:          Z > 0 (real)
C.    Parameters:        X     (real)
C.                       S > 0 (real)
C.
C.    Expectation value: E(Z) = EXP(X+S**2/2)
C.    Variance:          V(Z) = EXP(2*X+S**2) * (EXP(S**2)-1)
C.    Skewness:         G1(Z) = SQRT((EXP(S**2)-1)) * (EXP(S**2)+2)
C.    Kurtosis:         G2(Z) = (EXP(S**2)-1)
C.                           * (EXP(3*S**2)+3*EXP(2*S**2)+6*EXP(S**2)+6)
C.
C.----------------------------------------------------------------------
      RNLOGN = EXP ( RNNRMT(X,S) )
      RETURN
      END
