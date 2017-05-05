CDECK  ID>, IRNUNI.
      FUNCTION IRNUNI ( ILOW, IUP )
C-----------------------------------------------------------------------
C
C...       FUNCTION IRNUNI ( ILOW, IUP )                    Ch.Walck    841106
C.
C.    Integer pseudorandom number from ILOW to IUP with equal
C.    probability for each value.
C.
C.    P(R;ILOW,IUP) = 1 / N   with   N = IUP-ILOW+1
C.
C.    Variable:          R, ILOW <= R <= IUP         (integer)
C.    Parameters:        ILOW and IUP, ILOW <= IUP   (integers)
C
C.    Expectation value: E(R) = (ILOW+IUP)/2
C.    Variance:          V(R) = (N**2-1)/12
C.    Skewness:         G1(R) = 0
C.    Kurtosis:         G2(R) = - 1.2 * (N**2+1) / (N**2-1)
C.
C.----------------------------------------------------------------------
      IRNUNI = MIN ( INT ( (IUP-ILOW+1) * RNUNIF(DUM) + ILOW ), IUP )
      RETURN
      END
