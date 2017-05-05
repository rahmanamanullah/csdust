CDECK  ID>, IRNCPO.
      FUNCTION IRNCPO ( A, B )
C-----------------------------------------------------------------------
C
C...       FUNCTION IRNCPO ( A, B )                         Ch.Walck    841119
C.
C.    Pseudorandom number from a compound Poisson distribution with
C.    Parameters A and B.
C.
C.    P(R;A,B) = SUM ( (N*B)**R * EXP(-N*B) / R! * A**N EXP(-A) / N! )
C.                               (where sum runs from N=0 to infinity)
C.
C.    Variable:          R > 0 (integer)
C.    Parameters:        A > 0 (real)
C.                       B > 0 (real)
C.
C.    Expectation value: E(R) = A*B
C.    Variance:          V(R) = A*B*(1+A)
C.
C.    The distribution is the sum of N poisson variables all of mean B
C.    where N is also a Poisson variable of mean A.
C.
C.----------------------------------------------------------------------
      IR = IRNPOI ( A )
      IF ( IR .NE. 0 ) IR = IRNPOI ( IR * B )
      IRNCPO = IR
      RETURN
      END
