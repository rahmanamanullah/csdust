CDECK  ID>, IRNNBL.
      FUNCTION IRNNBL ( M, P )
C-----------------------------------------------------------------------
C
C...       FUNCTION IRNNBL ( M, P )                         Ch.Walck    841115
C.
C.    Pseudorandom number from negative binomial distribution.
C.
C.    P(R;M,P) = (R-1)! P**M (1-P)**(R-M) / (M-1)! (R-M)!
C.
C.    Variable:          R >= M      (integer)
C.    Parameters:        M >= 1      (integer)
C.                       0 <= P <= 1 (real)
C.
C.    Expectation value: E(R) = M/P
C.    Variance:          V(R) = M*(1-P)/P**2
C.    Skewness:         G1(R) = (2-P)/SQRT(M*(1-P))
C.    Kurtosis:         G2(R) = (P**2-6*P+6)/(M*(1-P))
C.
C.    This negative binomial describes the probability of having to wait
C.    a certain number of trials until M successes have occurred, when P
C.    is the probability of a success in a single trial.
C.
C.----------------------------------------------------------------------
      R  = RNUNIF(P)
      IR = M
      Q  = 1.0 - P
      PN = P**M
      S  = PN
   10 IF ( R .LE. S ) GO TO 20
      PN = PN * Q * FLOAT(IR) / FLOAT(IR+1-M)
      IR = IR + 1
      S  = S + PN
      GO TO 10
   20 IRNNBL = IR
      RETURN
      END
