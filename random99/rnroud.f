CDECK  ID>, RNROUD.
      FUNCTION RNROUD ( XMU, XSIG )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNROUD ( XMU, XSIG )                    Ch.Walck    981002
C.
C.    Gaussian random number with mean XMU and standard deviation XSIG.
C.
C.    F(X;XMU,XSIG) = EXP(-((X-XMU)/XSIG)**2/2) / SQRT(2*PI) / XSIG
C.
C.    Variable:          X        (real)
C.    Parameters:        XMU      (real)
C.                       XSIG > 0 (real)
C.
C.    Expectation value: E(Z) = XMU
C.    Variance:          V(Z) = XSIG**2
C.    Skewness:         G1(Z) = 0
C.    Kurtosis:         G2(Z) = 0
C.
C.    Using the technique of a ratio of uniform deviates as originally
C.    proposed by A. J. Kinderman and John F. Monahan in "Computer 
C.    Generation of Random Variables Using the Ratio of Uniform Deviates"
C.    in ACM Transactions on Mathematical Software 3 (1977) 257-260 and 
C.    later improved by Joseph L. Leva in "A Fast Normal Random Number
C.    Generator" in ACM TOMS 18 (1992) 449-453 and "Algorithm 712: A
C.    Normal Random Number Generator" ibid 18 (1992) 454-455.
C.
C.    Average number of uniform pseudorandom numbers consumed i 2.738
C.    per call and the number of logarithms taken is reduced to 0.012
C.    per call by first cutting on an inner and outer boundary to the 
C.    acceptance region (by quadratic form boundaries, ellipses).
C.
C.----------------------------------------------------------------------
      PARAMETER (A=0.19600,B=0.25472,S=0.449871,T=-0.386595)
      PARAMETER (R1=0.27597,R2=0.27846)
C
    1 U = RNUNIF(DUM)
      V = RNUNIF(DUM)
*       >2*sqrt(2/e)
      V = 1.7156 * ( V - 0.5 )
      X = U - S
      Y = ABS(V) - T
      Q = X**2 + Y * ( A*Y - B*X )
      IF ( Q .LT. R1 ) GO TO 2
      IF ( Q .GT. R2 ) GO TO 1
      IF ( V**2 .GT. -4.0*LOG(U)*U**2 ) GO TO 1
    2 RNROUD = XSIG*V/U + XMU
      RETURN
      END
