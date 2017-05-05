CDECK  ID>, IRNGEO.
      FUNCTION IRNGEO ( P )
C-----------------------------------------------------------------------
C
C...       FUNCTION IRNGEO ( P )                            Ch.Walck    841115
C.
C.    Random number from geometric distribution
C.    (probability of success in single trial is P)
C.
C.    P(R;P) = P * (1-P)**(R-1)
C.
C.    Variable:          R >= 1     (integer)
C.    Parameter:         0 < P <= 1 (real)
C.
C.    Expectation value: E(R) = 1/P
C.    Variance:          V(R) = (1-P)/P**2
C.    Skewness:         G1(R) = (2-P)/SQRT(1-P)
C.    Kurtosis:         G2(R) = (P**2-6*P+6)/(1-P)
C.
C.----------------------------------------------------------------------
      IF ( P .LE. 0.0 ) THEN
         IR = 99999
      ELSE IF ( P .GE. 1.00 ) THEN
         IR = 1
      ELSE IF ( P .LE. 0.07 ) THEN
*
*        Use logarithmic solution for P<0.07:
*
         Q  = 1.0 - P
         IR = LOG ( Q * RNUNIF(Q) ) / LOG ( Q )
      ELSE
*
*        Use cumulant technique for P>0.07:
*
         IR = 1
         Q  = 1.0 - P
         T  = Q
         R  = RNUNIF ( Q )
   10    IF ( R .GE. T ) GO TO 20
         IR = IR + 1
         T  = T * Q
         GO TO 10
      END IF
   20 IRNGEO = IR
      RETURN
      END
