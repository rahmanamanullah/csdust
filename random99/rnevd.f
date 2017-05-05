CDECK  ID>, RNEVD.
      FUNCTION RNEVD ( xmu, xsig )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNEVD ( XMU, XSIG )                     Ch.Walck    961117
C.
C.    Random number from extreme value distribution with location
C.    parameter (mode) XMU and shape parameter XSIG
C.
C.    F(X;XMU,XSIG) = EXP(-+Z-EXP(-+Z))/XSIG with Z=(X-XMU)/XSIG
C.
C.    with minus sign for the distribution of the maximum and plus sign 
C.    for the distribution of the minimum. For this routine put a minus
C.    sign on XSIG to flag the case for distribution of the minimum.
C.
C.    Variable:          X        (real)
C.    Parameter:         XMU      (real)
C.                       XSIG >0  (real)
C.
C.    Expectation value: E(X) = XMU+-XSIG*GAMMA = XMU+-XSIG*0.57722
C.    Variance:          V(X) = XSIG**2*PI**2/6
C.    Skewness:         G1(X) = 1.13955...
C.    Kurtosis:         G2(X) = 2.4
C.
C.----------------------------------------------------------------------
      RNEVD = XMU-XSIG*LOG(-LOG(RNUNIF(XMU)))
      RETURN
      END
