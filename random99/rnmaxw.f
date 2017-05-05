CDECK  ID>, RNMAXW.
      function rnmaxw ( a )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNMAXW ( A )                            Ch.Walck    950508
C.
C.    Pseudorandom number from Maxwell distribution
C.
C.                        2   x**2          x**2
C.         f(x) = sqrt ( -- ) ---- exp ( - ------ )
C.                       pi   a**3         2*a**2
C.
C.    Variable:          x >= 0 (real)
C.    Parameters:        a >  0 (real)
C.
C.    Expectation value: E(x) = sqrt(8/pi)*a
C.    Variance:          V(x) = (3-8/pi)*a**2
C.    Skewness:         G1(x) = 2*sqrt(2/pi)*(16/pi-5)/(3-8/pi)**1.5
C.    Kurtosis:         G2(x) = (15-8/pi)/(2-8/pi)**2 - 3
C.
C.    Technique: Making the transformation y = (x/a)**2/2 this variable 
C.    has the distribution g(y)=sqrt(y)*exp(-y)/Gamma(3/2) which is a 
C.    Gamma distribution with parameters a=1 and b=1.5 from which we 
C.    obtain a pseudorandom number by Johnk's algorithm which then is
C.    transformed to a random number from the Maxwell distribution.
C.
C.----------------------------------------------------------------------
      r  = -log(rnunif(a))
    1 w1 = rnunif(r)**2
      w2 = rnunif(w1)**2
      w  = w1 + w2
      if ( w .gt. 1.0 ) go to 1
      r  = r - log(rnunif(w2))*w1/w
      rnmaxw = a*sqrt(2.0*r)
      return
      end
