CDECK  ID>, RNRAYL.
      function rnrayl ( a )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNRAYL ( A )                            Ch.Walck    950508
C.
C.    Pseudorandom number from Rayleigh distribution
C.
C.                  x           x**2
C.         f(x) = ---- exp ( - ------ )
C.                a**2         2*a**2
C.
C.    Variable:          x >= 0 (real)
C.    Parameters:        a >  0 (real)
C.
C.    Expectation value: E(x) = a * sqrt(2/pi)
C.    Variance:          V(x) = a**2 * (2-pi/2)
C.    Skewness:         G1(x) = sqrt(pi/2)*(pi-3)/(2-pi/2)**1.5 = 0.6311
C.    Kurtosis:         G2(x) = (8-3*pi**2/4)/(2-pi/2)**2 - 3   = 0.2451
C.
C.    Technique: Making the transformation y = (x/a)**2/2 this variable 
C.    has the distribution g(y)=exp(-y) from which a pseudorandom number 
C.    is easily obtained by taking minus the natural logarithm of a 
C.    uniformly distributed pseudorandom number between zero and one.
C.
C.----------------------------------------------------------------------
      rnrayl = a*sqrt(-2.0*alog(rnunif(a)))
      return
      end
