CDECK  ID>, RMOYAL.
      FUNCTION RMOYAL ( XMU, XSIG )
C-----------------------------------------------------------------------
C
C...       FUNCTION RMOYAL ( XMU, XSIG )                    Ch.Walck    950429
C.
C.    Pseudorandom number from Moyal distribution
C.
C.                    1               1
C.         f(z) = ---------- exp ( - --- ( z + exp(-z) ) )
C.                sqrt(2*pi)          2
C.
C.    for the standardized variable z = ( x - xmu ) / xsig where xsig>0
C.    (i.e. f(x) = f(z)/xsig).
C.
C.    Variable:          x (real)
C.    Parameters:        xmu (real) and xsig>0 (real)
C.
C.    Expectation value: E(z) = ln 2 + gamma             = 1.27036...
C.    Variance:          V(z) = pi**2/2                  = 4.93480...
C.    Skewness:         G1(z) = 28*sqrt(2)*zeta(3)/pi**3 = 1.53514...
C.    Kurtosis:         G2(z) = 4                        = 4
C.
C.    For the original distribution f(x) use E(x) = E(z)*xsig + xmu, 
C.    V(x) = V(z)*xsig**2, G1(x) = G1(z), and G2(x) = G2(z). Above
C.    gamma is Euler's constant gamma=0.5772156649... and zeta denotes 
C.    Riemanns's zeta-function with zeta(3)=1.2020569031...
C.
C.    The Moyal distribution is a universal form for (a) the energy loss
C.    by ionization for a fast charged particle and (b) for the number 
C.    of ion pairs produced in this process. It was propsed by J.E.Moyal 
C.    in Phil. Mag. 46 (1955) 263 as a good approximation to the Landau 
C.    distribution. It was also shown that it remains valid taking into 
C.    account quantum resonance effects and details of atomic structure
C.    of the absorber.
C.
C.    Since the range of the variable x, or z, is from -infinity to 
C.    +infinity we make use of the transformation tan(y) = z with
C.
C.           f(y) = f(tan(y))/cos(y)**2
C.
C.    and hence y is limited between -pi/2 and pi/2. The maximum value 
C.    for f(y) in this range is somewhat smaller than 0.912 and we use
C.    a reject-accept (or hit-miss) technique in this box. If required
C.    surely a more effective generation could be achieved.
C.
C.    Using RNUNIF as the basic uniform pseudorandom number generator 
C.    we obtain the following timing results on some computers:
C.
C.    Node       Hardware                           us/call  MHz
C.
C.    vanqc74    Compaq Professional Works. XP1000    1.24   667 (unix)
C.    vanqc75    Compaq Professional Works. XP1000    1.65   500 (unix)
C.    ices02     AlphaServer 800 5/500                3.1    500 (unix)
C.    penguin    Digital PW 433au                     3.5    433 (unix)
C.    icelx01    PC - Pentium III 800 MHz             3.6    800 (linux)
C.    tmp3       PC - Compaq Proliant ML370, 800 MHz  3.6    800 (linux)
C.    ices01     AlphaServer 800 5/333                4.2    333 (unix)
C.    lambda     PC - Dell Dimension XPS R400         7.5    400 (linux)
C.    VANG       DEC 3000 model 900                   8.1    275
C.    omega      PC laptop - Euronote 740             8.7    333 (linux)
C.    VANZ06     Digital AlphaStation 255/233         9.3    233
C.    HAGZ03     Digital AlphaStation 200 4/233      10.6    233
C.    VANZ04     DEC 3000 model 700                  10.7    225
C.    HAGZ01     Digital AlphaStation 200 4/166      15.4    166
C.    VANZ       DEC 3000 model 500                  17.3    150
C.    VANG       DEC 3000 model 300LX                21.5    125
C.    VANZ11     DEC 3000 model 300L                 27.0    100
C.    VANA       VAX 4000-500                        39.5    400
C.    VANC       VAXstation 3200                    505
C.
C.
C.----------------------------------------------------------------------
      parameter (pi=3.141592653589793,twopi=2.0*pi,pihalf=pi/2.0
     +          ,ymax=0.912,cnst=0.398942280401433 )
*                           cnst = 1/sqrt(2*pi)
*
    1 y  = pi*rnunif(xsig) - pihalf
      f  = ymax * rnunif(xmu)
      x  = tan(y)
      if ( abs(x) .gt. 60.0 ) go to 1
*     fy = exp(-0.5*(x+exp(-x)))/cos(y)**2/sqrt(twopi)
      fy = cnst*exp(-0.5*(x+exp(-x)))/cos(y)**2
      if ( f .gt. fy ) go to 1
      rmoyal = x*xsig + xmu
      return
      end
