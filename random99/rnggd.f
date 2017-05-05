CDECK  ID>, RNGGD.
      function rnggd ( a, b, c )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNGGD ( A, B, C )                      Ch.Walck    961208
C.
C.    Pseudorandom number from generalized Gamma distribution (GGD)
C.
C.                               c
C.                     b-1  -(ax)
C.               a c (ax)  e
C.    f(x;a,b) = --------------
C.                  Gamma(b)
C.
C.    Variable:          X > 0     (real)
C.    Parameters:        A > 0     (real)
C.                       B > 0     (real)
C.                       C > 0     (real)
C.
C.                         '    1   Gamma(b+n/c)
C.    Algebraic moments: mu  = ---- ------------
C.                         n   a**n   Gamma(b)
C.
C.    Technique: The cumulative, distribution, function is given by
C.                           F(x) = P(b,(a*x)**c)
C.               and we use a routine for the inverse of the incomplete 
C.               Gamma function P, solving for x in F(x) = r where r is
C.               a uniform random number between zero and one.
C.
C.               This is sufficiently fast for most purposes but for
C.               extensive use of GGD random numbers one probably could
C.               gain substantially buy using a general technique.
C.
C.----------------------------------------------------------------------
      rn = 0.0
      xi = rnunif(a)
      pinv = paxinv ( b, xi )
      if ( pinv .ne. 0.0 ) rn = pinv**(1.0/c) / a
      rnggd = rn
      return
      end
