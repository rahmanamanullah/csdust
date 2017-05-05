CDECK  ID>, RNGASY.
      function rngasy ( xmu, sl, sr )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNGASY ( XMU, SL, SR )                  Ch.Walck    031002
C.
C.    Generate random numbers from an asymmetric Gaussian distribution 
C.    with different standard deviations (sl and sr) to the left and 
C.    right of the mode xmu. Property normalized and joining nicely at 
C.    x=mu this implies
C.
C.                      2 sl/sr
C.                      -------  G(x;mu,sl)    if    x.le.mu
C.                      1+sl/sr 
C.    f(x;mu,sl,sr) = 
C.                         2
C.                      -------  G(x;mu,sr)    if    x.gt.mu
C.                      1+sl/sr
C.
C.    where G(x;mu,s) is a Gaussian dtribution with mean mu and s.d. s.
C.    This is not a fundamental distribution but is sometimes used in
C.    e.g. estimating asymmetric errors or as a simple parameterization.
C.
C.----------------------------------------------------------------------
*
*     Generate a standard normal random number (take the absolute value)
*
      g = abs ( rnnrmt ( 0.0, 1.0 ) )
*
*     ... also need a uniform to select which side of xmu
*
      u = rnunif ( dum )
*
*     Gasussian random numbers from left or right with correct proportions
*
      h = sl/sr
      if ( u .lt. h/(1.0+h) ) then
         r = xmu - sl*g
      else
         r = xmu + sr*g
      end if
*
      rngasy = r
      return
      end
