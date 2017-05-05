CDECK  ID>, RNCCHY.
      FUNCTION RNCCHY ( X0, G )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNCCHY ( X0, G )                        Ch.Walck    860728
C.
C.    Pseudorandom number from Cauchy distribution with mode X0 and
C.    half-width at half-height G.
C.
C.    F(X;X0,G) = G / PI / (G**2+(X-X0)**2)
C.
C.    Variable:          X        (real)
C.    Parameters:        X0       (real)
C.                       G > 0    (real)
C.
C.    Expectation value: E(X) UNDEFINED
C.    Variance:          V(X) DIVERGENT
C.
C.----------------------------------------------------------------------
    1 U = 2.0*RNUNIF(X0) - 1.0
      V = 2.0*RNUNIF(G)  - 1.0
*     1-P1/4 = 21 % rejection below
      IF ( U**2+V**2 .GT. 1.0 ) GO TO 1
      RNCCHY = X0 + G * U / V
      RETURN
      END
