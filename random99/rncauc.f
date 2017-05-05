CDECK  ID>, RNCAUC.
      FUNCTION RNCAUC ( X0, G )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNCAUC ( X0, G )                        Ch.Walck    860911
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
C
C     The old version
C        PARAMETER (PI=3.1415926536)
C        RNCHAU = X0 + G * TAN(PI*(RNUNIF(G)-0.5))
C     is faster but gives strange machine code on VAX?!
C
      PARAMETER (TWOPI=6.2831853072)
      RNCAUC = X0 + G * TAN(TWOPI*RNUNIF(G))
      RETURN
      END
