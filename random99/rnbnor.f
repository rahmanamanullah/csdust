CDECK  ID>, RNBNOR.
      SUBROUTINE RNBNOR ( X1, S1, X2, S2, RHO, R1, R2 )
C-----------------------------------------------------------------------
C
C...       SUBROUTINE RNBNOR (X1,S1,X2,S2,RHO,R1,R2)        Ch.Walck    841105
C.
C.    Obtain in R1 and R2 two pseudorandom numbers from a binormal
C.    distribution with means X1 and X2, standard deviations S1 and S2
C.    and correlation coefficient RHO.
C.
C.----------------------------------------------------------------------
      PARAMETER (TWOPI=6.2831853072)
C
      Z = SQRT ( -2.0 * LOG(RNUNIF(TWOPI)) )
      ANGLE = TWOPI * RNUNIF(Z)
C
      Z2 = SIN(ANGLE)
      Z1 = SQRT ( 1.0 - RHO**2 ) * COS(ANGLE) + RHO * Z2
C
      R1 = X1 + S1 * Z * Z1
      R2 = X2 + S2 * Z * Z2
      RETURN
      END
