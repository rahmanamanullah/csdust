CDECK  ID>, DRN56.
      FUNCTION DRN56 ()
C-----------------------------------------------------------------------
C
C...       FUNCTION DRN56 ()                                Ch.Walck    870703
C.
C.    56-bit uniform random generator based on the multiplicative
C.    congruential method with the multiplier 11.133.510.565.745.309
C.    (closest integer to 2**54*Golden section ((SQRT(5)-1)/2) with
C.    modulus 8 = 5) and the start seed 1 by default. The period of the
C.    generator is 2**54 = 18.014.398.509.481.984. 56 bits are chosen
C.    since this is the capacity for the mantissa in a double precision
C.    word on VAX. Access and set seed (divided into two integer words)
C.    by CALL RN56IN(ISEED1,ISEED2) and CALL RN56UT(ISEED1,ISEED2).
C.    By default ISEED1=1 and ISEED2=0 at start. The minimum value
C.    returned is 2**-56 and the maximum value is 1-3*2**-56.
C.
C.    This routine use integer arithmetics, taking advantage of the IAND
C.    and ISHFT functions, and a call on a VAX-11/780 takes 74 microsec.
C.    Use R = SNGL(DRN56(DUM)) to obtain an effective single precision
C.    version.
C.
C.    The arithmetics involved causes integer overflows which is handled
C.    on a VAX by calling ERRSET once from the main program as follows:
C.       CALL ERRSET(70,.TRUE.,.FALSE.,.FALSE.,.FALSE.,100)
C.    or by compiling this routine with /CHECK=NOOVERFLOW
C.    or, as here, by the VAX FORTRAN OPTIONS statement before
C.       the routine.
C.    On most, if not all, other computers integer overflow is ignored
C.    by default.
C.
C.----------------------------------------------------------------------
      SAVE IDS1, IDS2
      PARAMETER (IDM1=241160861, IDM2=41475558, IDMA=4765, IDMB=14719,
     +           M14=16383, DP28=268435456.0D0, M28=268435455 )
      DOUBLE PRECISION DRN56
      DATA IDS1/1/, IDS2/0/
C
C     Multiply the 56-bit integers in (IDS2,IDS1) and (IDM2,IDM1)
C
      IDSA = IAND(IDS1,M14)
      IDSB = ISHFT(IDS1,-14)
      IDS2 = IAND ( ISHFT(ISHFT(IDSA*IDMA,-14)+IDSA*IDMB+IDSB*IDMA,-14)
     +       + IDSB*IDMB + IDS1*IDM2 + IDS2*IDM1, M28 )
      IDS1 = IAND(IDS1*IDM1,M28)
C
      DRN56 = ( DFLOAT(IDS2) + DFLOAT(IDS1)/DP28 ) / DP28
      RETURN
C
      ENTRY RN56UT ( ISEED1, ISEED2 )
C     ISEED1 is least significant 28 bits and ISEED2 most significant
C     28 bits in the 56-bit integer seed.
      ISEED1 = IDS1
      ISEED2 = IDS2
      RETURN
C
      ENTRY RN56IN ( ISEED1, ISEED2 )
      IDS1 = ISEED1
      IDS2 = ISEED2
      RETURN
      END
