CDECK  ID>, DRN64.
      FUNCTION DRN64 ()
C-----------------------------------------------------------------------
C
C...       FUNCTION DRN64 ()                                Ch.Walck    870701
C.
C.    64-bit uniform random generator based on the multiplicative
C.    congruential method with the multiplier 2.850.178.704.830.799.621
C.    (closest integer to 2**62*Golden section ((SQRT(5)-1)/2) with
C.    modulus 8 = 5) and the start seed 1 by default. The period of the
C.    generator is 2**62 = 4.61*10**18. Access and set seed (divided
C.    into two integer words) by CALL RN64IN(ISEED1,ISEED2) and
C.    CALL RN64UT(ISEED1,ISEED2). By default ISEED1=1 and ISEED2=0 at
C.    start. The minimum value returned is 2**-64 and the maximum
C.    value is 1.0.
C.
C.    This routine use integer arithmetics, taking advantage of the IAND
C.    and ISHFT functions, and a call on a VAX-11/780 takes 95 microsec.
C.    Use R = SNGL(DRN64(DUM)) to obtain an effective single precision
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
      PARAMETER (IDM1=1607638789, IDM2=663608942, IDMA=40709,
     +           IDMB=24530, M16=65535, DM16=2.0D0**(-16) )
      DOUBLE PRECISION DRN64
      DATA IDS1/1/, IDS2/0/
C
C     Multiply the 64-bit integers in (IDS2,IDS1) and (IDM2,IDM1)
C
      IDSA = IAND(IDS1,M16)
      IDSB = ISHFT(IDS1,-16)
      IDS2 = ISHFT(ISHFT(IDSA*IDMA,-16)+IDSA*IDMB+IDSB*IDMA,-16)
     +       + IDSB*IDMB + IDS1*IDM2 + IDS2*IDM1
      IDS1 = IDS1*IDM1
C
C     Stmt as in DRN56 above does not work because IDS1 and IDS2 are
C     32-bit integers!
C     I.e. not DRN64 = ( DFLOAT(IDS2) + DFLOAT(IDS1)/2**32 ) / 2**32 but
C     DRN64 = ( A + ( B + ( C + D/2**16 ) / 2**16 ) / 2**16 ) / 2**16
C     where A, B, C and D are 16-bit parts of the 64-bit number ABCD.
C
      DRN64 = ( DFLOAT(ISHFT(IDS2,-16)) + ( DFLOAT(IAND(IDS2,M16))
     +      + ( DFLOAT(ISHFT(IDS1,-16)) + DFLOAT(IAND(IDS1,M16))*DM16 )
     +        * DM16 ) * DM16 ) * DM16
      RETURN
C
      ENTRY RN64UT ( ISEED1, ISEED2 )
C     ISEED1 is least significant 32 bits and ISEED2 most significant
C     32 bits in the 64-bit integer seed.
      ISEED1 = IDS1
      ISEED2 = IDS2
      RETURN
C
      ENTRY RN64IN ( ISEED1, ISEED2 )
      IDS1 = ISEED1
      IDS2 = ISEED2
      RETURN
      END
