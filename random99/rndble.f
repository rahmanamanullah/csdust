CDECK  ID>, RNDBLE.
      DOUBLE PRECISION FUNCTION RNDBLE ( DUMMY )
*
*     Return double precision pseudorandom numbers from a uniform
*     distribution. The routine merges two 24-bit pseudorandom
*     numbers using the Marsaglia-Zaman algorithm and thus only
*     48 bits are used in the mantissa although generally more
*     bits are available (56 on VAX). The smallest number returned
*     is 2**(-48) and the largest 1-2**(-48).
*
*     For some timing results see table in the next patch (RNQDBL).
*
*                                                    Ch.Walck 1992-03-03
*
      COMMON /RNCMMN/ LAG1, LAG2, U(97), C, CD, CM
      DOUBLE PRECISION RR
*
*     Floating point arithmetics version
*
      DIMENSION RN(2)
    1 DO 100 I = 1, 2
         UNI = U(LAG1) - U(LAG2)
         IF ( UNI .LT. 0.0 ) UNI = UNI + 1.0
         U(LAG1) = UNI
         LAG1 = LAG1 - 1
         IF ( LAG1 .EQ. 0 ) LAG1 = 97
         LAG2 = LAG2 - 1
         IF ( LAG2 .EQ. 0 ) LAG2 = 97
         C = C - CD
         IF ( C .LT. 0.0 ) C = C + CM
         UNI = UNI - C
         IF ( UNI .LT. 0.0 ) UNI = UNI + 1.0
         RN(I) = UNI
  100 CONTINUE
      RR = RN(1) / 16777216.0D0 + RN(2)
*     Avoid returning zero
      IF ( RR .EQ. 0.0D0 ) GO TO 1
      RNDBLE = RR
      RETURN
      END
