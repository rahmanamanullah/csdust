CDECK  ID>, RNINIT.
      SUBROUTINE RNINIT ( ISEED )
*
*     Initialize random number generator by Marsaglia & Zaman (& Tsang).
*
*     The initial values for U(97) are generated by using a 3-lag
*     Fibonacci generator with 3 initial seeds and a congruential
*     generator with one initial seed.
*
*     - The values of the three initial seeds for the 3-lag Fibonacci
*       generator may be in the range 1 to 178. This generator has a
*       period of 32044 in about half of all cases. In about a quarter
*       of all cases each the period is 16022 and 8011. In a few cases
*       with combinations like 1, 1, 178 or 178, 1, 178 the period is
*       only 1, 2 or 4. The authors proposes to avoid the combination
*       1, 1, 1 but we choose to be somewhat more restrictive and use
*       only values from 2 to 177 for the three seeds.
*     - The initial seed for the congruential generator may be in the
*       range 0 to 168. The period of this generator is 169 regardless
*       of the initial seed value.
*
*     The four initial seeds are packed into one integer which can be
*     regarded as a four digit number with a variable base for each
*     digit. This in order to get a one-to-one correspondence between
*     the four seeds and the big integer without leaving any holes.
*     With I, J, K being the seeds of the Fibonacci generator and L the
*     seed of the congruential generator we pack them as:
*        ISEED = (I-2)*176*176*169 + (J-2)*176*169 + (K-2)*169 + L
*     ISEED can thus have the range 0 to 169*176**3-1=921350143 all
*     values giving a legal and unique I, J, K, L combination.
*
*                                                    Ch.Walck 1989-12-21
*
      COMMON /RNCMMN/ LAG1, LAG2, U(97), C, CD, CM
*
      LAG1 = 97
      LAG2 = 33
*
*     Abort run if ISEED value is out of its valid range
*
      IF ( ISEED.LT.0 .OR. ISEED.GT.921350143 ) THEN
         WRITE (6,1001) ISEED
 1001    FORMAT (//' RNINIT: error - ISEED out of range, ISEED=',I11
     +          ,'), run aborted.'//)
         STOP
      END IF
*
*     Unpack seeds for 3-lag Fibonacci and congruential generator
*
      I = MOD(ISEED/169/176/176,176) + 2
      J = MOD(ISEED/169/176,176) + 2
      K = MOD(ISEED/169,176) + 2
      L = MOD(ISEED,169)
*
*     Floating point arithmetics version
*
      DO 200 II = 1, 97
         S = 0.0
         T = 0.5
         DO 100 JJ = 1, 24
            M = MOD(MOD(I*J,179)*K,179)
            I = J
            J = K
            K = M
            L = MOD(53*L+1,169)
            IF ( MOD(L*M,64) .GE. 32 ) S = S + T
            T = 0.5 * T
  100    CONTINUE
         U(II) = S
  200 CONTINUE
      C  =   362436.0 / 16777216.0
      CD =  7654321.0 / 16777216.0
      CM = 16777213.0 / 16777216.0
      RETURN
      END
