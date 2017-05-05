CDECK  ID>, RNGAUS.
      FUNCTION RNGAUS ( XMU, XSIG )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNGAUS ( X, S )                         CH.WALCK    891231
C.
C.    Generate normal (N(X,S)) random numbers using the "NORRAN"-method.
C.
C.                   1               1   z - x  2
C.    f(z;x,s) = ----------- exp ( - - ( ----- )  )
C.               s sqrt(2pi)         2     s
C.
C.    Variable:          Z     (real)
C.    Parameters:        X     (real)
C.                       S > 0 (real)
C.
C.    Expectation value: E(Z) = X
C.    Variance:          V(Z) = S**2
C.    Skewness:         G1(Z) = 0
C.    Kurtosis:         G2(Z) = 0
C.
C.    This code is "stolen" from the CERN library routine NORRAN (entry
C.    V101). The method is similar to the histogram method used in
C.    RNNRMH. It uses, however, some tricks to be more efficient. Two of
C.    the most efficient tricks of NORRAN is not implemented (a) inline
C.    code for the generation of uniform random numbers and (b) double
C.    use of one random number for two purposes. To be more specific on
C.    (b) NORRAN uses the first bits of a 32-bit integer from the random
C.    sequence for selecting which part of the code to be used and then
C.    the remaining bits as the base of a random number. The method uses
C.    bins of size 1/16 between -2.75 and 2.75 (in standard normal
C.    variable) which are treated with a combination of a histogram,
C.    triangle and a reject-accept method plus an ordinary tail method.
C.
C.----------------------------------------------------------------------
      SAVE C, TBL
      DIMENSION C(45), TBL(0:326), RN(4)
      EQUIVALENCE (R1,RN(1)), (R2,RN(2)), (R3,RN(3)), (R4,RN(4))
      DATA C / 0.9889430404, 0.9889430404, 0.9791515470, 0.9595685005
     +       , 0.9497770071, 0.9301939607, 0.9008194208, 0.8812363744
     +       , 0.8518618345, 0.8224872947, 0.7833212614, 0.7539466619
     +       , 0.7147806287, 0.6756145358, 0.6364485025, 0.5972824097
     +       , 0.5679078698, 0.5287418365, 0.4895757437, 0.4504097104
     +       , 0.4210351706, 0.3818690777, 0.3524945378, 0.3231199980
     +       , 0.2937454581, 0.2643709183, 0.2349963784, 0.2154133320
     +       , 0.1860387921, 0.1664557457, 0.1468726993, 0.1272896528
     +       , 0.1174981594, 0.0979151130, 0.0881236196, 0.0783321261
     +       , 0.0685405731, 0.0587490946, 0.0489575788, 0.0391660631
     +       , 0.0293745473, 0.0293745473, 0.0195830315, 0.0195830315
     +       , 0.0195830315 /
      DATA TBL/   0.0000,   0.0625, 2*0.1250, 4*0.1875, 5*0.2500
     +        ,   0.5625, 5*0.6250, 3*0.8750,   1.1250,   1.4375
     +        , 5*0.0000, 5*0.0625, 4*0.1250, 2*0.1875,   0.2500
     +        , 5*0.3125, 5*0.3750, 5*0.4375, 5*0.5000, 4*0.5625
     +        , 4*0.6875, 4*0.7500, 4*0.8125,   0.8750, 3*0.9375
     +        , 3*1.0000, 3*1.0625, 2*1.1250, 2*1.1875, 2*1.2500
     +        , 2*1.3125, 2*1.3750,   1.4375,   1.5000,   1.5625
     +        ,   1.6250,   1.6875,   1.7500,   1.8125,10*0.3125
     +        , 7*0.3750, 5*0.4375, 2*0.5000, 9*0.6875, 5*0.7500
     +        ,   0.8125,10*0.9375, 7*1.0000, 3*1.0625,12*1.1875
     +        , 9*1.2500, 5*1.3125, 2*1.3750,13*1.5000,10*1.5625
     +        , 7*1.6250, 5*1.6875, 2*1.7500,15*1.8750,13*1.9375
     +        ,12*2.0000,10*2.0625, 9*2.1250, 8*2.1875, 7*2.2500
     +        , 6*2.3125, 5*2.3750, 4*2.4375, 3*2.5000, 3*2.5625
     +        , 2*2.6250, 2*2.6875 /
*
      IUNI = 4096.0 * 4096.0 * RNUNIF(DUM)
      IR   = IUNI / 65536
*
*     Part 1: From histogram with 30 bins between 0 and 1.875
*
      IF ( IR .LE. 103 ) THEN
         R = RNUNIF(DUM)/16.0 + TBL(IR)
*
*     Part 2: From histogram with 30 bins between -1.875 and 0
*
      ELSE IF ( IR .LE. 207 ) THEN
         R = - ( RNUNIF(DUM)/16.0 + TBL(IR-104) )
*
*     Part 3: From histogram with 44 bins between 0 and 2.75
*
      ELSE IF ( IUNI .LE. 14872575 ) THEN
         IR = IUNI / 4096
         R  = RNUNIF(DUM)/16.0 + TBL(IR-3304)
*
*     Part 4: From histogram with 44 bins between -2.75 and 0
*
      ELSE IF ( IUNI .LE. 16113663 ) THEN
         IR = IUNI / 4096
         R  = - ( RNUNIF(DUM)/16.0 + TBL(IR-3607) )
*
*     Part 5: From triangles in 88 bins between -2.75 and 2.75
*
      ELSE IF ( IUNI .LE. 16499539 ) THEN
         CALL RNUNIV ( RN, 4 )
         B = AINT ( 7.0*(R1+R2) + 37.0*ABS(R1-R2) )
         X = R3 - R4
         R = 0.0625 * ( X + SIGN(B,X) )
*
*     Part 6: Reject-accept method for remaining part between standard
*             normal curve in the region -2.75 and 2.75 and the histogram
*             plus triangles.
*
      ELSE IF ( IUNI .LE. 16677232 ) THEN
    1    R = 5.5 * RNUNIF(DUM) - 2.75
         J = 16.0 * ABS(R) + 1.0
         IF ( J .LE. 14 ) THEN
            P =  FLOAT(2*J-1) * 0.001497466
         ELSE
            P = FLOAT(89-2*J) * 0.000698817
         END IF
         IF ( RNUNIF(DUM) .GT.
     +        79.78846*(EXP(-0.5*R**2)-C(J)-P*(J-16.0*ABS(R))) ) GO TO 1
*
*     Part 7: From tail outside +-2.75
*
      ELSE
    2    V = 2.0*RNUNIF(DUM) - 1.0
         IF ( V .EQ. 0.0 ) GO TO 2
         X = SQRT ( 7.5625 - 2.0*LOG(ABS(V)) )
         IF ( RNUNIF(DUM)*X .GT. 2.75 ) GO TO 2
         R = SIGN ( X, V )
      END IF
      RNGAUS = R * XSIG + XMU
      RETURN
      END
