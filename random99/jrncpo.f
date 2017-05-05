CDECK  ID>, JRNCPO.
      SUBROUTINE JRNCPO ( NCUM, CUM, XLA, XMU )
C-----------------------------------------------------------------------
C
C...       SUBROUTINE JRNCPO ( NCUM, CUM, XLA, XMU )        Ch.Walck    860919
C.
C.    Prepare cumulative vector for compound Poisson distribution
C.    with parameters XLA and XMU so that pseudorandom numbers can
C.    be efficiently obtained with function IRNFNC.
C.
C.    CUM should be dimensioned to at least NCUM in the calling routine
C.
C.----------------------------------------------------------------------
      DIMENSION CUM(NCUM)
      REAL N, NOLD
      IR    = 0
      NBIN  = NCUM - 5
      XLNRF = 0.0
      NOLD  = 10000.0
      H1    = LOG(XLA) - XMU
C     N=0 term for IR=0
      SUM = EXP(-XLA)
    1 N = 1.0
C     LOG of N=1 term
      TS = IR*LOG(XMU) + H1 - XLA - XLNRF
      DS  = EXP(TS)
      SUM = SUM + DS
      OLD = SUM
      ODS = DS
C     LOG of N+1 term from N term by recursive formula
    2 TS = TS + IR*(LOG(N+1.0)-LOG(N)) + H1 - LOG(N+1.0)
      DS = EXP(TS)
      SUM = SUM + DS
      IF ( ( SUM.EQ.OLD .AND. DS.LT.ODS ) .OR.
     +     ( SUM.EQ.0.0 .AND. N.GT.NOLD ) ) GO TO 3
      OLD = SUM
      ODS = DS
      N = N + 1.0
      GO TO 2
    3 IR = IR + 1
      CUM(IR) = SUM
      IF ( IR.EQ.NBIN .OR. CUM(IR).EQ.0.0 ) GO TO 4
      NOLD = N
      SUM  = 0.0
      XLNRF = XLNRF + LOG(FLOAT(IR))
      GO TO 1
    4 CALL IRNHIS ( CUM, IR, 0, CUM )
      RETURN
      END
