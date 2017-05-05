CDECK  ID>, RNRECA.
      SUBROUTINE RNRECA ( ISEQ )
*
*     Recall sequence ISEQ for random number generation as stored by
*     a previous call to RNSAVE.
*                                                    Ch.Walck 1989-12-21
      COMMON /RNCMMN/ LAG1, LAG2, U(97), C, CD, CM
      PARAMETER (NSEQ=10)
      COMMON /RNSEQC/ LAG1S(NSEQ), LAG2S(NSEQ), US(97,NSEQ), CS(NSEQ)
*
      IF ( ISEQ.LE.0 .OR. ISEQ.GT.NSEQ ) THEN
         WRITE (6,1001) ISEQ
 1001    FORMAT (/' RNRECA: error, sequence number out of range',I11)
         STOP
      END IF
*
      LAG1 = LAG1S(ISEQ)
      LAG2 = LAG2S(ISEQ)
      DO 100 I = 1, 97
         U(I) = US(I,ISEQ)
  100 CONTINUE
      C = CS(ISEQ)
      RETURN
      END
