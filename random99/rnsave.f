CDECK  ID>, RNSAVE.
      SUBROUTINE RNSAVE ( ISEQ )
*
*     Save current seeds for random number generator as sequence ISEQ.
*     A maximum of NSEQ sequences are allowed. Use RNRECA to recall
*     a certain sequence.
*                                                    Ch.Walck 1989-12-21
      COMMON /RNCMMN/ LAG1, LAG2, U(97), C, CD, CM
      PARAMETER (NSEQ=10)
      COMMON /RNSEQC/ LAG1S(NSEQ), LAG2S(NSEQ), US(97,NSEQ), CS(NSEQ)
*
      IF ( ISEQ.LE.0 .OR. ISEQ.GT.NSEQ ) THEN
         WRITE (6,1001) ISEQ
 1001    FORMAT (/' RNSAVE: error, sequence number out of range',I11)
         STOP
      END IF
*
      LAG1S(ISEQ) = LAG1
      LAG2S(ISEQ) = LAG2
      DO 100 I = 1, 97
         US(I,ISEQ) = U(I)
  100 CONTINUE
      CS(ISEQ) = C
      RETURN
      END
