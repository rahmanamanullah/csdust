CDECK  ID>, IRNHGM.
      FUNCTION IRNHGM ( NPICK, NTOT, NTYPE )
C-----------------------------------------------------------------------
C
C...       FUNCTION IRNHGM ( NPICK, NTOT, NTYPE )           Ch.Walck    860804
C.
C.    Generate a pseudorandom number from a hypergeometric
C.    distribution.
C.
C.                             NTYPE   NTOT-NTYPE     NTOT
C.    P(R;NPICK,NTOT,NTYPE) = (     ) (          ) / (     )
C.                             NPICK    NPICK-R       NPICK
C.
C.    NPICK elements are to be picked (without replacement) out of NTOT
C.    element of which NTYPE is of a requested type. The distribution in
C.    the number of elements of the requested type picked is the hyper-
C.    geometric distribution.
C.
C.    Variable:          R, RMIN <= R <= RMAX           (integer)
C.                  with RMIN = MAX(0,NPICK-NTOT+NTYPE)
C.                   and RMAX = MIN(NPICK,NTYPE)
C.    Parameters:        NPICK, 1 <= NPICK <= NTOT      (integer)
C.                       NTYPE >= 1                     (integer)
C.                       NTOT >= 1                      (integer)
C.
C.    Expectation value: E(R) = NPICK*NTYPE/NTOT
C.    Variance:          V(R) = NPICK*NTYPE*(NTOT-NTYPE)*(NTOT-NPICK)/
C.                              (NTOT-1)/NTOT**2
C.
C.----------------------------------------------------------------------
C
C        Initialize counters:
C        IR    = No of elements picked of requested type
C        MTYPE = No of remaining elements of requested type
C        MTOT  = No of remaining elements totally
C        MPICK = No of picked elements
C
      IR    = 0
      MTYPE = NTYPE
      MTOT  = NTOT
      MPICK = 0
C
C        Select one element out of the MTOT remaining
C
    1 K = IRNUNI(1,MTOT)
C
C        If selected element is of requested type increment IR and
C        decrement MTYPE.
C
      IF ( K .LE. MTYPE ) THEN
         IR    = IR    + 1
         MTYPE = MTYPE - 1
      END IF
C
C        Always decrement MTOT and increment MPICK
C
      MTOT  = MTOT  - 1
      MPICK = MPICK + 1
C
C        Finish if all elements of requested type finished or
C        if all NPICK elements already picked.
C
      IF ( MTYPE.GT.0 .AND. MPICK.NE.NPICK ) GO TO 1
C
      IRNHGM = IR
      RETURN
      END
