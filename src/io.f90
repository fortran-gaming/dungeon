      
! TXCRYP - Subroutine to encrypt/decrypt text strings.

! This subroutine performs a reversible encryption on a text string.
! The purpose is not to protect the data base but to make it more
! difficult for the casual game user to read the data base file.
! It is located here, rather than in the SUBRoutine module, because
! it is used by both the game and the separate data base compiler.

module io
    use,intrinsic:: iso_fortran_env,only: output_unit
    implicit none
contains

  SUBROUTINE TXCRYP(R,LINE)
    integer, intent(in) :: R
    CHARACTER(*),intent(inout) :: LINE
    integer i,j,x

    Do concurrent (I=1:LEN(LINE))
        X=IAND(R, 31)+I
        LINE(I:I)=CHAR(IEOR(ICHAR(LINE(I:I)), j))
    enddo

  END SUBROUTINE TXCRYP
    
! SAVE- Save game state
  SUBROUTINE SAVEGM
   use state
    integer u,i

    IF(SUBLNT == 0) SUBBUF='DSAVE.DAT'
    OPEN (newUNIT=u,FILE=SUBBUF,ACCESS='SEQUENTIAL',&
            STATUS='UNKNOWN',FORM='UNFORMATTED',ERR=100)

    CALL GTTIME(I)                        ! get time.
    WRITE(u) VMAJ,VMIN
    WRITE(u) WINNER,HERE,THFPOS,TELFLG,THFFLG,THFACT,SWDACT,SWDSTA,CPVEC
    WRITE(u) I,MOVES,DEATHS,RWSCOR,EGSCOR,MXLOAD,LTSHFT,BLOC,MUNGRM,HS,FROMDR,SCOLRM,SCOLAC
    WRITE(u) ODESC1,ODESC2,OFLAG1,OFLAG2,OFVAL,OTVAL,OSIZE,OCAPAC,OROOM,OADV,OCAN
    WRITE(u) RDESC1,RVAL,RFLAG,TRAVEL
    WRITE(u) AROOM,ASCORE,AVEHIC,ASTREN,AFLAG
    WRITE(u) FLAGS,SWITCH,VPROB,CFLAG,CTICK,CCNCEL

    CALL RSPEAK(597)
    CLOSE (u)
    RETURN

100 CALL RSPEAK(598)                  ! cant do it.

    END SUBROUTINE SAVEGM

! RESTORE- Restore game state

    SUBROUTINE RSTRGM
    use state
    integer :: u,i,j

    IF(SUBLNT==0) SUBBUF='DSAVE.DAT'
    OPEN (newUNIT=u,FILE=SUBBUF,ACCESS='SEQUENTIAL',STATUS='OLD',FORM='UNFORMATTED',ERR=100)

    READ(U) I,J
    IF((I.NE.VMAJ).OR.(J.NE.VMIN)) GO TO 200

    READ(U) WINNER,HERE,THFPOS,TELFLG,THFFLG,THFACT, SWDACT,SWDSTA,CPVEC
    READ(U) PLTIME,MOVES,DEATHS,RWSCOR,EGSCOR,MXLOAD,LTSHFT,BLOC,MUNGRM, HS,FROMDR,SCOLRM,SCOLAC
    READ(U) ODESC1,ODESC2,OFLAG1,OFLAG2,OFVAL,OTVAL, OSIZE,OCAPAC,OROOM,OADV,OCAN
    READ(U) RDESC1,RVAL,RFLAG,TRAVEL
    READ(U) AROOM,ASCORE,AVEHIC,ASTREN,AFLAG
    READ(U) FLAGS,SWITCH,VPROB,CFLAG,CTICK,CCNCEL

    CALL RSPEAK(599)
    CLOSE(U)
    RETURN

100 CALL RSPEAK(598)                  ! cant do it.
    RETURN

200 CALL RSPEAK(600)                  ! obsolete version
    CLOSE(U)

    END SUBROUTINE RSTRGM
    
! RSPEAK(MSGNUM) -- Output random message routine
    
      SUBROUTINE RSPEAK(N)
      integer, intent(in) :: n
      CALL RSPSB2(N,0,0)
      END SUBROUTINE RSPEAK

! RSPSUB(MSGNUM,SUBNUM) -- Output random message with substitutable argument
      SUBROUTINE RSPSUB(N,S1)
      integer, intent(in) :: n,s1
      CALL RSPSB2(N,S1,0)
      END SUBROUTINE RSPSUB

! RSPSB2(MSGNUM,S1,S2) -- Output random message with substitutable arguments

      SUBROUTINE RSPSB2(A,B,C)
      use state, only: texlnt,rtext,dbch,telflg
      integer, intent(in) :: a,b,c
      CHARACTER(TEXLNT) B1,B2
      integer x,y,z,i,j,newrec,oldrec

! Convert all arguments from dictionary numbers (if positive) to absolute record numbers.

      X=A                              ! set up work variables.
      Y=B
      Z=C
      IF(X > 0) X=RTEXT(X)                  ! if >0, look up in rtext.
      IF(Y > 0) Y=RTEXT(Y)
      IF(Z > 0) Z=RTEXT(Z)
      X=IABS(X)                        ! take abs value.
      Y=IABS(Y)
      Z=IABS(Z)
      IF(X == 0) RETURN                  ! anything to do?
      TELFLG=.TRUE.                        ! said something.

      READ(DBCH,REC=X) OLDREC,B1            ! read first line.
100   CALL TXCRYP(X,B1)                  ! decrypt line.

200   IF(Y==0) GO TO 400                  ! any substitutable?
      I=INDEX(B1,'#')                        ! look for #.
      IF(I>0) GO TO 1000                  ! found?

400   WRITE(output_unit,650) B1(1:MAX0(1,len_trim(B1)))! output line.
650   FORMAT(1X,A)
      X=X+1                              ! on to next record.
      READ(DBCH,REC=X) NEWREC,B1            ! read next record.
      IF(OLDREC.EQ.NEWREC) GO TO 100            ! continuation?
      RETURN                              ! no, exit.

! RSPSB2, PAGE 2

! Substitution with substitutable available.
! I is index of # in B1.
! Y is number of record to substitute.
!
! Procedure:
!   1) Copy rest of B1 to B2
!   2) Read substitutable over B1
!   3) Restore tail of original B1

! The implicit assumption here is that the substitutable string is very short.

1000  B2(1:(TEXLNT-I))=B1(I+1:TEXLNT)            ! copy rest of B1.

      READ(DBCH,REC=Y) J,B1(I:TEXLNT)            ! read sub record.
      CALL TXCRYP(Y,B1(I:TEXLNT))            ! decrypt sub record.
      J=len_trim(B1)                        ! backscan for blanks.
      B1(J+1:TEXLNT)=B2(1:TEXLNT-J)

      Y=Z                              ! set up for next
      Z=0                              ! substitution and
      GO TO 200                        ! recheck line.

   END SUBROUTINE RSPSB2
      
end module io
