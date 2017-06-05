C Subroutines for DUNGEON
C
C COPYRIGHT 1980, 1990, INFOCOM COMPUTERS AND COMMUNICATIONS, CAMBRIDGE MA.
C ALL RIGHTS RESERVED, COMMERCIAL USAGE STRICTLY PROHIBITED
C WRITTEN BY R. M. SUPNIK
C
C 02-Dec-15     EMG     Compile using gfortran
C 27-Sep-94      RMS      Fixed bugs in WEIGHR, JIGSUP, SCORE.
C                  Added GRANITE WALL to GHERE.
C 30-Jan-94      RMS      Fixed bugs from MS DOS port.
C 30-Jun-92      RMS      Changed file names to lower case.
C 29-Jun-92      RMS      Removed extraneous declaration in RMDESC.
C
C RSPEAK-- Output random message routine
C
C Called by--
C
C      CALL RSPEAK(MSGNUM)
      module subr
      use state
      use io
      use,intrinsic:: iso_fortran_env, only: input_unit,output_unit
      implicit none
      
      public:: rnd,rspeak,qhere,newsta,qempty
      contains

      SUBROUTINE RSPEAK(N)
      integer, intent(in) :: n

      CALL RSPSB2(N,0,0)
      END SUBROUTINE RSPEAK
C
C RSPSUB-- Output random message with substitutable argument
C
C Called by--
C
C      CALL RSPSUB(MSGNUM,SUBNUM)
C
      SUBROUTINE RSPSUB(N,S1)

      integer, intent(in) :: n,s1
C
      CALL RSPSB2(N,S1,0)

      END SUBROUTINE RSPSUB

C RSPSB2-- Output random message with substitutable arguments
C
C Called by--
C
C      CALL RSPSB2(MSGNUM,S1,S2)
C
      SUBROUTINE RSPSB2(A,B,C)
      use state!, only: texlnt

      integer, intent(in) :: a,b,c

      CHARACTER(TEXLNT) B1,B2

      integer x,y,z,i,j,newrec,oldrec

C Convert all arguments from dictionary numbers (if positive)
c to absolute record numbers.
C
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
C
      READ(DBCH,REC=X) OLDREC,B1            ! read first line.
100   CALL TXCRYP(X,B1)                  ! decrypt line.
C
200   IF(Y.EQ.0) GO TO 400                  ! any substitutable?
      I=INDEX(B1,'#')                        ! look for #.
      IF(I>0) GO TO 1000                  ! found?
C
400   WRITE(output_unit,650) B1(1:MAX0(1,len_trim(B1)))! output line.
650   FORMAT(1X,A)
      X=X+1                              ! on to next record.
      READ(DBCH,REC=X) NEWREC,B1            ! read next record.
      IF(OLDREC.EQ.NEWREC) GO TO 100            ! continuation?
      RETURN                              ! no, exit.

C RSPSB2, PAGE 2
C
C Substitution with substitutable available.
C I is index of # in B1.
C Y is number of record to substitute.
C
C Procedure:
C   1) Copy rest of B1 to B2
C   2) Read substitutable over B1
C   3) Restore tail of original B1
C
C The implicit assumption here is that the substitutable string
c is very short.
C
1000      B2(1:(TEXLNT-I))=B1(I+1:TEXLNT)            ! copy rest of B1.
C
      READ(DBCH,REC=Y) J,B1(I:TEXLNT)            ! read sub record.
      CALL TXCRYP(Y,B1(I:TEXLNT))            ! decrypt sub record.
      J=len_trim(B1)                        ! backscan for blanks.
      B1(J+1:TEXLNT)=B2(1:TEXLNT-J)
C
      Y=Z                              ! set up for next
      Z=0                              ! substitution and
      GO TO 200                        ! recheck line.

      END SUBROUTINE RSPSB2
 
C OBJACT-- Apply objects from parse vector
C
C Declarations
C
      LOGICAL FUNCTION OBJACT()
       use state!, only: prsi,prso
      use objapp,only: oappli

C
      OBJACT=.TRUE.                        ! assume wins.
      IF(PRSI.EQ.0) GO TO 100                  ! ind object?
      IF(OAPPLI(OACTIO(PRSI),0)) RETURN      ! yes, let it handle.
C
100      IF(PRSO.EQ.0) GO TO 200                  ! dir object?
      IF(OAPPLI(OACTIO(PRSO),0)) RETURN      ! yes, let it handle.
C
200      OBJACT=.FALSE.                        ! loses.

      END FUNCTION OBJACT

C BUG-- Report fatal system error
C
C Declarations
C
      SUBROUTINE BUG(A,B)
      use state
      use verbs,only: savegm
      integer, intent(in) :: A,B

      WRITE(output_unit,100) A,B                  ! gonzo
      IF(DBGFLG/=0) RETURN
      SUBBUF='CRASH.DAT'                  ! set up crash save name.
      SUBLNT=len_trim(SUBBUF)
      CALL SAVEGM                        ! do final save.
      WRITE(output_unit,200)
      error stop ' abnormal termination'
C
100      FORMAT(' Program error ',I2,', parameter =',I6)
200      FORMAT(' Game state saved in "CRASH.DAT".')
C
      END subroutine BUG

C NEWSTA-- Set new status for object
C
C Called by--
C
C      CALL NEWSTA(OBJECT,STRING,NEWROOM,NEWCON,NEWADV)
C
      SUBROUTINE NEWSTA(O,R,RM,CN,AD)
      use state, only: oroom,ocan,oadv
      integer, intent(in) :: o,r, rm, cn, ad

      CALL RSPEAK(R)
      OROOM(O)=RM
      OCAN(O)=CN
      OADV(O)=AD

      END SUBROUTINE NEWSTA

C QHERE-- Test for object in room

      pure LOGICAL FUNCTION QHERE(OBJ,RM)
      use state, only: oroom,r2lnt,r2,o2
      integer, intent(IN) :: OBJ,rm
    
      integer i
      QHERE=.TRUE.
      IF(OROOM(OBJ).EQ.RM) RETURN            ! in room?
      DO I=1,R2LNT                  ! no, sch room2.
        IF((O2(I).EQ.OBJ).AND.(R2(I).EQ.RM)) RETURN
      enddo
      QHERE=.FALSE.                        ! not present.

      END FUNCTION QHERE

C QEMPTY-- Test for object empty

      LOGICAL FUNCTION QEMPTY(OBJ)
      use state, only: ocan,olnt
      integer, intent(in) :: obj

      integer i

      QEMPTY=.FALSE.                        ! assume lose.
      DO I=1,OLNT
        IF(OCAN(I).EQ.OBJ) RETURN            ! inside target?
      enddo
      QEMPTY=.TRUE.
 
      END FUNCTION QEMPTY

C JIGSUP- You are dead
C
C Declarations
C
      SUBROUTINE JIGSUP(DESC)
       use state!,only: KITCH,CLEAR,FORE3,FORE2,SHOUS,FORE2,KITCH,EHOUS,

      integer, intent(in) :: desc

      integer i,j,nonofl
      LOGICAL F
      INTEGER,parameter :: RLIST(*)=
     & [KITCH,CLEAR,FORE3,FORE2,SHOUS,FORE2,KITCH,EHOUS]

      CALL RSPEAK(DESC)                  ! describe sad state.
      PRSCON=0                        ! stop parser.
      IF(DBGFLG.NE.0) RETURN                  ! if dbg, exit.
      AVEHIC(WINNER)=0                  ! get rid of vehicle.
      IF(WINNER.EQ.PLAYER) GO TO 10            ! himself?
      CALL RSPSUB(432,ODESC2(AOBJ(WINNER)))      ! no, say who died.
      CALL NEWSTA(AOBJ(WINNER),0,0,0,0)      ! send object to hyper space.
      AROOM(WINNER)=0                        ! send actor to hyper space.
      RETURN
C
10      CALL SCRUPD(-10)                  ! charge 10 points.
      IF(ENDGMF) GO TO 900                  ! no recovery in end game.
      IF(DEATHS.GE.2) GO TO 1000            ! dead twice? kick him off.
      DEATHS=DEATHS+1                        ! record deaths.
      DEADF=.TRUE.                        ! flag dead player.
      I=8                              ! normal message.
      IF(LLDF) I=1074                        ! ghosts exorcised?
      CALL RSPEAK(I)                        ! tell him bad news.
      AACTIO(PLAYER)=PLAYER                  ! turn on dead player func.
C
      DO J=1,OLNT                        ! turn off fighting.
        IF(QHERE(J,HERE)) OFLAG2(J)=IAND(OFLAG2(J), NOT(FITEBT))
      enddo
C
      F=MOVETO(LLD1,WINNER)                  ! reposition him.
      EGYPTF=.TRUE.                        ! restore coffin.
      IF(OADV(COFFI).EQ.WINNER) CALL NEWSTA(COFFI,0,EGYPT,0,0)
      OFLAG2(DOOR)=IAND(OFLAG2(DOOR), NOT(TCHBT)) ! restore door.
      OFLAG1(ROBOT)=IAND(IOR(OFLAG1(ROBOT), VISIBT), NOT(NDSCBT))
      CALL NEWSTA(LAMP,0,LROOM,0,0)            ! lamp to living room,
      OFLAG1(LAMP)=IOR(OFLAG1(LAMP), VISIBT)      ! visible
      DO 100 I=1,CLNT                        ! disable cevnts if needed.
        IF(CCNCEL(I)) CFLAG(I)=.FALSE.
100      CONTINUE

C JIGSUP, PAGE 2
C
C Now redistribute his valuables and other belongings.
C
C The lamp has been placed in the living room.
C The first 8 non-valuables are placed in locations around the house.
C His valuables are placed starting at Troll Room.
C Remaining non-valuables are after that.
C
      I=0
      DO 200 J=1,OLNT                        ! loop thru objects.
        IF((OADV(J).NE.WINNER).OR.(OTVAL(J).NE.0))
     &      GO TO 200                  ! get his non-val objs.
        I=I+1
        IF(I.GT.8) GO TO 400                  ! move to random locations.
        CALL NEWSTA(J,0,RLIST(I),0,0)
200      CONTINUE
C
400      I=MTROL                              ! now move valuables.
      NONOFL=RAIR+RWATER+REND                  ! dont move here.
      DO 300 J=1,OLNT
        IF((OADV(J).NE.WINNER).OR.(OTVAL(J).EQ.0))
     &      GO TO 300                  ! on adv and valuable?
250        I=I+1                              ! find next room.
        IF(IAND(RFLAG(I), NONOFL).NE.0) GO TO 250      ! skip if nono.
        CALL NEWSTA(J,0,I,0,0)            ! yes, move.
300      CONTINUE
C
      DO 500 J=1,OLNT                        ! now get rid of remainder.
        IF(OADV(J).NE.WINNER) GO TO 500
450        I=I+1                              ! find next room.
        IF(IAND(RFLAG(I), NONOFL).NE.0) GO TO 450      ! skip if nono.
        CALL NEWSTA(J,0,I,0,0)
500      CONTINUE
      RETURN
C
C Cant or wont continue, clean up and exit.
C
900      CALL RSPEAK(625)                  ! in endgame, lose.
      GO TO 1100
C
1000      CALL RSPEAK(7)                        ! involuntary exit.
1100      CALL SCORE(.FALSE.)                  ! tell score.
      error stop

      END SUBROUTINE JIGSUP

C OACTOR-      Get actor associated with object
C
C Declarations
C
      INTEGER FUNCTION OACTOR(OBJ)
      use state,only: aobj,alnt
      integer, intent(in) :: obj

      DO OACTOR=1,ALNT                  ! loop thru actors.
        IF(AOBJ(OACTOR)==OBJ) RETURN      ! found it?
      enddo

      CALL BUG(40,OBJ)                  ! no, die.

      END FUNCTION OACTOR

C PROB-            Compute probability
C
C Declarations
C
      LOGICAL FUNCTION PROB(G,B)
      use state,only: badlkf

      integer, intent(in) :: G,B

        integer i

      I=G                              ! assume good luck.
      IF(BADLKF) I=B                        ! if bad, too bad.
      PROB=RND(100)<I                  ! compute.

      END function prob

C RMDESC-- Print room description
C
C RMDESC prints a description of the current room.
C It is also the processor for verbs 'LOOK' and 'EXAMINE'
C when there is no direct object.
C
      LOGICAL FUNCTION RMDESC(FULL)
      use state
      use rooms,only: rappli
      integer, intent(in) :: full

      integer i,ra
C FULL=      0/1/2/3=      full/obj/room/full but no applicable


      RMDESC=.TRUE.                        ! assume wins.
      RA=RACTIO(HERE)                        ! get room action.
      IF(PRSO.LT.XMIN) GO TO 50            ! if direction,
      FROMDR=PRSO                        ! save and
      PRSO=0                              ! clear.
50      IF(FULL.EQ.1) GO TO 600                  ! objects only?
      IF(HERE.EQ.AROOM(PLAYER)) GO TO 100      ! player just move?
      CALL RSPEAK(2)                        ! no, just say done.
      PRSA=WALKIW                        ! set up walk in action.
      RETURN
C
100      IF(LIT(HERE)) GO TO 300                  ! lit?
      CALL RSPEAK(430)                  ! warn of grue.
      RMDESC=.FALSE.
      RETURN
C
300      I=RDESC2-HERE                        ! assume short desc.
      IF((FULL.EQ.0)
     &     .AND. (SUPERF.OR.((IAND(RFLAG(HERE), RSEEN).NE.0)
     &     .AND. (BRIEFF.OR.PROB(80,80))))) GO TO 400
      I=RDESC1(HERE)                        ! use long.
      IF((I.NE.0).OR.(RA.EQ.0)) GO TO 400      ! if got desc, skip.
      PRSA=LOOKW                        ! pretend look around.
      PRSO=0                              ! no object referenced.
      CALL RAPPLI(RA)                        ! let room handle.
      PRSA=FOOW                        ! nop parser.
      GO TO 500
C
400      CALL RSPEAK(I)                        ! output description.
500      IF(AVEHIC(WINNER) /= 0) CALL RSPSUB(431,ODESC2(AVEHIC(WINNER)))
      RFLAG(HERE)=IOR(RFLAG(HERE), RSEEN)      ! indicate room seen.
C
600      IF(LIT(HERE)) GO TO 700                  ! if lit, do objects
      CALL RSPEAK(1036)                  ! can't see anything
      RETURN
C
700      IF(FULL.NE.2) CALL PRINCR(FULL,HERE)      ! print room contents
      IF((FULL.NE.0).OR.(RA.EQ.0)) RETURN      ! anything more?
      PRSA=WALKIW                        ! give him a surpise.
      CALL RAPPLI(RA)                        ! let room handle
      PRSA=FOOW

      END

C PRINCR- Print contents of room
C
C Declarations
C
      SUBROUTINE PRINCR(FULL,RM)
       use state
      integer, intent(in) :: FULL,RM

      integer i,j,k

      J=329                              ! assume superbrief format.
      DO 500 I=1,OLNT                        ! loop on objects
        IF(.NOT.QHERE(I,RM).OR.(IAND(OFLAG1(I), VISIBT).EQ.0).OR.
     &      ((IAND(OFLAG1(I), NDSCBT).NE.0).AND.(FULL.NE.1)).OR.
     &      (I.EQ.AVEHIC(WINNER))) GO TO 500
        IF((FULL.EQ.0).AND.(SUPERF.OR.(BRIEFF.AND.
     &      (IAND(RFLAG(HERE), RSEEN).NE.0)))) GO TO 200
C
C Do long description of object.
C
        K=ODESCO(I)                        ! get untouched.
        IF((K.EQ.0).OR.(IAND(OFLAG2(I), TCHBT).NE.0)) K=ODESC1(I)
        IF((K.EQ.0).AND.(FULL.EQ.1)) CALL RSPSUB(936,ODESC2(I))
        CALL RSPEAK(K)                  ! describe.
        GO TO 500
C
C Do short description of object.
C
200        CALL RSPSUB(J,ODESC2(I))            ! you can see it.
        J=502
C
500      CONTINUE
C
C Now loop to print contents of objects in room.
C
      DO 1000 I=1,OLNT                  ! loop on objects.
        IF(.NOT.QHERE(I,RM).OR.(IAND(OFLAG1(I), VISIBT).EQ.0).OR.
     &      ((IAND(OFLAG1(I), NDSCBT).NE.0).AND.(FULL.NE.1)))
     &      GO TO 1000
        IF(IAND(OFLAG2(I), ACTRBT).NE.0) CALL INVENT(OACTOR(I))
        IF(((IAND(OFLAG1(I), TRANBT).EQ.0).AND.(IAND(OFLAG2(I), OPENBT)
     &      .EQ.0)).OR.QEMPTY(I)) GO TO 1000
C
C Object is not empty and is open or transparent.
C
        IF(I.NE.TCASE) GO TO 600            ! trophy case?
        IF((.NOT.(BRIEFF.OR.SUPERF)).OR.(FULL.EQ.1))
     &      CALL PRINCO(I,1053,.FALSE.)      ! print contents.
        GO TO 1000
600        CALL PRINCO(I,573,.TRUE.)            ! print contents
1000      CONTINUE

      END SUBROUTINE PRINCR

C INVENT- Print contents of adventurer
C
C Declarations
C
      SUBROUTINE INVENT(ADV)
      use state
      integer, intent(in) :: adv

      integer i,j

      I=575                              ! first line.
      IF(ADV.NE.PLAYER) I=576                  ! if not me.
      DO 10 J=1,OLNT                        ! loop
        IF((OADV(J).NE.ADV).OR.(IAND(OFLAG1(J), VISIBT).EQ.0))
     &      GO TO 10
        CALL RSPSUB(I,ODESC2(AOBJ(ADV)))
        I=0
        CALL RSPSUB(502,ODESC2(J))
10      CONTINUE
C
      IF(I.EQ.0) GO TO 25                  ! any objects?
      IF(ADV.EQ.PLAYER) CALL RSPEAK(578)      ! no, tell him.
      RETURN
C
25      DO 100 J=1,OLNT                        ! loop.
        IF((OADV(J).NE.ADV).OR.(IAND(OFLAG1(J), VISIBT).EQ.0).OR.
     &      ((IAND(OFLAG1(J), TRANBT).EQ.0).AND.
     &      (IAND(OFLAG2(J), OPENBT).EQ.0))) GO TO 100
        IF(.NOT.QEMPTY(J)) CALL PRINCO(J,573,.TRUE.) ! if not empty, list.
100      CONTINUE

      END SUBROUTINE INVENT

C PRINCO-      Print contents of object

      SUBROUTINE PRINCO(OBJ,DESC,LDESCF)
      use state
      integer, intent(in) :: obj,desc
      logical, intent(in) :: LDESCF

      integer i,j,x,y,also
      LOGICAL MOREF,QSEEIN,QUAL
C
C Functions and data
C
      QSEEIN(X)=(IAND(OFLAG1(X), TRANBT).NE.0).OR.
     &        (IAND(OFLAG2(X), OPENBT).NE.0)
      QUAL(X,Y)=(IAND(OFLAG1(X), VISIBT).NE.0).AND.
     &         (OCAN(X).EQ.Y).AND.(X.NE.AOBJ(WINNER))
C
      MOREF=.FALSE.                        ! no additional printouts.
      ALSO=0                              ! no untouched descriptions.
      IF(SUPERF.OR..NOT.LDESCF) GO TO 2000      ! skip long descriptions?
      DO 1000 I=1,OLNT                  ! loop thru objects.
        IF(.NOT.QUAL(I,OBJ)) GO TO 1000      ! inside target?
        IF((ODESCO(I).EQ.0).OR.
     &   (IAND(OFLAG2(I), TCHBT).NE.0)) GO TO 700
        CALL RSPEAK(ODESCO(I))            ! print untouched descr.
        ALSO=1                        ! flag.
        IF(.NOT.QSEEIN(I).OR.QEMPTY(I)) GO TO 1000
        CALL RSPSUB(573,ODESC2(I))            ! object, which contains:
        DO 500 J=1,OLNT                  ! loop thru objects.
          IF(QUAL(J,I)) CALL RSPSUB(502,ODESC2(J))
500        CONTINUE
        GO TO 1000
700        MOREF=.TRUE.
1000      CONTINUE
      IF(.NOT.MOREF) RETURN                  ! more to do?
C
2000      CALL RSPSUB(DESC+ALSO,ODESC2(OBJ))      ! print header.
      DO 3000 I=1,OLNT                  ! loop thru objects.
        IF(.NOT.QUAL(I,OBJ)) GO TO 3000      ! inside target?
        IF((ALSO.NE.0).AND.(ODESCO(I).NE.0).AND.
     &    (IAND(OFLAG2(I), TCHBT).EQ.0)) GO TO 3000
        IF(.NOT.QSEEIN(I).OR.QEMPTY(I)) GO TO 2700
        CALL RSPSUB(1050,ODESC2(I))            ! object, which contains:
        DO 2500 J=1,OLNT                  ! loop thru objects.
          IF(QUAL(J,I)) CALL RSPSUB(1051,ODESC2(J))
2500        CONTINUE
        GO TO 3000
2700        CALL RSPSUB(502,ODESC2(I))            ! object, nothing inside.
3000      CONTINUE

      END sUBROUTINE PRINCO

C MOVETO- Move player to new room
C
C Declarations
C
      LOGICAL FUNCTION MOVETO(NR,WHO)
      use state
      integer, intent(in) :: nr,who

      integer j,bits

      LOGICAL NLV,LHR,LNR
C
      MOVETO=.FALSE.                        ! assume fails.
      LHR=IAND(RFLAG(HERE), RLAND).NE.0      ! land  here flag.
      LNR=IAND(RFLAG(NR), RLAND).NE.0            ! land there flag.
      J=AVEHIC(WHO)                        ! his vehicle
C
      IF(J.NE.0) GO TO 100                  ! in vehicle?
      IF(LNR) GO TO 500                  ! no, going to land?
      CALL RSPEAK(427)                  ! can't go without vehicle.
      RETURN
C
100      BITS=0                              ! assume nowhere.
      IF(J.EQ.RBOAT) BITS=RWATER            ! in boat?
      IF(J.EQ.BALLO) BITS=RAIR            ! in balloon?
      IF(J.EQ.BUCKE) BITS=RBUCK            ! in bucket?
      NLV=IAND(RFLAG(NR), BITS).EQ.0            ! got wrong vehicle flag.
      IF((.NOT.LNR .AND.NLV) .OR.
     &      (LNR.AND.LHR.AND.NLV.AND.(BITS.NE.RLAND)))
     &      GO TO 800                  ! got wrong vehicle?
C
500      MOVETO=.TRUE.                        ! move should succeed.
      IF(IAND(RFLAG(NR), RMUNG).EQ.0) GO TO 600 ! room munged?
      CALL RSPEAK(RDESC1(NR))                  ! yes, tell how.
      RETURN
C
600      IF(WHO.NE.PLAYER) CALL NEWSTA(AOBJ(WHO),0,NR,0,0)
      IF(J.NE.0) CALL NEWSTA(J,0,NR,0,0)
      HERE=NR
      AROOM(WHO)=HERE
      CALL SCRUPD(RVAL(NR))                  ! score room
      RVAL(NR)=0
      RETURN
C
800      CALL RSPSUB(428,ODESC2(J))            ! wrong vehicle.

      END FUNCTION MOVETO

C SCORE-- Print out current score
C
C Declarations
C
      SUBROUTINE SCORE(FLG)
      use state
      LOGICAL,intent(in) :: FLG
      INTEGER,parameter :: RANK(*)=[20,19,18,16,12,8,4,2,1,0],
     & ERANK(*) = [20,15,10,5,0]

      integer as,i


      AS=ASCORE(WINNER)
      IF(ENDGMF) GO TO 60                  ! endgame?
      IF(FLG) WRITE(output_unit,100)
      IF(.NOT.FLG) WRITE(output_unit,110)
      IF(MOVES.NE.1) WRITE(output_unit,120) AS,MXSCOR,MOVES
      IF(MOVES.EQ.1) WRITE(output_unit,130) AS,MXSCOR,MOVES
      IF(AS.LT.0) GO TO 50                  ! negative score?
      DO 10 I=1,10                        ! find rank.
        IF((AS*20/MXSCOR).GE.RANK(I)) GO TO 20
10      CONTINUE
      I=10                              ! beginner.
20      CALL RSPEAK(484+I)                  ! print rank.
      RETURN
C
50      CALL RSPEAK(886)                  ! negative score.
      RETURN
C
60      IF(FLG) WRITE(output_unit,140)
      IF(.NOT.FLG) WRITE(output_unit,150)
      WRITE(output_unit,120) EGSCOR,EGMXSC,MOVES
      DO 70 I=1,5
        IF((EGSCOR*20/EGMXSC).GE.ERANK(I)) GO TO 80
70      CONTINUE
      I=5                              ! beginner.
80      CALL RSPEAK(786+I)
      RETURN
C
100      FORMAT(' Your score would be',$)
110      FORMAT(' Your score is',$)
120      FORMAT('+',I4,' [total of',I4,' points], in',I5,' moves.')
130      FORMAT('+',I4,' [total of',I4,' points], in',I5,' move.')
140      FORMAT(' Your score in the endgame would be',$)
150      FORMAT(' Your score in the endgame is',$)

      END SUBROUTINE SCORE

C SCRUPD- Update winner's score
C
C Declarations
C
      SUBROUTINE SCRUPD(N)
      use state
      integer, intent(in) :: N

      IF(ENDGMF) GO TO 100                  ! endgame?
      ASCORE(WINNER)=ASCORE(WINNER)+N            ! update score
      RWSCOR=RWSCOR+N                        ! update raw score
      IF(ASCORE(WINNER).LT.(MXSCOR-(10*MIN0(1,DEATHS)))) RETURN
      CFLAG(CEVEGH)=.TRUE.                  ! turn on end game
      CTICK(CEVEGH)=15
      RETURN
C
100      EGSCOR=EGSCOR+N                        ! update eg score.
 
      END SUBROUTINE SCRUPD

C FINDXT- Find exit from room
C
C Declarations
C
      LOGICAL FUNCTION FINDXT(DIR,RM)
      use state
      integer, intent(in):: dir,rm

      integer i,xi

      FINDXT=.TRUE.                        ! assume wins.
      XI=REXIT(RM)                        ! find first entry.
      IF(XI==0) GO TO 1000                  ! no exits?
C
100      I=TRAVEL(XI)                        ! get entry.
      XROOM1=IAND(I, XRMASK)                  ! isolate room.
      XTYPE=IAND((IAND(I, NOT(XLFLAG))/XFSHFT), XFMASK)+1
      GO TO (110,120,130,130),XTYPE            ! branch on entry.
      CALL BUG(10,XTYPE)
C
130      XOBJ=IAND(TRAVEL(XI+2), XRMASK)            ! door/cexit- get obj/flag.
      XACTIO=TRAVEL(XI+2)/XASHFT
120      XSTRNG=TRAVEL(XI+1)                  ! door/cexit/nexit - string.
110      XI=XI+XELNT(XTYPE)                  ! advance to next entry.
      IF(IAND(I, XDMASK).EQ.DIR) RETURN      ! match?
      IF(IAND(I, XLFLAG).EQ.0) GO TO 100      ! last entry?
1000      FINDXT=.FALSE.                        ! yes, lose.

      END FUNCTION FINDXT

C FWIM- Find what I mean
C
C Declarations
C
      INTEGER FUNCTION FWIM(F1,F2,RM,CON,ADV,NOCARE)
      use state
      integer, intent(in) :: F1,F2,RM,CON,ADV
      LOGICAL, intent(in) :: NOCARE

        integer i,j

      FWIM=0                              ! assume nothing.
      DO 1000 I=1,OLNT                  ! loop
        IF(((RM.EQ.0).OR.(.NOT.QHERE(I,RM))) .AND.
     &      ((ADV.EQ.0).OR.(OADV(I).NE.ADV)) .AND.
     &      ((CON.EQ.0).OR.(OCAN(I).NE.CON)))
     &      GO TO 1000
C
C Object is on list... is it a match?
C
        IF(IAND(OFLAG1(I), VISIBT).EQ.0) GO TO 1000
        IF((.NOT.NOCARE .AND.(IAND(OFLAG1(I), TAKEBT).EQ.0)) .OR.
     &      ((IAND(OFLAG1(I), F1)==0).AND.
     &       (IAND(OFLAG2(I), F2)==0))) GO TO 500
        IF(FWIM.EQ.0) GO TO 400            ! already got something?
        FWIM=-FWIM                        ! yes, ambiguous.
        RETURN
C
400        FWIM=I                        ! note match.
C
C Does object contain a match?
C
500        IF(IAND(OFLAG2(I), OPENBT).EQ.0) GO TO 1000 ! closed?
        DO J=1,OLNT                  ! no, search contents.
          IF((OCAN(J)/=I).OR.(IAND(OFLAG1(J), VISIBT)==0) .OR.
     & ((IAND(OFLAG1(J), F1).EQ.0).AND.
     &  (IAND(OFLAG2(J), F2).EQ.0))) cycle
          IF(FWIM==0) GO TO 600
          FWIM=-FWIM
          RETURN
C
600          FWIM=J
      enddo
1000      CONTINUE
      
      END FUNCTION FWIM

C ORPHAN- Set up orphans for parser
      SUBROUTINE ORPHAN(OR1,OR2,OR3,OR4,OR5,OR6,OR7,OR8)
      use state

      integer, intent(in) :: OR1,OR2,OR3,OR4,OR5,OR7,OR8
      CHARACTER(*),intent(in) :: OR6

      OFLAG=OR1
      OACT=OR2
      OPREP1=OR3
      OOBJ1=OR4
      OPREP=OR5
      ONAME=OR6
      OPREP2=OR7
      OOBJ2=OR8

      END SUBROUTINE ORPHAN

C YESNO- Obtain yes/no answer
C
C Called by-
C
C      YES-IS-TRUE=YESNO(QUESTION,YES-STRING,NO-STRING)
C
      LOGICAL FUNCTION YESNO(Q,Y,N)

      integer, intent(in) :: Q,Y,N

      CHARACTER(1) ANS
C
100      CALL RSPEAK(Q)                        ! ask
      READ(input_unit,110,END=120) ANS            ! get answer
110      FORMAT(A)
      IF((ANS.EQ.'Y').OR.(ANS.EQ.'y')) GO TO 200
      IF((ANS.EQ.'N').OR.(ANS.EQ.'n')) GO TO 300
120      CALL RSPEAK(6)                        ! scold.
      GO TO 100
C
200      YESNO=.TRUE.                        ! yes,
      CALL RSPEAK(Y)                        ! out with it.
      RETURN
C
300      YESNO=.FALSE.                        ! no,
      CALL RSPEAK(N)                        ! likewise.

      END Function YesNo

C ROBADV-- Steal winner's valuables
C
C Declarations
C
      INTEGER FUNCTION ROBADV(ADV,NR,NC,NA)
      use state,
        integer, intent(in) :: ADV,NR,NC,NA

      integer i

      ROBADV=0                        ! count objects
      DO 100 I=1,OLNT
        IF((OADV(I).NE.ADV).OR.(OTVAL(I).LE.0).OR.
     &      (IAND(OFLAG2(I), SCRDBT).NE.0)) GO TO 100
        CALL NEWSTA(I,0,NR,NC,NA)            ! steal object
        ROBADV=ROBADV+1
100      CONTINUE

      END FUNCTION robadv

C ROBRM-- Steal room valuables
C
C Declarations
C
      INTEGER FUNCTION ROBRM(RM,PR,NR,NC,NA)
       use state
      integer, intent(in) :: RM,PR,NR,NC,NA

        integer i

      ROBRM=0                              ! count objects
      DO 100 I=1,OLNT                        ! loop on objects.
        IF(.NOT. QHERE(I,RM)) GO TO 100
        IF((OTVAL(I).LE.0).OR.(IAND(OFLAG2(I), SCRDBT).NE.0).OR.
     &      (IAND(OFLAG1(I), VISIBT).EQ.0).OR.(.NOT.PROB(PR,PR)))
     &      GO TO 50
        CALL NEWSTA(I,0,NR,NC,NA)
        ROBRM=ROBRM+1
        OFLAG2(I)=IOR(OFLAG2(I), TCHBT)
        GO TO 100
50        IF(IAND(OFLAG2(I), ACTRBT).NE.0)
     &      ROBRM=ROBRM+ROBADV(OACTOR(I),NR,NC,NA)
100      CONTINUE

      END FUNCTION ROBRM
 
C WINNIN-- See if villain is winning

      LOGICAL FUNCTION WINNIN(VL,HR)
      use state
      integer, intent(in):: vl, hr

        integer ps,vs

      VS=OCAPAC(VL)                        ! villain strength
      PS=VS-FIGHTS(HR,.TRUE.)                  ! his margin over hero
      WINNIN=PROB(90,100)
      IF(PS.GT.3) RETURN                  ! +3... 90% winning
      WINNIN=PROB(75,85)
      IF(PS.GT.0) RETURN                  ! >0... 75% winning
      WINNIN=PROB(50,30)
      IF(PS.EQ.0) RETURN                  ! =0... 50% winning
      WINNIN=PROB(25,25)
      IF(VS.GT.1) RETURN                  ! any villain strength.
      WINNIN=PROB(10,0)

      END FUNCTION WINNIN

C FIGHTS-- Compute fight strength

      INTEGER FUNCTION FIGHTS(H,FLG)
        use state

      integer,PARAMETER :: STRMAX=7,STRMIN=2
      LOGICAL, intent(in) :: FLG
      integer, intent(in) :: h
C
      FIGHTS=STRMIN+((((STRMAX-STRMIN)*ASCORE(H))+(MXSCOR/2))/MXSCOR)
      IF(FLG) FIGHTS=FIGHTS+ASTREN(H)

      END FUNCTION FIGHTS

C VILSTR-      Compute villain strength
C
C Declarations
C
      INTEGER FUNCTION VILSTR(V)
      use state
      integer, intent(in) :: V

      integer i



      VILSTR=OCAPAC(V)
      IF(VILSTR<=0) RETURN
      IF((V<=THIEF).OR..NOT.THFENF) GO TO 100
      THFENF=.FALSE.                        ! thief unengrossed.
      VILSTR=MIN0(VILSTR,2)                  ! no better than 2.
C
100      DO 200 I=1,VLNT                        ! see if best weapon.
        IF((VILLNS(I).EQ.V).AND.(PRSI.EQ.VBEST(I)))
     &      VILSTR=MAX0(1,VILSTR-1)
200      CONTINUE

      END  FUNCTION VILSTR

C GTTIME-- Get total time played
C
C Declarations
C
      SUBROUTINE GTTIME(T)
      USE state,only: shour, smin,pltime,tmarray
      integer, intent(out) :: t
      integer h,m,s
    
      Call date_and_time(values=TMARRAY)
      H=TMARRAY(5)
      M=TMARRAY(6)
      S=TMARRAY(7)

      T=((H*60)+M)-((SHOUR*60)+SMIN)
      IF(T<0) T=T+1440
      T=T+PLTIME

      END SUBROUTINE GTTIME

C OPNCLS-- Process open/close for doors
C
C Declarations
C
      LOGICAL FUNCTION OPNCLS(OBJ,SO,SC)
      use state!, only: oflag2
      integer, intent(in) :: obj,so,sc

      LOGICAL QOPEN
      integer o
C
C Functions and data
C
      QOPEN(O)=IAND(OFLAG2(O), OPENBT).NE.0
C
      OPNCLS=.TRUE.                        ! assume wins.
      IF(PRSA.EQ.CLOSEW) GO TO 100            ! close?
      IF(PRSA.EQ.OPENW) GO TO 50            ! open?
      OPNCLS=.FALSE.                        ! lose
      RETURN
C
50      IF(QOPEN(OBJ)) GO TO 200            ! open... is it?
      CALL RSPEAK(SO)
      OFLAG2(OBJ)=IOR(OFLAG2(OBJ), OPENBT)
      RETURN
C
100      IF(.NOT.QOPEN(OBJ)) GO TO 200            ! close... is it?
      CALL RSPEAK(SC)
      OFLAG2(OBJ)=IAND(OFLAG2(OBJ), NOT(OPENBT))
      RETURN
C
200      CALL RSPEAK(125+RND(3))                  ! dummy.

      END  FUNCTION OPNCLS

C LIT-- Is room lit?
C
C Declarations
C
      LOGICAL FUNCTION LIT(RM)
      use state!, only: rflag,deadf,olnt,onbt

      integer, intent(in) :: rm
      integer i,j,oa
C
      LIT=.TRUE.                        ! assume wins
      IF(DEADF.OR.(IAND(RFLAG(RM), RLIGHT).NE.0)) RETURN      ! room lit?
C
      DO 1000 I=1,OLNT                  ! look for lit obj
        IF(QHERE(I,RM)) GO TO 100            ! in room?
        OA=OADV(I)                        ! no
        IF(OA.LE.0) GO TO 1000            ! on adv?
        IF(AROOM(OA).NE.RM) GO TO 1000      ! adv in room?
C
C Obj in room or on adv in room
C
100        IF(IAND(OFLAG1(I), ONBT).NE.0) RETURN      ! lit?
        IF((IAND(OFLAG1(I), VISIBT).EQ.0).OR.
     &      ((IAND(OFLAG1(I), TRANBT).EQ.0).AND.
     &      (IAND(OFLAG2(I), OPENBT).EQ.0))) GO TO 1000
C
C Obj is visible and open or transparent
C
        DO 500 J=1,OLNT
          IF((OCAN(J).EQ.I).AND.(IAND(OFLAG1(J), ONBT).NE.0))
     &      RETURN
500        CONTINUE
1000      CONTINUE
      LIT=.FALSE.

      END FUNCTION LIT

C WEIGHR- Returns sum of weight of qualifying objects
C
C Declarations
C
      INTEGER FUNCTION WEIGHR(CN,AD)
      use state
        integer, intent(in) :: cn,ad
      integer i,j
      LOGICAL QHERE
C
      WEIGHR=0
      DO 100 I=1,OLNT                        ! omit big fixed items.
        IF(OSIZE(I)>=10000) GO TO 100      ! if fixed, forget it.
        IF((OADV(I)==AD).AND.(AD/=0)) GO TO 50      ! on adv?
        J=I                              ! see if contained.
25        J=OCAN(J)                        ! get next level up.
        IF(J==0) GO TO 100                  ! end of list?
        IF(((OADV(J)/=AD).OR.(AD==0)) .AND.
     &      (J/=CN)) GO TO 25            ! cont on adv, or argument?
50        WEIGHR=WEIGHR+OSIZE(I)            ! add in weight.
100      CONTINUE

      END FUNCTION WEIGHR

C GHERE--      Is global actually in this room?
C
C Declarations
C
      LOGICAL FUNCTION GHERE(OBJ,RM)
      use state
      integer, intent(in) :: OBJ,RM        

      GHERE=.TRUE.                        ! assume wins.
      IF(OBJ<=GLOBAL) RETURN            ! if untested, return.
      GO TO (  100,1000,2000,3000,4000,5000,5000,5000,6000,
     &      7000,8000,9000,9100,8000,10000,11000,12000,
     &      13000,14000,15000),OBJ-GLOBAL
      CALL BUG(60,OBJ)
C
C 100-- Granite Wall
C
100      GHERE=(RM.EQ.TEMP1).OR.(RM.EQ.TREAS).OR.(RM.EQ.SLIDE)
      RETURN
C
C 1000--      House
C
1000      GHERE=((RM.GE.WHOUS).AND.(RM.LE.EHOUS)).OR.
     &      ((RM.GE.FORE1).AND.(RM.LE.CLEAR)).OR.(RM.EQ.MTREE)
      RETURN
C
C 2000--      Bird
C
2000      GHERE=((RM.GE.FORE1).AND.(RM.LT.CLEAR)).OR.(RM.EQ.MTREE)
      RETURN
C
C 3000--      Tree
C
3000      GHERE=((RM.GE.FORE1).AND.(RM.LT.CLEAR)).AND.(RM.NE.FORE3)
      RETURN
C
C 4000--      North wall
C
4000      GHERE=((RM.GE.BKVW).AND.(RM.LE.BKBOX)).OR.(RM.EQ.CPUZZ)
      RETURN
C
C 5000--      East, south, west walls
C
5000      GHERE=((RM.GE.BKVW).AND.(RM.LT.BKBOX)).OR.(RM.EQ.CPUZZ)
      RETURN
C
C 6000--      Global water
C
6000      GHERE=IAND(RFLAG(RM), (RWATER+RFILL)).NE.0
      RETURN
C
C 7000--      Global guardians
C
7000      GHERE=((RM.GE.MRC).AND.(RM.LE.MRD)).OR.
     &      ((RM.GE.MRCE).AND.(RM.LE.MRDW)).OR.(RM.EQ.INMIR)
      RETURN
C
C 8000--      Rose/channel
C
8000      GHERE=((RM.GE.MRA).AND.(RM.LE.MRD)).OR.(RM.EQ.INMIR)
      RETURN
C
C 9000--      Mirror
C 9100            Panel
C
9100      IF(RM.EQ.FDOOR) RETURN                  ! panel at fdoor.
9000      GHERE=((RM.GE.MRA).AND.(RM.LE.MRC)).OR.
     &      ((RM.GE.MRAE).AND.(RM.LE.MRCW))
      RETURN
C
C 10000--      Master
C
10000      GHERE=(RM.EQ.FDOOR).OR.(RM.EQ.NCORR).OR.(RM.EQ.PARAP).OR.
     &      (RM.EQ.CELL).OR.(RM.EQ.PCELL).OR.(RM.EQ.NCELL)
      RETURN
C
C 11000--      Ladder
C
11000      GHERE=(RM.EQ.CPUZZ)
      RETURN
C
C 12000--      Well
C
12000      GHERE=(RM.EQ.TWELL).OR.(RM.EQ.BWELL)
      RETURN
C
C 13000--      Rope in slide
C
13000      GHERE=(RM.GE.SLID1).AND.(RM.LE.SLEDG)
      RETURN
C
C 14000--      Slide
C
14000      GHERE=(RM.GE.SLIDE).OR.((RM.GE.SLID1).AND.(RM.LE.SLEDG))
      RETURN
C
C 15000--      Bunch pseudo object
C
15000      GHERE=.FALSE.                  ! never present

      END  FUNCTION GHERE

C MRHERE--      Is mirror here?
C
C Declarations
C
      INTEGER FUNCTION MRHERE(RM)
      use state, only: mrae, mrdw,mdir,mloc
      integer, intent(in) :: rm

      IF((RM.LT.MRAE).OR.(RM.GT.MRDW)) GO TO 100
C
C Room is an e-w room, mirror must be n-s (mdir= 0 or 180)
C
      MRHERE=1                        ! assume mirror 1 here.
      IF(MOD(RM-MRAE,2).EQ.(MDIR/180)) MRHERE=2
      RETURN
C
C Room is north or south of mirror.  If mirror is n-s or not
c within one room of room, lose.
C
100      MRHERE=0
      IF((IABS(MLOC-RM).NE.1).OR.(MOD(MDIR,180).EQ.0)) RETURN
C
C Room is within one of mloc, and mdir is e-w
C
      MRHERE=1
      IF(((RM.LT.MLOC).AND.(MDIR.LT.180)).OR.
     &  ((RM.GT.MLOC).AND.(MDIR.GT.180))) MRHERE=2

      END FUNCTION MRHERE

C ENCRYP--      Encrypt password
C
C Declarations
C
      SUBROUTINE ENCRYP(INW,OUTW)
      use state, only: wrdlnt
      CHARACTER(WRDLNT),intent(IN) :: INW
      character(*), intent(out) :: OUTW
      INTEGER UINW(8),UKEYW(8)
      character(*),parameter :: KEYW='ECOVXRMS'
      integer i,j,ichara,uinws,ukeyws,usum
C
      ICHARA=ICHAR('A')-1                  ! character base.
      UINWS=0                              ! unbiased inw sum.
      UKEYWS=0                        ! unbiased keyw sum.
      J=1                              ! pointer in keyword.
      DO 100 I=1,WRDLNT                  ! unbias, compute sums.
        UKEYW(I)=ICHAR(KEYW(I:I))-ICHARA      ! strip ascii.
        IF(ICHAR(INW(J:J)).LE.ICHARA) J=1      ! recycle on bad.
        UINW(I)=ICHAR(INW(J:J))-ICHARA
        UKEYWS=UKEYWS+UKEYW(I)
        UINWS=UINWS+UINW(I)
        J=J+1
100      CONTINUE
C
      USUM=MOD(UINWS,8)+(8*MOD(UKEYWS,8))      ! compute mask.
      DO 200 I=1,8
        J=IAND(IEOR(IEOR(UINW(I), UKEYW(I)), USUM), 31)
        USUM=MOD(USUM+1,32)
        IF(J.GT.26) J=MOD(J,26)
        OUTW(I:I)=CHAR(MAX0(1,J)+ICHARA)
200      CONTINUE

      END SUBROUTINE ENCRYP

C CPGOTO--      Move to next state in puzzle room
C
C Declarations
C
      SUBROUTINE CPGOTO(ST)
      use state
      integer, intent(in) :: st
      integer i

      RFLAG(CPUZZ)=IAND(RFLAG(CPUZZ), NOT(RSEEN))
      DO I=1,OLNT                        ! relocate objects.
        IF((OROOM(I).EQ.CPUZZ).AND.
     &      (IAND(OFLAG2(I), (ACTRBT+VILLBT)).EQ.0))
     &      CALL NEWSTA(I,0,CPHERE*HFACTR,0,0)
        IF(OROOM(I).EQ.(ST*HFACTR))
     &      CALL NEWSTA(I,0,CPUZZ,0,0)
      enddo
      CPHERE=ST

      END SUBROUTINE CPGOTO

C CPINFO--      Describe puzzle room
C
C Declarations
C
      SUBROUTINE CPINFO(RMK,ST)
       use state,only: cpoutf,cpvec
      integer, intent(in) :: rmk, st

      INTEGER :: DGMOFT(8)=[-9,-8,-7,-1,1,7,8,9]
      CHARACTER(2) :: DGM(8),QMK='??'
      character(*),parameter :: PICT(*)=['SS','SS','SS','  ','MM']
        
      integer i,j,k,l

C
      CALL RSPEAK(RMK)
      DO 100 I=1,8
        J=DGMOFT(I)
        DGM(I)=PICT(CPVEC(ST+J)+4)            ! get picture element.
        IF((IABS(J).EQ.1).OR.(IABS(J).EQ.8)) GO TO 100
        K=8
        IF(J.LT.0) K=-8                  ! get ortho dir.
        L=J-K
        IF((CPVEC(ST+K).NE.0).AND.(CPVEC(ST+L).NE.0))
     &      DGM(I)=QMK
100      CONTINUE
      WRITE(output_unit,10) DGM
C
      IF(ST.EQ.10) CALL RSPEAK(870)            ! at hole?
      IF(ST.EQ.37) CALL RSPEAK(871)            ! at niche?
      I=872                              ! door open?
      IF(CPOUTF) I=873
      IF(ST.EQ.52) CALL RSPEAK(I)            ! at door?
      IF(CPVEC(ST+1).EQ.-2) CALL RSPEAK(874)      ! east ladder?
      IF(CPVEC(ST-1).EQ.-3) CALL RSPEAK(875)      ! west ladder?
      RETURN
C
10      FORMAT('       |',A,1X,A,1X,A,'|'/,
     & ' West  |',A,' .. ',A,'|  East'/,
     & '       |',A,1X,A,1X,A,'|')
C
      END SUBROUTINE CPINFO


      INTEGER FUNCTION RND (N)
      implicit none
      integer, intent(in) :: N
      real ran

      call random_number(ran)
      rnd = int(ran*FLOAT(N))

      print *,rnd

      END FUNCTION RND

      subroutine init_random_seed()
        integer :: i, n, clock
        integer, allocatable :: seed(:)

        call random_seed(size=n)
        allocate(seed(n))
        call system_clock(count=clock)
        
        do concurrent (i=1:n)
            seed(i) = clock + 37 * (i-1)
        enddo

        call random_seed(put=seed)
      end subroutine init_random_seed

      end module
