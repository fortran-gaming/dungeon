C Clock events, demons, actors for DUNGEON.
C
C COPYRIGHT 1980, 1990, INFOCOM COMPUTERS AND COMMUNICATIONS, CAMBRIDGE MA.
C ALL RIGHTS RESERVED, COMMERCIAL USAGE STRICTLY PROHIBITED
C WRITTEN BY R. M. SUPNIK
C
C 02-Dec-15     EMG     Compile using gfortran
C 27-Sep-94      RMS      Fixed bugs in thief demon, fight demon, master actor,
C                  robot actor, dead player, balloon, bell.
C 30-Jan-94      RMS      Fixed bugs from MS DOS port.
C 18-Jan-94      RMS      Fixed bug in dead player recovery.
C 01-Jul-92      RMS      Removed extraneous function from CEVAPP.
C 30-Jun-92      RMS      Changed file names to lower case.
C
      module timefnc
        use subr,only: newsta,qhere
        use io,only: rspeak,rspsub
      implicit none
      contains
C LITINT-      Light interrupt processor
C
C Declarations
C
      SUBROUTINE LITINT(OBJ,CTR,CEV,TICKS,TICKLN)
      use state
      integer, intent(in) :: OBJ,CEV,TICKS(TICKLN),TICKLN
      integer, intent(inout) :: CTR

      CTR=CTR+1                        ! advance state cntr.
      CTICK(CEV)=TICKS(CTR)                  ! reset interrupt.
      IF(CTICK(CEV)/=0) GO TO 100            ! expired?
      OFLAG1(OBJ)=IAND(OFLAG1(OBJ), NOT(LITEBT+FLAMBT+ONBT))
      IF((OROOM(OBJ)==HERE).OR.(OADV(OBJ)==WINNER))
     &      CALL RSPSUB(293,ODESC2(OBJ))
      RETURN
C
100      CFLAG(CEV)=.TRUE.
      IF((OROOM(OBJ)==HERE).OR.(OADV(OBJ)==WINNER))
     &      CALL RSPEAK(TICKS(CTR+(TICKLN/2)))

      END SUBROUTINE LITINT

C FIGHTD- Intermove fight demon

      SUBROUTINE FIGHTD
      use state
      use objapp,only: oappli
      use subr

      integer i,res,ra,out,obj,j
      LOGICAL F
      integer,parameter :: ROUT=1

C FIGHTD, PAGE 2
C
      DO 2400 I=1,VLNT                  ! loop thru villains.
        VOPPS(I)=0                        ! clear opponent slot.
        OBJ=VILLNS(I)                        ! get object no.
        RA=OACTIO(OBJ)                  ! get his action.
        IF(HERE/=OROOM(OBJ)) GO TO 2200      ! adventurer still here?
        IF((OBJ/=THIEF).OR. .NOT.THFENF) GO TO 2010 ! thief engrossed?
        THFENF=.FALSE.                  ! yes, not anymore.
        GO TO 2400
C
2010        IF(OCAPAC(OBJ)>=0) GO TO 2050      ! no, vill awake?
        IF((VPROB(I)==0).OR..NOT.PROB(VPROB(I),(100+VPROB(I))/2))
     &      GO TO 2025                  ! no, see if wakes up.
        OCAPAC(OBJ)=IABS(OCAPAC(OBJ))
        VPROB(I)=0
        IF(RA==0) GO TO 2400            ! anything to do?
        PRSA=INXW                        ! yes, wake him up.
        F=OAPPLI(RA,0)
        GO TO 2400                        ! nothing else happens.
C
2025        VPROB(I)=VPROB(I)+10                  ! increase wakeup prob.
        GO TO 2400                        ! nothing else.
C
2050        IF(IAND(OFLAG2(OBJ), FITEBT)==0) GO TO 2100
        VOPPS(I)=OBJ                        ! fighting, set up opp.
        GO TO 2400
C
2100        IF(RA==0) GO TO 2400            ! not fighting,
        PRSA=FRSTQW                        ! set up probability
        IF(.NOT.OAPPLI(RA,0)) GO TO 2400      ! of fighting.
        OFLAG2(OBJ)=IOR(OFLAG2(OBJ), FITEBT)
        VOPPS(I)=OBJ                        ! set up opp.
        PRSCON=0                        ! stop cmd stream.
        GO TO 2400
C
2200        IF((IAND(OFLAG2(OBJ), FITEBT)==0).OR.(RA==0))
     &      GO TO 2300                  ! nothing to do.
        PRSA=FIGHTW                        ! have a fight.
        F=OAPPLI(RA,0)
2300        IF(OBJ==THIEF) THFENF=.FALSE.      ! turn off engrossed.
        AFLAG(PLAYER)=IAND(AFLAG(PLAYER), NOT(ASTAG))
        OFLAG2(OBJ)=IAND(OFLAG2(OBJ), NOT(STAGBT+FITEBT))
        IF((OCAPAC(OBJ)>=0).OR.(RA==0))
     &      GO TO 2400
        PRSA=INXW                        ! wake him up.
        F=OAPPLI(RA,0)
        OCAPAC(OBJ)=IABS(OCAPAC(OBJ))
2400      CONTINUE

C FIGHTD, PAGE 3
C
C Now do actual counterblows.
C
      OUT=0                              ! assume hero ok.
2600      DO 2700 I=1,VLNT                  ! loop thru opps.
        J=VOPPS(I)
        IF(J==0) GO TO 2700                  ! slot empty?
        PRSCON=0                        ! stop cmd stream.
        RA=OACTIO(J)
        IF(RA==0) GO TO 2650            ! villain action?
        PRSA=FIGHTW                        ! see if
        IF(OAPPLI(RA,0)) GO TO 2700            ! special action.
2650        RES=BLOW(PLAYER,J,VMELEE(I),.FALSE.,OUT) ! strike blow.
        IF(RES<0) RETURN                  ! if hero dead, exit.
        IF(RES==ROUT) OUT=2+RND(3)            ! if hero out, set flg.
2700      CONTINUE
      OUT=OUT-1                        ! decrement out count.
      IF(OUT>0) GO TO 2600                  ! if still out, go again.

      END SUBROUTINE FIGHTD

C BLOW- Strike blow
C
C Declarations
C
      INTEGER FUNCTION BLOW(H,V,RMK,HFLG,OUT)
      use state
      use objapp,only: oappli
      use subr,only: fights,fwim,prob,rnd,vilstr,jigsup

      integer, intent(in) :: H,V,RMK,OUT
      LOGICAL,intent(in) :: HFLG

       integer att,def,dv,dweap,i,j,mi,oa,od,pblose,ra,res,tbl
      logical F
C
C Functions and data
C
      integer,parameter :: RMISS=0,ROUT=1,RKILL=2,RXXX=3,RSER=4,RSTAG=5,
     & RLOSE=6, RHES=7,RSIT=8
      integer,parameter :: DEF1R(*)=[1,2,3], DEF2R(*)=[13,23,24,25],
     & DEF3R(*)=[35,36,46,47,57]
C
      integer,parameter :: RVECTR(*)=[0,0,0,0,5,5,1,1,2,2,2,2,
     &      0,0,0,0,0,5,5,3,3,1,
     &      0,0,0,5,5,3,3,3,1,2,2,2,
     &      0,0,0,0,0,5,5,3,3,4,4,
     &      0,0,0,5,5,3,3,3,4,4,4,
     &      0,5,5,3,3,3,3,4,4,4]
      integer,parameter :: RSTATE(*)=[5000,3005,3008,4011,3015,3018,
     &      1021,0,0, 5022,3027,3030,4033,3037,3040,1043,0,0,
     &      4044,2048,4050,4054,5058,4063,4067,3071,1074,
     &      4075,1079,4080,4084,4088,4092,4096,4100,1104,
     &      4105,2109,4111,4115,4119,4123,4127,3131,3134]

C BLOW, PAGE 2
C
      RA=OACTIO(V)                        ! get villain action,
      DV=ODESC2(V)                        ! description.
      BLOW=RMISS                        ! assume no result.
C      TYPE 10, H,V,RMK,HFLG,OUT
10      FORMAT(' BLOW 10-- ',3I7,L7,I7)
      IF(.NOT.HFLG) GO TO 1000            ! hero striking blow?
C
C Hero is attacker, villain is defender.
C
      PBLOSE=10                        ! bad lk prob.
      OFLAG2(V)=IOR(OFLAG2(V), FITEBT)            ! yes, villain gets mad.
      IF(IAND(AFLAG(H), ASTAG)==0) GO TO 100      ! hero stag?
      CALL RSPEAK(591)                  ! yes, cant fight.
      AFLAG(H)=IAND(AFLAG(H), NOT(ASTAG))
      RETURN
C
100   ATT=MAX0(1,FIGHTS(H,.TRUE.))            ! get his strength.
      OA=ATT
      DEF=VILSTR(V)                        ! get vill strength.
      OD=DEF
      DWEAP=0                              ! assume no weapon.
      DO 200 I=1,OLNT                        ! search villain.
        IF((OCAN(I)==V).AND.(IAND(OFLAG2(I), WEAPBT)/=0))
     &      DWEAP=I
200      CONTINUE
      IF(V==AOBJ(PLAYER)) GO TO 300            ! killing self?
      IF(DEF/=0) GO TO 2000                  ! defender alive?
      CALL RSPSUB(592,DV)                  ! villain dead.
      RETURN
C
300      CALL JIGSUP(593)                  ! killing self.
      RETURN
C
C Villain is attacker, hero is defender.
C
1000      PRSCON=0                        ! stop cmd stream.
      PBLOSE=50                        ! bad lk prob.
      AFLAG(H)=IAND(AFLAG(H), NOT(ASTAG))      ! vill striking.
      IF(IAND(OFLAG2(V), STAGBT)==0) GO TO 1200 ! vill staggered?
      OFLAG2(V)=IAND(OFLAG2(V), NOT(STAGBT))      ! make him ok.
      CALL RSPSUB(594,DV)                  ! describe.
      RETURN
C
1200      ATT=VILSTR(V)                        ! set up att, def.
      OA=ATT
      DEF=FIGHTS(H,.TRUE.)
      IF(DEF<=0) RETURN                  ! dont allow dead def.
      OD=FIGHTS(H,.FALSE.)
      DWEAP=IABS(FWIM(0,WEAPBT,0,0,H,.TRUE.))      ! find a weapon.

C BLOW, PAGE 3
C
C Parties are now equipped.  DEF cannot be zero.
C ATT must be > 0.
C
2000      CONTINUE
C      TYPE 2050,ATT,OA,DEF,OD,DWEAP
2050      FORMAT(' BLOW 2050-- ',5I7)
      IF(DEF>0) GO TO 2100                  ! def alive?
      RES=RKILL
      IF(HFLG) CALL RSPSUB(595,DV)            ! deader.
      GO TO 3000
C
2100      IF(DEF-2) 2200,2300,2400            ! def <2,=2,>2
2200      ATT=MIN0(ATT,3)                        ! scale att.
      TBL=DEF1R(ATT)                        ! choose table.
      GO TO 2500
C
2300      ATT=MIN0(ATT,4)                        ! scale att.
      TBL=DEF2R(ATT)                        ! choose table.
      GO TO 2500
C
2400      ATT=ATT-DEF                        ! scale att.
      ATT=MIN0(2,MAX0(-2,ATT))+3
      TBL=DEF3R(ATT)
C
2500      RES=RVECTR(TBL+RND(10))                  ! get result.
      IF(OUT==0) GO TO 2600                  ! was he out?
      IF(RES==RSTAG) GO TO 2550            ! yes, stag--> hes.
      RES=RSIT                        ! otherwise, sitting.
      GO TO 2600
2550      RES=RHES
2600      IF((RES==RSTAG).AND.(DWEAP/=0).AND.PROB(25,PBLOSE))
     &      RES=RLOSE                  ! lose weapon.
C
      MI=RSTATE(((RMK-1)*9)+RES+1)            ! choose table entry.
      IF(MI==0) GO TO 3000
      I=(MOD(MI,1000)+RND(MI/1000))+MBASE+1
      J=DV
      IF(.NOT.HFLG .AND.(DWEAP/=0)) J=ODESC2(DWEAP)
C      TYPE 2650,RES,MI,I,J,MBASE
2650      FORMAT(' BLOW 2650-- ',5I7)
      CALL RSPSUB(I,J)                  ! present result.

C BLOW, PAGE 4
C
C Now apply result.
C
3000      GO TO (4000,3100,3200,3300,3400,3500,3600,4000,3200),RES+1
C             miss, out,kill,lght,svre,stag,lose, hes, sit
C
3100      IF(HFLG) DEF=-DEF                  ! unconscious.
      GO TO 4000
C
3200      DEF=0                              ! killed or sitting duck.
      GO TO 4000
C
3300      DEF=MAX0(0,DEF-1)                  ! light wound.
      GO TO 4000
C
3400      DEF=MAX0(0,DEF-2)                  ! serious wound.
      GO TO 4000
C
3500      IF(HFLG) GO TO 3550                  ! staggered.
      AFLAG(H)=IOR(AFLAG(H), ASTAG)
      GO TO 4000
C
3550      OFLAG2(V)=IOR(OFLAG2(V), STAGBT)
      GO TO 4000
C
3600      CALL NEWSTA(DWEAP,0,HERE,0,0)            ! lose weapon.
      DWEAP=0
      IF(HFLG) GO TO 4000                  ! if hero, done.
      DWEAP=IABS(FWIM(0,WEAPBT,0,0,H,.TRUE.)) ! get new.
      IF(DWEAP/=0) CALL RSPSUB(605,ODESC2(DWEAP))

C BLOW, PAGE 5
C
4000      BLOW=RES                        ! return result.
      IF(.NOT.HFLG) GO TO 4500            ! hero?
      OCAPAC(V)=DEF                        ! store new capacity.
      IF(DEF/=0) GO TO 4100                  ! dead?
      OFLAG2(V)=IAND(OFLAG2(V), NOT(FITEBT))      ! yes, not fighting.
      CALL RSPSUB(572,DV)                  ! he dies.
      CALL NEWSTA(V,0,0,0,0)                  ! make him disappear.
      IF(RA==0) RETURN                  ! if nx to do, exit.
      PRSA=DEADXW                        ! let him know.
      F=OAPPLI(RA,0)
      RETURN
C
4100      IF((RES/=ROUT).OR.(RA==0)) RETURN
      PRSA=OUTXW                        ! let him be out.
      F=OAPPLI(RA,0)
      RETURN
C
4500      ASTREN(H)=-10000                  ! assume dead.
      IF(DEF/=0) ASTREN(H)=DEF-OD
      IF(DEF>=OD) GO TO 4600
      CTICK(CEVCUR)=30
      CFLAG(CEVCUR)=.TRUE.
4600      IF(FIGHTS(H,.TRUE.)>0) RETURN
      ASTREN(H)=1-FIGHTS(H,.FALSE.)            ! he's dead.
      CALL JIGSUP(596)
      BLOW=-1

      END FUNCTION BLOW

C SWORDD- Intermove sword demon
C
C Declarations
C
      SUBROUTINE SWORDD
      use state
      use subr,only: findxt
      integer i,ng

      IF(OADV(SWORD)/=PLAYER) GO TO 500      ! holding sword?
      NG=2                              ! assume vill close.
      IF(INFEST(HERE)) GO TO 300            ! vill here?
      NG=1
      DO 200 I=XMIN,XMAX,XMIN                  ! no, search rooms.
        IF(.NOT.FINDXT(I,HERE)) GO TO 200      ! room that way?
        GO TO (50,200,50,50),XTYPE            ! see if room at all.
50        IF(INFEST(XROOM1)) GO TO 300            ! check room.
200      CONTINUE
      NG=0                              ! no glow.
C
300      IF(NG==SWDSTA) RETURN                  ! any state change?
      CALL RSPEAK(NG+495)                  ! yes, tell new state.
      SWDSTA=NG
      RETURN
C
500      SWDACT=.FALSE.                        ! dropped sword,
      RETURN                              ! disable demon.
      END

C INFEST-      Subroutine to test for infested room
C
C Declarations
C
      pure LOGICAL FUNCTION INFEST(R)
      use state,only: oroom,troll,thief,cyclo,thfact,endgmf,inmir,mloc,
     & mrg,mrge,mrgw
      integer, intent(in) :: r
C
      IF(.NOT.ENDGMF) INFEST=(OROOM(CYCLO)==R).OR.
     &      (OROOM(TROLL)==R).OR.
     &      ((OROOM(THIEF)==R).AND.THFACT)
      IF(ENDGMF) INFEST=(R==MRG).OR.(R==MRGE).OR.
     &      (R==MRGW).OR.
     &      ((R==INMIR).AND.(MLOC==MRG))

      END FUNCTION INFEST

C AAPPLI- Applicables for adventurers
C
C Declarations
C
      LOGICAL FUNCTION AAPPLI(RI)
      use state
      use subr,only: qhere,moveto,findxt,bug,newsta
      integer,intent(in) :: ri
      integer i,j

      LOGICAL F
C
      IF(RI==0) GO TO 10                  ! if zero, no app.
      AAPPLI=.TRUE.                        ! assume wins.
      GO TO (1000,2000,3000),RI            ! branch on adv.
      CALL BUG(11,RI)
C
C Common false return.
C
10      AAPPLI=.FALSE.
      RETURN

C AAPPLI, PAGE 2
C
C A1--      Dead player.
C
1000      IF((PRSA/=ATTACW).AND.(PRSA/=MUNGW).AND.
     &  (PRSA/=KILLW).AND.(PRSA/=SWINGW).AND.
     &  (PRSA/=KICKW).AND.(PRSA/=BLASTW)) GO TO 1050
      CALL RSPEAK(949)                  ! dead can't attack.
      RETURN
C
1050      IF((PRSA/=OPENW).AND.(PRSA/=CLOSEW).AND.
     &  (PRSA/=EATW).AND.(PRSA/=DRINKW).AND.
     &  (PRSA/=INFLAW).AND.(PRSA/=DEFLAW).AND.
     &  (PRSA/=TURNW).AND.(PRSA/=TIEW).AND.
     &  (PRSA/=RUBW).AND.(PRSA/=COUNTW).AND.
     &  (PRSA/=BURNW).AND.(PRSA/=UNTIEW)) GO TO 1100
      CALL RSPEAK(950)                  ! dead can't do simple acts.
      RETURN
C
1100      IF(PRSA/=TRNONW) GO TO 1150
      CALL RSPEAK(951)                  ! dead don't need lights.
      RETURN
C
1150      IF(PRSA/=SCOREW) GO TO 1200
      CALL RSPEAK(952)                  ! dead can't score.
      RETURN
C
1200      IF(PRSA/=TELLW) GO TO 1250
      CALL RSPEAK(953)                  ! dead can't give orders.
      RETURN
C
1250      IF(PRSA/=TAKEW) GO TO 1300
      CALL RSPEAK(954)                  ! dead can't take.
      RETURN
C
1300      IF((PRSA/=DROPW).AND.(PRSA/=THROWW).AND.
     &  (PRSA/=INVENW)) GO TO 1350
      CALL RSPEAK(955)                  ! dead have no possesions
      RETURN
C
1350      IF(PRSA/=DIAGNW) GO TO 1400
      CALL RSPEAK(956)                  ! dead as a doornail
      RETURN
C
1400      IF(PRSA/=LOOKW) GO TO 1500
      I=957                              ! assume nothing here
      DO 1450 J=1,OLNT                  ! loop through objects
        IF(QHERE(J,HERE)) I=958            ! found something
1450      CONTINUE
      CALL RSPEAK(I)                        ! describe objects
      IF(IAND(RFLAG(HERE), RLIGHT)==0) CALL RSPEAK(959)
      GO TO 10                        ! don't handle
C
1500      IF(PRSA/=PRAYW) GO TO 1600
      IF(HERE==TEMP2) GO TO 1550            ! praying in temple?
      CALL RSPEAK(960)                  ! prayers are not answered
      RETURN
C
1550      OFLAG1(LAMP)=IOR(OFLAG1(LAMP), VISIBT)      ! back to life, restore lamp
      AACTIO(PLAYER)=0                  ! disable dead player
      DEADF=.FALSE.                        ! clear dead flag
      F=MOVETO(FORE1,WINNER)                  ! move to forest
      CALL RSPEAK(9)                        ! describe
      RETURN
C
1600      IF(PRSA/=WALKW) GO TO 1700
      IF(.NOT.FINDXT(PRSO,HERE)) GO TO 10      ! if no exits, don't handle
      IF(XROOM1/=BSHAF) GO TO 10            ! if not bshaft, don't handle
      CALL RSPEAK(962)                  ! can't go and score points
      RETURN
C
1700      IF(PRSA==QUITW) GO TO 10            ! if quit, don't handle
      CALL RSPEAK(963)                  ! can't do it
      RETURN

C
C A2--      Robot.  Process most commands given to robot.
C
2000      IF((PRSA/=RAISEW).OR.(PRSO/=RCAGE)) GO TO 2200
      CFLAG(CEVSPH)=.FALSE.                  ! robot raised cage.
      WINNER=PLAYER                        ! reset for player.
      F=MOVETO(CAGER,WINNER)                  ! move to new room.
      CALL NEWSTA(CAGE,567,CAGER,0,0)            ! install cage in room.
      CALL NEWSTA(ROBOT,0,CAGER,0,0)            ! install robot in room.
      AROOM(AROBOT)=CAGER                  ! also move robot/adv.
      CAGESF=.TRUE.                        ! cage solved.
      OFLAG1(ROBOT)=IAND(OFLAG1(ROBOT), NOT(NDSCBT))
      OFLAG1(SPHER)=IOR(OFLAG1(SPHER), TAKEBT)      ! reset flags.
      PRSCON=0                        ! stop cmd stream.
      RETURN
C
2200      IF((PRSA/=DRINKW).AND.(PRSA/=EATW)) GO TO 2300
      CALL RSPEAK(568)                  ! eat or drink, joke.
      RETURN
C
2300      IF(PRSA/=READW) GO TO 2400            ! read,
      CALL RSPEAK(569)                  ! joke.
      RETURN
C
2400      IF((PRSA==WALKW).OR.(PRSA==TAKEW).OR.(PRSA==DROPW)
     & .OR.(PRSA==PUTW).OR.(PRSA==PUSHW).OR.(PRSA==LEAPW)
     & .OR.(PRSA==TURNW)) GO TO 2500      ! test for robot verb.
      CALL RSPEAK(570)                  ! joke.
      RETURN
C
2500      CALL RSPEAK(930)                  ! buzz, whirr, click!
      GO TO 10                        ! don't handle here.

C AAPPLI, PAGE 3
C
C A3--      Master.  Process most commands given to master.
C
3000      IF(IAND(OFLAG2(QDOOR), OPENBT)/=0) GO TO 3100
      CALL RSPEAK(783)                  ! no master yet.
      RETURN
C
3100      IF(PRSA/=WALKW) GO TO 3200            ! walk?
      I=784                              ! assume wont.
      IF(((HERE==SCORR).AND.
     &      ((PRSO==XNORTH).OR.(PRSO==XENTER))).OR.
     &  ((HERE==NCORR).AND.
     &      ((PRSO==XSOUTH).OR.(PRSO==XENTER))))
     &      I=785                        ! if prison, cant.
      CALL RSPEAK(I)
      RETURN
C
3200      IF((PRSA==STAYW).OR.(PRSA==FOLLOW).OR.(PRSA==KILLW).OR.
     &  (PRSA==MUNGW).OR.(PRSA==ATTACW)) GO TO 10
      IF((PRSA==TAKEW).OR.(PRSA==DROPW).OR.(PRSA==PUTW).OR.
     &  (PRSA==THROWW).OR.(PRSA==PUSHW).OR.(PRSA==TURNW).OR.
     &  (PRSA==SPINW).OR.(PRSA==TRNTOW).OR.(PRSA==OPENW).OR.
     &  (PRSA==CLOSEW)) GO TO 3300            ! master can, politely.
      CALL RSPEAK(786)                  ! master can't.
      RETURN
C
3300      CALL RSPEAK(1057)                  ! polite reply.
      GO TO 10
C
      END

C THIEFD-      Intermove thief demon
C
C Declarations
C
C This routine details on bit 6 of PRSFLG
C
      SUBROUTINE THIEFD
      use state
      use subr
      integer r,i,j,nr,rhere,rmk
      LOGICAL DFLAG,ONCE,QSTILL,WASLIT
C
C Functions AND DATA
C
      QSTILL(R)=(QHERE(STILL,R).OR.(OADV(STILL)==-THIEF))

C THIEFD, PAGE 2
C
      DFLAG=IAND(PRSFLG, 64)/=0            ! set up detail flag.
      ONCE=.FALSE.                        ! init flag.
1025      WASLIT=LIT(HERE)                  ! record if lit.
      RHERE=OROOM(THIEF)                  ! visible pos.
      IF(RHERE/=0) THFPOS=RHERE
C
      IF((THFPOS==HERE).AND..NOT.DEADF) GO TO 1100      ! thief in live win rm?
      IF(THFPOS/=TREAS) GO TO 1400            ! thief not in treas?
C
C Thief is in treasure room, and winner is not.
C
      IF(DFLAG) PRINT 10
10      FORMAT(' THIEFD-- IN TREASURE ROOM')
      IF(RHERE==0) GO TO 1050            ! visible?
      CALL NEWSTA(THIEF,0,0,0,0)            ! yes, vanish.
      RHERE=0
      IF(QSTILL(TREAS)) CALL NEWSTA(STILL,0,0,THIEF,0)
      DO 1040 I=1,OLNT                  ! loop through objects.
        IF(QHERE(I,THFPOS))
     &      OFLAG1(I)=IOR(OFLAG1(I), VISIBT)      ! make objects visible
1040      CONTINUE
1050      I=ROBADV(-THIEF,THFPOS,0,0)            ! drop valuables.
      IF(QHERE(EGG,THFPOS)) OFLAG2(EGG)=IOR(OFLAG2(EGG), OPENBT)
      GO TO 1700

C THIEFD, PAGE 3
C
C Thief and (live) winner in same room.
C
1100      IF(THFPOS==TREAS) GO TO 1700            ! if treas room, nothing.
      IF(IAND(RFLAG(THFPOS), RLIGHT)/=0) GO TO 1400 ! not if light.
      IF(DFLAG) PRINT 20
20      FORMAT(' THIEFD-- IN ADV ROOM')
      IF(THFFLG) GO TO 1300                  ! thief announced?
      IF((RHERE/=0).OR.PROB(70,70))      GO TO 1150      ! if invis and 30%.
      IF(OCAN(STILL)/=THIEF) GO TO 1700      ! abort if no stilletto.
      CALL NEWSTA(THIEF,583,THFPOS,0,0)      ! insert thief into room.
      THFFLG=.TRUE.                        ! thief is announced.
      RETURN
C
1150      IF((RHERE==0).OR.(IAND(OFLAG2(THIEF), FITEBT)==0))
     &      GO TO 1200                  ! if visible and fight.
      IF(WINNIN(THIEF,PLAYER)) GO TO 1175      ! winning?
      CALL NEWSTA(THIEF,584,0,0,0)            ! no, vanish thief.
      OFLAG2(THIEF)=IAND(OFLAG2(THIEF), NOT(FITEBT))
      IF(QSTILL(THFPOS)) CALL NEWSTA(STILL,0,0,THIEF,0)
      RETURN
C
1175      IF(PROB(90,90)) GO TO 1700            ! 90% chance to stay.
C
1200      IF((RHERE==0).OR.PROB(70,70)) GO TO 1250 ! if visible and 30%
      CALL NEWSTA(THIEF,585,0,0,0)            ! vanish thief.
      IF(QSTILL(THFPOS)) CALL NEWSTA(STILL,0,0,THIEF,0)
      RETURN
C
1300      IF(RHERE==0) GO TO 1700            ! announced.  visible?
1250      IF(PROB(70,70)) RETURN                  ! 70% chance to do nothing.
      THFFLG=.TRUE.
      NR=ROBRM(THFPOS,100,0,0,-THIEF)+ROBADV(PLAYER,0,0,-THIEF)
      I=586                              ! robbed em.
      IF(RHERE/=0) I=588                  ! was he visible?
      CALL NEWSTA(THIEF,I+MIN0(1,NR),0,0,0)      ! vanish thief, give result.
      IF(QSTILL(THFPOS))
     &      CALL NEWSTA(STILL,0,0,THIEF,0)      ! reclaim stilletto.
      IF(WASLIT.AND..NOT.LIT(HERE).AND.(HERE==AROOM(PLAYER)))
     &      CALL RSPEAK(915)            ! leave player in dark?
      RHERE=0
      GO TO 1700                        ! onward.

C THIEFD, PAGE 4
C
C Not in adventurers room, or adventurer dead, or room lit.
C
1400      CALL NEWSTA(THIEF,0,0,0,0)            ! vanish.
      RHERE=0
      IF(DFLAG) PRINT 30,THFPOS
30      FORMAT(' THIEFD-- IN ROOM ',I4)
      IF(QSTILL(THFPOS)) CALL NEWSTA(STILL,0,0,THIEF,0)
      IF(IAND(RFLAG(THFPOS), RSEEN)==0) GO TO 1700      ! cant rob unseen.
      RMK=1045                        ! first object to vanish.
      I=ROBRM(THFPOS,75,0,0,-5555)            ! rob room 75% to hyperspace.
      DO 1410 I=1,OLNT                  ! loop through objects.
        IF(OADV(I)/=-5555) GO TO 1410      ! in hyperspace?
        CALL NEWSTA(I,0,0,0,-THIEF)            ! move to thief.
        IF((THFPOS==HERE).AND..NOT.DEADF)      ! thief's remarks.
     &      CALL RSPSUB(RMK,ODESC2(I))
        RMK=1083                        ! for next object.
1410      CONTINUE
C
      IF((THFPOS<MAZE1).OR.(THFPOS>MAZ15).OR.
     &      (HERE<MAZE1).OR.(HERE>MAZ15)) GO TO 1500
      DO 1450 I=1,OLNT                  ! both in maze.
        IF(.NOT.QHERE(I,THFPOS).OR.PROB(60,60).OR.(I==WATER).OR.
     &      (IAND(OFLAG1(I), (VISIBT+TAKEBT))/=(VISIBT+TAKEBT)))
     &      GO TO 1450
        IF(.NOT.DEADF) CALL RSPSUB(590,ODESC2(I))      ! thief's remarks.
        IF(PROB(40,20)) GO TO 1700
        CALL NEWSTA(I,0,0,0,-THIEF)            ! steal it.
        OFLAG2(I)=IOR(OFLAG2(I), TCHBT)
        GO TO 1700
1450      CONTINUE
      GO TO 1700
C
1500      DO 1550 I=1,OLNT                  ! not in maze.
        IF(.NOT.QHERE(I,THFPOS).OR.(OTVAL(I)/=0).OR.
     &      PROB(80,60).OR.(I==WATER).OR.
     &      (IAND(OFLAG1(I), (VISIBT+TAKEBT))/=(VISIBT+TAKEBT)))
     &      GO TO 1550
        CALL NEWSTA(I,0,0,0,-THIEF)
        OFLAG2(I)=IOR(OFLAG2(I), TCHBT)
        IF((THFPOS==HERE).AND..NOT.DEADF)
     &      CALL RSPSUB(RMK,ODESC2(I))      ! vanishes before you.
        GO TO 1700
1550      CONTINUE

C THIEFD, PAGE 5
C
C Now move to new room.
C
1700      IF(OADV(ROPE)/=-THIEF) GO TO 1725      ! did he steal rope?
      DOMEF=.FALSE.
      TTIE=0
1725      IF(ONCE) GO TO 1800
      ONCE=.NOT.ONCE
1750      THFPOS=THFPOS-1                        ! next room.
      IF(THFPOS<=0) THFPOS=RLNT
      IF(IAND(RFLAG(THFPOS), (RLAND+RSACRD+REND))/=RLAND)
     &      GO TO 1750                  ! must be land, profane.
      THFFLG=.FALSE.                        ! not announced.
      GO TO 1025                        ! once more.
C
C All done.
C
1800      IF(THFPOS==TREAS) RETURN            ! in treasure room?
      J=1055                              ! no, drop junky stuff.
      IF(THFPOS/=HERE) J=0
      DO 1850 I=1,OLNT
        IF((OADV(I)/=-THIEF).OR.PROB(70,30).OR.
     &      (OTVAL(I)>0)) GO TO 1850
        CALL NEWSTA(I,J,THFPOS,0,0)
        J=0
1850      CONTINUE
      RETURN
C
      END

      end module
