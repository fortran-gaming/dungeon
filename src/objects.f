C Object processors for DUNGEON
C
C COPYRIGHT 1980, 1990, INFOCOM COMPUTERS AND COMMUNICATIONS, CAMBRIDGE MA.
C ALL RIGHTS RESERVED, COMMERCIAL USAGE STRICTLY PROHIBITED
C WRITTEN BY R. M. SUPNIK

C 02-Dec-15     EMG     Compile using gfortran
C 29-Sep-94      RMS      Fixed bugs in KILL MASTER, palantir, dial button,
C                  well, slide, bat, global brochure, granite wall,
C                  bottle, leaves, broken lamp, beam, robot, thief,
C                  troll, me, subscripting, object substitution.
C                  Added features to heads, coke bottles, balloon,
C                  bucket, stove.
C 30-Jan-94      RMS      Fixed bugs from MS-DOS port.
C 25-Jan-94      RMS      Added ground = sand at sandy beach.
C 30-Jun-92      RMS      Changed file names to lower case.
C
C OAPPLI- Object action routines
C
C Declarations

      module objapp
      implicit none
      
      public:: oappli,thiefp,cyclop,nobjs,trollp,mirpan
      
      contains
      
! OBJACT-- Apply objects from parse vector
      LOGICAL FUNCTION OBJACT()
       use state, only: prsi,prso,oactio

      OBJACT=.TRUE.                        ! assume wins.
      IF(PRSI.EQ.0) GO TO 100                  ! ind object?
      IF(OAPPLI(OACTIO(PRSI),0)) RETURN      ! yes, let it handle.

100      IF(PRSO.EQ.0) GO TO 200                  ! dir object?
      IF(OAPPLI(OACTIO(PRSO),0)) RETURN      ! yes, let it handle.
C
200      OBJACT=.FALSE.                        ! loses.

      END FUNCTION OBJACT
      

      LOGICAL FUNCTION OAPPLI(RI,ARG)
      use state
      use subr
      use io,only: rspeak,rspsub

      integer, intent(in) :: ri,arg


      LOGICAL QOPEN,QON,WASLIT


      LOGICAL F
      integer, PARAMETER :: MXSMP=99
      integer r,av,flobts,i,j,nloc,odi2,odo2

      QOPEN(R)=IAND(OFLAG2(R), OPENBT)/=0
      QON(R)=IAND(OFLAG1(R), ONBT)/=0
C
      IF(RI==0) GO TO 10                  ! zero is false app.
      IF(RI<=MXSMP) GO TO 100            ! simple object?
      ODO2=0
      ODI2=0
      IF((PRSO/=0).AND.(PRSO<=OMAX)) ODO2=ODESC2(PRSO)
      IF(PRSI/=0) ODI2=ODESC2(PRSI)
      AV=AVEHIC(WINNER)
      FLOBTS=FLAMBT+LITEBT+ONBT
      OAPPLI=.TRUE.
      WASLIT=LIT(HERE)
C
      GO TO (2000,5000,10000,11000,12000,15000,18000,
     & 19000,20000,22000,25000,26000,32000,35000,39000,40000,
     & 45000,47000,48000,49000,50000,51000,52000,54000,55000,
     & 56000,57000,58000,59000,60000,61000,62000),
     &      (RI-MXSMP)
      CALL BUG(6,RI)
C
C Return here to declare false result.
C
10      OAPPLI=.FALSE.
      RETURN
C
C Return here to test for light source change.
C
50      IF(WASLIT.AND..NOT.LIT(HERE)) CALL RSPEAK(406)
      RETURN
C
C Simple objects, processed externally.
C
100      IF(RI<32) OAPPLI=SOBJS(RI,ARG)
      IF(RI>=32) OAPPLI=NOBJS(RI,ARG)
      RETURN

C OAPPLI, PAGE 3
C
C O100--      Machine
C
2000      IF(HERE/=MMACH) GO TO 10            ! not here? f
      OAPPLI=OPNCLS(MACHI,123,124)            ! handle opn/cls.
      RETURN
C
C O101--      Water
C
5000      IF(PRSA/=GTHROW) GO TO 5025            ! go through?
      CALL RSPEAK(331+RND(3))                  ! joke.
      RETURN
C
5025      IF((PRSO==WATER).OR.(PRSO==GWATE)) GO TO 5100
      CALL RSPEAK(561)                  ! water is ind obj,
      RETURN                              ! punt.
C
5100      IF(PRSA/=TAKEW) GO TO 5400            ! take water?
      IF(PRSI/=0) GO TO 5200            ! from x?
      IF((OADV(BOTTL)==WINNER).AND.(OCAN(PRSO)/=BOTTL))
     &      GO TO 5500                  ! take, have bottle -> put.
      IF(OCAN(PRSO)/=BOTTL) GO TO 5150      ! water in bottle?
      IF(OADV(BOTTL)/=WINNER) GO TO 5125      ! already have bottle?
      CALL RSPEAK(103)                  ! yes, already have water.
      RETURN
C
5125      PRSO=BOTTL                        ! take bottle.
      GO TO 10                        ! do normal take.
C
5150      IF(OCAN(PRSO)==0) GO TO 5300            ! if not inside, take.
      PRSI=OCAN(PRSO)                        ! make into take from.
      GO TO 5250                        ! check for open.
C
5200      IF(OCAN(PRSO)/=PRSI) GO TO 5250      ! is it inside?
      CALL RSPEAK(1038)                  ! not in that.
      RETURN
C
5250      IF(QOPEN(PRSI)) GO TO 5300            ! is it open?
      CALL RSPSUB(525,ODI2)                  ! no, not open.
      RETURN
C
5300      IF(AV/=0) GO TO 5800                  ! if in vehicle, put there.
      I=615                              ! assume player.
      IF(WINNER/=PLAYER) I=1081
      CALL RSPEAK(I)                        ! slips thru fingers.
      RETURN
C
5400      IF(PRSA/=PUTW) GO TO 5700            ! put water in x?
      IF(PRSI==BOTTL) GO TO 5500            ! in bottle?
      IF((IAND(OFLAG2(PRSI), VEHBT)/=0).OR.
     &  ((AV/=0).AND.(PRSI==AV))) GO TO 5450      ! in veh?
      CALL RSPSUB(297,ODI2)                  ! wont go elsewhere.
      CALL NEWSTA(PRSO,0,0,0,0)            ! vanish water.
      RETURN
C
5450      CALL NEWSTA(WATER,0,0,PRSI,0)            ! water into vehicle.
      CALL RSPSUB(296,ODESC2(PRSI))            ! describe.
      RETURN
C
5500      IF(QOPEN(BOTTL)) GO TO 5550            ! bottle open?
      CALL RSPEAK(612)                  ! no, lose.
      RETURN
C
5550      IF(QEMPTY(BOTTL)) GO TO 5600            ! open, empty?
      CALL RSPEAK(613)                  ! no, already full.
      RETURN
C
5600      CALL NEWSTA(WATER,614,0,BOTTL,0)      ! take water to bottle.
      RETURN
C
5700      IF((PRSA/=DROPW).AND.(PRSA/=POURW))
     &      GO TO 5900                  ! drop, pour?
      IF(AV/=0) GO TO 5800                  ! into vehicle?
      CALL NEWSTA(PRSO,133,0,0,0)            ! no, vanishes.
      RETURN
C
5800      CALL NEWSTA(WATER,0,0,AV,0)            ! water into his vehicle.
      CALL RSPSUB(296,ODESC2(AV))            ! describe.
      RETURN
C
5900      IF(PRSA/=THROWW) GO TO 10            ! last chance, throw?
      CALL NEWSTA(PRSO,132,0,0,0)            ! vanishes.
      RETURN

C OAPPLI, PAGE 4
C
C O102--      Leaf pile
C
10000      IF(PRSA/=BURNW) GO TO 10500            ! burn?
      IF(QOPEN(GRATE).OR.(RVCLR/=0)) GO TO 10050
      RVCLR=1                              ! leaves moved.
      CALL NEWSTA(GRATE,30,HERE,0,0)            ! grating appears.
10050      IF(OADV(PRSO)==PLAYER) GO TO 10100      ! was he carrying?
      CALL NEWSTA(LEAVE,158,0,0,0)            ! no, burn leaves.
      RETURN
C
10100      CALL NEWSTA(LEAVE,0,HERE,0,0)            ! drop leaves.
      IF(HERE==MTREE) CALL NEWSTA(LEAVE,0,FORE3,0,0)
      CALL JIGSUP(159)                  ! burn him.
      RETURN
C
10500      IF(PRSA/=MOVEW) GO TO 10600            ! move?
      CALL RSPEAK(2)                        ! done.
10550      IF(QOPEN(GRATE).OR.(RVCLR/=0)) RETURN      ! done before?
      RVCLR=1                              ! leaves moved.
      CALL NEWSTA(GRATE,30,HERE,0,0)            ! grating appears.
      RETURN
C
10600      IF(PRSA/=TAKEW) GO TO 10700            ! take?
      OAPPLI=.FALSE.                        ! don't handle here.
      GO TO 10550                        ! make grate visible.
C
10700      IF((PRSA/=LOOKUW).OR.QOPEN(GRATE).OR.(RVCLR/=0)) GO TO 10
      CALL RSPEAK(344)                  ! look under?
      RETURN
C
C O103--      Troll, done externally.
C
11000      OAPPLI=TROLLP()                  ! troll processor.
      GO TO 50                        ! go see if now dark.
C
C O104--      Rusty knife.
C
12000      IF(PRSA/=TAKEW) GO TO 12100            ! take?
      IF(OADV(SWORD)==WINNER) CALL RSPEAK(160) ! pulse sword.
      GO TO 10
C
12100      IF((((PRSA/=ATTACW).AND.(PRSA/=KILLW)).OR.
     &      (PRSI/=RKNIF)).AND.
     &  (((PRSA/=SWINGW).AND.(PRSA/=THROWW)).OR.
     &      (PRSO/=RKNIF))) GO TO 10
      CALL NEWSTA(RKNIF,0,0,0,0)            ! kill knife.
      CALL JIGSUP(161)                  ! kill him.
      RETURN

C OAPPLI, PAGE 5
C
C O105--      Glacier
C
15000      IF(PRSA/=THROWW) GO TO 15500            ! throw?
      IF(PRSO/=TORCH) GO TO 15400            ! torch?
      CALL NEWSTA(ICE,169,0,0,0)            ! melt ice.
      ODESC1(TORCH)=174                  ! mung torch.
      ODESC2(TORCH)=173
      OFLAG1(TORCH)=IAND(OFLAG1(TORCH), NOT(FLOBTS))
      CALL NEWSTA(TORCH,0,STREA,0,0)            ! move torch.
      GLACRF=.TRUE.				! glacier gone.
	IF(.NOT.LIT(HERE)) CALL RSPEAK(170)	! in dark?
	RETURN
C
15400	CALL RSPEAK(171)			! joke if not torch.
	GO TO 10				! don't handle.
C
15500	IF((PRSA/=MELTW).OR.(PRSO/=ICE)) GO TO 10
	IF(IAND(OFLAG1(PRSI), FLOBTS)==FLOBTS) GO TO 15600
	CALL RSPSUB(298,ODI2)			! cant melt with that.
	RETURN
C
15600	GLACMF=.TRUE.				! partial melt.
	IF(PRSI/=TORCH) GO TO 15700		! melt with torch?
	ODESC1(TORCH)=174			! mung torch.
	ODESC2(TORCH)=173
	OFLAG1(TORCH)=IAND(OFLAG1(TORCH), NOT(FLOBTS))
15700	CALL JIGSUP(172)			! drown.
	RETURN
C
C O106--	Black book
C
18000	IF(PRSA/=OPENW) GO TO 18100		! open?
	CALL RSPEAK(180)			! joke.
	RETURN
C
18100	IF(PRSA/=CLOSEW) GO TO 18200		! close?
	CALL RSPEAK(181)
	RETURN
C
18200	IF(PRSA/=BURNW) GO TO 10		! burn?
	CALL NEWSTA(BOOK,0,0,0,0)		! vanish book.
	CALL JIGSUP(182)			! fatal joke.
	RETURN

C OAPPLI, PAGE 6
C
C O107--	Candles
C
19000	IF(ORCAND/=0) GO TO 19100		! first ref?
	ORCAND=1				! yes, candles are
	CFLAG(CEVCND)=.TRUE.
	CTICK(CEVCND)=50			! burning when seen.
C
19100	IF(PRSI==CANDL) GO TO 10		! ignore ind refs.
	IF(PRSA/=TRNOFW) GO TO 19200		! turn off?
	I=513					! assume off.
	IF(QON(CANDL)) I=514			! if on, different.
	CFLAG(CEVCND)=.FALSE.			! disable countdown.
	OFLAG1(CANDL)=IAND(OFLAG1(CANDL), NOT(ONBT))
	CALL RSPEAK(I)
	GO TO 50				! go see if now dark.
C
19200	IF((PRSA/=BURNW).AND.(PRSA/=TRNONW)) GO TO 10
	IF(IAND(OFLAG1(CANDL), LITEBT)/=0) GO TO 19300
	CALL RSPEAK(515)			! candles too short.
	RETURN
C
19300	IF(PRSI/=0) GO TO 19400		! any flame?
	CALL RSPEAK(516)			! no, lose.
	CALL ORPHAN(-1,ACT,PREP1,CANDL,2,' ',0,0) ! orphan "light candle with"
	PRSWON=.FALSE.
	PRSCON=0
      RETURN
C
19400      IF((PRSI/=MATCH).OR. .NOT.QON(MATCH)) GO TO 19500
      I=517                              ! assume off.
      IF(QON(CANDL)) I=518                  ! if on, joke.
      OFLAG1(CANDL)=IOR(OFLAG1(CANDL), ONBT)      ! lite candles.
      CFLAG(CEVCND)=.TRUE.                  ! resume countdown.
      CALL RSPEAK(I)
      RETURN
C
19500      IF((PRSI/=TORCH).OR. .NOT.QON(TORCH)) GO TO 19600
      IF(QON(CANDL)) GO TO 19700            ! already on?
      CALL NEWSTA(CANDL,521,0,0,0)            ! no, vaporize.
      RETURN
C
19600      CALL RSPEAK(519)                  ! cant light with that.
      RETURN
C
19700      CALL RSPEAK(520)                  ! already on.
      RETURN
C
C O108--      Matches
C
20000      IF((PRSA/=TRNONW).OR.(PRSO/=MATCH)) GO TO 20500
      IF(ORMTCH/=0) GO TO 20100            ! any matches left?
      CALL RSPEAK(183)                  ! no, lose.
      RETURN
C
20100      ORMTCH=ORMTCH-1                        ! decrement no matches.
      OFLAG1(MATCH)=IOR(OFLAG1(MATCH), FLOBTS)
      CFLAG(CEVMAT)=.TRUE.
      CTICK(CEVMAT)=2                        ! countdown.
      CALL RSPEAK(184)
      RETURN
C
20500      IF((PRSA/=TRNOFW).OR.(IAND(OFLAG1(MATCH), ONBT)==0))
     &      GO TO 10                  ! extinguish?
      OFLAG1(MATCH)=IAND(OFLAG1(MATCH), NOT(FLOBTS))
      CTICK(CEVMAT)=0
      CALL RSPEAK(185)
      GO TO 50                        ! go see if now dark.
C
C O109--      Cyclops, processed externally.
C
22000      OAPPLI=CYCLOP()                  ! cyclops
      GO TO 50                        ! go see if now dark.
C
C O110--      Thief, processed externally.
C
25000      OAPPLI=THIEFP()
      GO TO 50                        ! go see if now dark.
C
C O111--      Window
C
26000      OAPPLI=OPNCLS(WINDO,208,209)            ! open/cls window.
      RETURN
C
C O112--      Pile of bodies
C
32000      IF(PRSA/=TAKEW) GO TO 32500            ! take?
      CALL RSPEAK(228)                  ! cant.
      RETURN
C
32500      IF((PRSA/=BURNW).AND.(PRSA/=MUNGW)) GO TO 10
      IF(ONPOLF) RETURN                  ! burn or mung?
      ONPOLF=.TRUE.                        ! set head on pole.
      CALL NEWSTA(HPOLE,0,LLD2,0,0)
      CALL JIGSUP(229)                  ! beheaded.
      RETURN
C
C O113--      Vampire bat
C
35000      CALL RSPEAK(50)                        ! time to fly, jack.
      F=MOVETO(BATDRP(RND(9)+1),WINNER)      ! select random dest.
      F=RMDESC(0)
      PRSCON=0                        ! disable parser.
      RETURN

C OAPPLI, PAGE 7
C
C O114--      Stick
C
39000      IF(PRSA/=WAVEW) GO TO 10            ! wave?
      IF(HERE==MRAIN) GO TO 39500            ! on rainbow?
      IF((HERE==POG).OR.(HERE==FALLS)) GO TO 39200
      CALL RSPEAK(244)                  ! nothing happens.
      RETURN
C
39200      OFLAG1(POT)=IOR(OFLAG1(POT), VISIBT)      ! make gold visible.
      RAINBF=.NOT. RAINBF                  ! complement rainbow.
      I=245                              ! assume off.
      IF(RAINBF) I=246                  ! if on, solid.
      CALL RSPEAK(I)                        ! describe.
      RETURN
C
39500      RAINBF=.FALSE.                        ! on rainbow,
      CALL JIGSUP(247)                  ! take a fall.
      RETURN
C
C O115--      Balloon, handled externally.
C
40000      OAPPLI=BALLOP(ARG)
      RETURN
C
C O116--      Heads
C
45000      IF(PRSA/=HELLOW) GO TO 45100            ! hello heads?
      CALL RSPEAK(633)                  ! truly bizarre.
      RETURN
C
45100      IF((PRSA/=KILLW).AND.(PRSA/=MUNGW).AND.
     &  (PRSA/=RUBW).AND.(PRSA/=OPENW).AND.
     &  (PRSA/=TAKEW).AND.(PRSA/=BURNW).AND.(PRSA/=SPINW).AND.
     &  (PRSA/=ATTACW).AND.(PRSA/=KICKW)) GO TO 10
      CALL RSPEAK(260)                  ! bad news for player
      I=ROBADV(WINNER,0,LCASE,0)+ROBRM(HERE,100,0,LCASE,0)
      IF(I/=0) CALL NEWSTA(LCASE,0,LROOM,0,0) ! if robbed, make large case.
      CALL JIGSUP(261)                  ! kill him.
      RETURN

C OAPPLI, PAGE 8
C
C O117--      Sphere
C
47000      IF(CAGESF.OR.(PRSA/=TAKEW)) GO TO 47600 ! take?
      IF(WINNER/=PLAYER) GO TO 47500      ! robot take?
      CALL RSPEAK(263)                  ! no, drop cage.
      IF(OROOM(ROBOT)/=HERE) GO TO 47200      ! robot here?
      F=MOVETO(CAGED,WINNER)                  ! yes, move into cage.
      CALL NEWSTA(ROBOT,0,CAGED,0,0)            ! move robot.
      AROOM(AROBOT)=CAGED
      OFLAG1(ROBOT)=IOR(OFLAG1(ROBOT), NDSCBT)      ! don't describe robot.
      CFLAG(CEVSPH)=.TRUE.
      CTICK(CEVSPH)=10                  ! get out in 10 or else.
      RETURN
C
47200      CALL NEWSTA(SPHER,0,0,0,0)            ! you're dead.
      RFLAG(CAGER)=IOR(RFLAG(CAGER), RMUNG)      ! mung cage room.
      RDESC1(CAGER)=147
      CALL JIGSUP(148)                  ! mung player.
      RETURN
C
47500      CALL NEWSTA(SPHER,0,0,0,0)            ! robot tried,
      CALL NEWSTA(ROBOT,264,0,0,0)            ! kill him.
      CALL NEWSTA(CAGE,0,HERE,0,0)            ! insert mangled cage.
      GO TO 50                        ! go see if now dark.
C
47600      IF(PRSA/=LOOKIW) GO TO 10            ! look in?
      OAPPLI=NOBJS(OACTIO(PALAN),ARG)            ! do palantir function.
      RETURN
C
C O118--      Geometrical buttons
C
48000      IF(PRSA/=PUSHW) GO TO 10            ! push?
      I=PRSO-SQBUT+1                        ! get button index.
      IF((I<=0).OR.(I>=4)) GO TO 10      ! a button?
      IF(WINNER/=PLAYER) GO TO (48100,48200,48300),I
      CALL JIGSUP(265)                  ! you pushed, you die.
      RETURN
C
48100      I=267                              ! square, speed up.
      IF(CAROZF) I=266
      CAROZF=.TRUE.
      CALL RSPEAK(I)
      RETURN
C
48200      I=266                              ! round, slow down.
      IF(CAROZF) I=268
      CAROZF=.FALSE.
      CALL RSPEAK(I)
      RETURN
C
48300      CAROFF=.NOT.CAROFF                  ! triangle, flip carousel.
      IF(OROOM(IRBOX)/=CAROU) GO TO 48400      ! iron box in carousel?
      CALL RSPEAK(269)                  ! yes, thump.
      OFLAG1(IRBOX)=IEOR(OFLAG1(IRBOX), VISIBT)      ! complement visibility.
      IF(CAROFF) RFLAG(CAROU)=IAND(RFLAG(CAROU), NOT(RSEEN))
      RETURN
C
48400      CALL RSPEAK(232)                  ! click.
      RETURN
C
C O119--      Flask function
C
49000      IF(PRSA==OPENW) GO TO 49100            ! open?
      IF((PRSA/=MUNGW).AND.(PRSA/=THROWW)) GO TO 10
      CALL NEWSTA(FLASK,270,0,0,0)            ! kill flask.
49100      RFLAG(HERE)=IOR(RFLAG(HERE), RMUNG)      ! mung room.
      RDESC1(HERE)=271
      CALL JIGSUP(272)                  ! poisoned.
      RETURN
C
C O120--      Bucket function
C
50000      IF(ARG==1) GO TO 10                  ! read in?
      IF(ARG==2) GO TO 50400            ! read out?
      IF(PRSA/=BURNW) GO TO 50100            ! burn?
      CALL RSPEAK(928)                  ! can't.
      RETURN
C
50100      IF(PRSA/=KICKW) GO TO 10            ! kick?
      CALL JIGSUP(1067)                  ! dead.
      RETURN
C
50400      IF((OCAN(WATER)/=BUCKE).OR.BUCKTF) GO TO 50500
      BUCKTF=.TRUE.                        ! bucket at top.
      CFLAG(CEVBUC)=.TRUE.
      CTICK(CEVBUC)=100                  ! start countdown.
      CALL NEWSTA(BUCKE,290,TWELL,0,0)      ! reposition bucket.
      GO TO 50900                        ! finish up.
C
50500      IF((OCAN(WATER)==BUCKE).OR..NOT.BUCKTF) GO TO 10
      BUCKTF=.FALSE.
      CALL NEWSTA(BUCKE,291,BWELL,0,0)      ! bucket at bottom.
50900      IF(AV/=BUCKE) RETURN                  ! not in bucket?
      F=MOVETO(OROOM(BUCKE),WINNER)            ! move adventurer.
      F=RMDESC(0)                        ! describe room.
      RETURN

C OAPPLI, PAGE 9
C
C O121--      Eatme cake
C
51000      IF((PRSA/=EATW).OR.(PRSO/=ECAKE).OR.
     &      (HERE/=ALICE)) GO TO 10      ! eat cake in aliceroom?
      CALL NEWSTA(ECAKE,273,0,0,0)            ! vanish cake.
      OFLAG1(ROBOT)=IAND(OFLAG1(ROBOT), NOT(VISIBT))      ! vanish robot.
      DO 51100 I=1,OLNT                  ! make objects big.
        IF((OROOM(I)/=ALICE).OR.(OSIZE(I)==10000))
     &      GO TO 51100
        OSIZE(I)=OSIZE(I)*64
        OROOM(I)=ALISM
51100      CONTINUE
      OAPPLI=MOVETO(ALISM,WINNER)            ! move to alice small.
      RETURN
C
C O122--      Icings
C
52000      IF(PRSA/=READW) GO TO 52200            ! read?
      I=274                              ! cant read.
      IF(PRSI/=0) I=275                  ! through something?
      IF(PRSI==BOTTL) I=276                  ! through bottle?
      IF(PRSI==FLASK) I=277+(PRSO-ORICE)      ! through flask?
      CALL RSPEAK(I)                        ! read flask.
      RETURN
C
52200      IF((PRSA/=THROWW).OR.(PRSO/=RDICE).OR.(PRSI/=POOL))
     &      GO TO 52300                  ! throw rdice at pool?
      CALL NEWSTA(POOL,280,0,0,0)            ! vanish pool.
      OFLAG1(SAFFR)=IOR(OFLAG1(SAFFR), VISIBT)      ! materialize spices.
      RETURN
C
52300      IF((HERE/=ALICE).AND.(HERE/=ALISM).AND.(HERE/=ALITR))
     &      GO TO 10                  ! in wonderland?
      IF(((PRSA/=EATW).AND.(PRSA/=THROWW)).OR.
     &      (PRSO/=ORICE)) GO TO 52400      ! throw orange ice?
      CALL NEWSTA(ORICE,0,0,0,0)            ! vanish orange ice.
      RFLAG(HERE)=IOR(RFLAG(HERE), RMUNG)      ! vanish room.
      RDESC1(HERE)=281
      CALL JIGSUP(282)                  ! vanish adventurer.
      RETURN
C
52400      IF((PRSA/=EATW).OR.(PRSO/=BLICE))
     &      GO TO 10                  ! eat blue ice?
      CALL NEWSTA(BLICE,283,0,0,0)            ! vanish blue ice.
      IF(HERE/=ALISM) GO TO 52500            ! in reduced room?
      OFLAG1(ROBOT)=IOR(OFLAG1(ROBOT), VISIBT)      ! materialize robot.
      DO 52450 I=1,OLNT                  ! enlarge world.
        IF((OROOM(I)/=HERE).OR.(OSIZE(I)==10000))
     &      GO TO 52450
        OROOM(I)=ALICE
        OSIZE(I)=OSIZE(I)/64
52450      CONTINUE
      OAPPLI=MOVETO(ALICE,WINNER)            ! return
      RETURN
C
52500      CALL JIGSUP(284)                  ! enlarged in wrong room.
      RETURN
C
C O123--      Brick
C
54000      IF(PRSA/=BURNW) GO TO 10            ! burn?
      CALL NEWSTA(BRICK,0,0,0,0)            ! vanish brick.
      CALL JIGSUP(150)                  ! boom!
      RETURN
C
C O124--      Myself
C
55000      IF((PRSA/=GIVEW).OR.
     & (IAND(OFLAG2(PRSO), NOCHBT)/=0)) GO TO 55100      ! give?
      IF(PRSO/=WATER) GO TO 55050            ! water?
      CALL NEWSTA(WATER,615,0,0,0)            ! slips through fingers.
      RETURN
C
55050      CALL NEWSTA(PRSO,2,0,0,PLAYER)            ! done.
      RETURN
C
55100      IF(PRSA/=TAKEW) GO TO 55200            ! take?
      CALL RSPEAK(286)                  ! joke.
      RETURN
C
55200      IF(((PRSA/=KILLW).AND.(PRSA/=MUNGW))
     &      .OR.(PRSO/=OPLAY)) GO TO 10
      WINNER=PLAYER                        ! can't kill someone else.
      CALL JIGSUP(287)                  ! kill, no joke.
      RETURN

C OAPPLI, PAGE 10
C
C O125--      Panels inside mirror
C
56000      IF(PRSA/=PUSHW) GO TO 10            ! push?
      IF(POLEUF/=0) GO TO 56100            ! short pole up?
      I=731                              ! no, wont budge.
      IF(MOD(MDIR,180)==0) I=732            ! diff msg if n-s.
      CALL RSPEAK(I)                        ! tell wont move.
      RETURN
C
56100      IF(MLOC/=MRG) GO TO 56200            ! in gdn room?
      CALL RSPEAK(733)                  ! you lose.
      CALL JIGSUP(685)
      RETURN
C
56200      I=831                              ! rotate l or r.
      IF((PRSO==RDWAL).OR.(PRSO==YLWAL)) I=830
      CALL RSPEAK(I)                        ! tell direction.
      MDIR=MOD(MDIR+45+(270*(I-830)),360)      ! calculate new dir.
      CALL RSPSUB(734,695+(MDIR/45))            ! tell new dir.
      IF(WDOPNF) CALL RSPEAK(730)            ! if panel open, close.
      WDOPNF=.FALSE.
      RETURN                              ! done.
C
C O126--      Ends inside mirror
C
57000      IF(PRSA/=PUSHW) GO TO 10            ! push?
      IF(MOD(MDIR,180)==0) GO TO 57100      ! mirror n-s?
      CALL RSPEAK(735)                  ! no, wont budge.
      RETURN
C
57100      IF(PRSO/=PINDR) GO TO 57300            ! push pine wall?
      IF(((MLOC==MRC).AND.(MDIR==180)).OR.
     &  ((MLOC==MRD).AND.(MDIR==0)).OR.
     &   (MLOC==MRG)) GO TO 57200            ! in view of gdn?
      CALL RSPEAK(736)                  ! no, opens.
      WDOPNF=.TRUE.                        ! indicate open.
      CFLAG(CEVPIN)=.TRUE.                  ! time opening.
      CTICK(CEVPIN)=5
      RETURN
C
57200      CALL RSPEAK(737)                  ! gdn sees you, die.
      CALL JIGSUP(685)
      RETURN
C
57300      NLOC=MLOC-1                        ! new loc if south.
      IF(MDIR==0) NLOC=MLOC+1            ! new loc if north.
      IF((NLOC>=MRA).AND.(NLOC<=MRD)) GO TO 57400
      CALL RSPEAK(738)                  ! have reached end.
      RETURN
C
57400      I=699                              ! assume south.
      IF(MDIR==0) I=695                  ! north.
      J=739                              ! assume smooth.
      IF(POLEUF/=0) J=740                  ! pole up, wobbles.
      CALL RSPSUB(J,I)                  ! describe.
      MLOC=NLOC
      IF(MLOC/=MRG) RETURN                  ! now in gdn room?
C
      IF(POLEUF/=0) GO TO 57500            ! pole up, gdn sees.
      IF(MROPNF.OR.WDOPNF) GO TO 57600      ! door open, gdn sees.
      IF(MR1F.AND.MR2F) RETURN            ! mirrors intact, ok.
      CALL RSPEAK(742)                  ! mirrors broken, die.
      CALL JIGSUP(743)
      RETURN
C
57500      CALL RSPEAK(741)                  ! pole up, die.
      CALL JIGSUP(743)
      RETURN
C
57600      CALL RSPEAK(744)                  ! door open, die.
      CALL JIGSUP(743)
      RETURN

C OAPPLI, PAGE 11
C
C O127--      Global guardians
C
58000      IF((PRSA/=ATTACW).AND.(PRSA/=KILLW).AND.
     &  (PRSA/=MUNGW)) GO TO 58100            ! aggressive?
      CALL JIGSUP(745)                  ! lose.
      RETURN
C
58100      IF(PRSA/=HELLOW) GO TO 10            ! hello?
      CALL RSPEAK(746)                  ! no reply.
      RETURN
C
C O128--      Global master
C
59000      IF(((PRSA/=ATTACW).AND.(PRSA/=KILLW).AND.(PRSA/=MUNGW))
     &      .OR.(PRSO/=MASTER).OR.(PRSI==MASTER))
     &      GO TO 59100                  ! kill master?
      WINNER=PLAYER                        ! rebounds on player.
      CALL JIGSUP(747)                  ! bad idea.
      RETURN
C
59100      IF(PRSA/=TAKEW) GO TO 10            ! take?
      CALL RSPEAK(748)                  ! joke.
      RETURN
C
C O129--      Numeral five
C
60000      IF(PRSA/=TAKEW) GO TO 10            ! take five?
      CALL RSPEAK(419)                  ! time passes.
      DO 60100 I=1,3                        ! wait a while.
        IF(CLOCKD()) RETURN
60100      CONTINUE
      RETURN
C
C O130--      Crypt function
C
61000      IF(.NOT.ENDGMF) GO TO 45000            ! if not eg, die.
      IF(PRSA/=OPENW) GO TO 61100            ! open?
      I=793
      IF(QOPEN(TOMB)) I=794
      CALL RSPEAK(I)
      OFLAG2(TOMB)=IOR(OFLAG2(TOMB), OPENBT)      ! now tomb with view.
      RETURN
C
61100      IF(PRSA/=CLOSEW) GO TO 45000            ! close?
      I=795
      IF(QOPEN(TOMB)) I=796
      CALL RSPEAK(I)
      OFLAG2(TOMB)=IAND(OFLAG2(TOMB), NOT(OPENBT))
      IF(HERE/=CRYPT) RETURN
      CFLAG(CEVSTE)=.TRUE.
      CTICK(CEVSTE)=3                        ! if in crypt, start eg.
      RETURN

C OAPPLI, PAGE 12
C
C O131--      Global ladder
C
62000      IF((CPVEC(CPHERE+1)==-2).OR.(CPVEC(CPHERE-1)==-3))
     &      GO TO 62100                  ! ladder here?
      CALL RSPEAK(865)                  ! no, lose.
      RETURN
C
62100      IF((PRSA==CLMBW).OR.(PRSA==CLMBUW)) GO TO 62200
      CALL RSPEAK(866)                  ! climb it?
      RETURN
C
62200      IF((CPHERE==10).AND.(CPVEC(CPHERE+1)==-2))
     &      GO TO 62300                  ! at exit?
      CALL RSPEAK(867)                  ! no, hit your head.
      RETURN
C
62300      F=MOVETO(CPANT,WINNER)                  ! to anteroom.
      F=RMDESC(3)                        ! describe.
      RETURN
C
      END

C SOBJS-      Simple objects processor
C
C Declarations
C
      LOGICAL FUNCTION SOBJS(RI,ARG)
      use state
      use subr
      use io,only: rspsub,rspeak
      integer,intent(in) :: ri,arg

      LOGICAL WASLIT,F,QOPEN
      integer r,av,i,mroom,odi2,odo2
C
C Functions and data
C
      QOPEN(R)=IAND(OFLAG2(R), OPENBT)/=0
C
      ODO2=0
      ODI2=0
      IF((PRSO/=0).AND.(PRSO<=OMAX)) ODO2=ODESC2(PRSO)
      IF(PRSI/=0) ODI2=ODESC2(PRSI)
      AV=AVEHIC(WINNER)
      SOBJS=.TRUE.
      WASLIT=LIT(HERE)
C
      GO TO (1000,3000,4000,6000,7000,8000,9000,
     & 13000,14000,16000,17000,
     & 21000,23000,24000,27000,28000,29000,30000,
     & 31000,33000,34000,36000,37000,38000,
     & 41000,42000,43000,44000,46000,
     & 53000,56000)
     &      RI
      CALL BUG(6,RI)
C
C Return here to declare false result.
C
10      SOBJS=.FALSE.
      RETURN
C
C Return here to test for light source change.
C
50      IF(WASLIT.AND..NOT.LIT(HERE)) CALL RSPEAK(406)
      RETURN

C SOBJS, PAGE 3
C
C O1--      Gunk
C
1000      IF(OCAN(GUNK)==0) GO TO 10            ! not inside? f
      CALL NEWSTA(GUNK,122,0,0,0)            ! falls apart.
      RETURN
C
C O2--      Trophy case
C
3000      IF(PRSA/=TAKEW) GO TO 10            ! take?
      CALL RSPEAK(128)                  ! cant.
      RETURN
C
C O3--      Bottle
C
4000      IF((PRSA/=THROWW).OR.(PRSO/=BOTTL)) GO TO 4100      ! throw?
      CALL NEWSTA(BOTTL,129,0,0,0)            ! breaks.
      RETURN
C
4100      IF(PRSA/=MUNGW) GO TO 10            ! mung?
      CALL NEWSTA(BOTTL,131,0,0,0)            ! breaks.
      RETURN

C SOBJS, PAGE 4
C
C O4--      Rope
C
6000      IF((HERE==DOME).OR.(HERE==SLIDE).OR.(PRSI==0).OR.
     &  (PRSI==TIMBE).OR.(PRSI==COFFI)) GO TO 6100
      IF(PRSA==TIEW) CALL RSPEAK(135)      ! tie, cant do it.
6050      DOMEF=.FALSE.                        ! not tied in dome.
      TTIE=0                              ! not tied to timber.
      OFLAG1(TIMBE)=IAND(OFLAG1(TIMBE), NOT(NDSCBT))
      OFLAG1(COFFI)=IAND(OFLAG1(COFFI), NOT(NDSCBT))
      ODESC1(TIMBE)=1032                  ! restore timber, coffin
      ODESC1(COFFI)=1033
      OFLAG1(ROPE)=IAND(OFLAG1(ROPE), NOT(NDSCBT))
      OFLAG2(ROPE)=IAND(OFLAG2(ROPE), NOT(CLMBBT))
      RETURN                              ! rope not climbable
C
6100      IF((PRSA/=CLMBDW).OR.(HERE/=CPANT)) GO TO 6200
      IF(TTIE==0) GO TO 6150            ! climb down, tied?
      IF(OROOM(TTIE)/=HERE) GO TO 6150      ! tied here?
      CALL RSPSUB(1028,ODESC2(TTIE))            ! yes, tumbles after you.
      CALL NEWSTA(ROPE,0,CPUZZ,0,0)            ! now in puzzle room.
      CALL NEWSTA(TTIE,0,CPUZZ,0,0)
      GO TO 10                        ! not handled here.
C
6150      CALL RSPEAK(1029)                  ! not tied.
      GO TO 10                        ! not handled here.
C
6200      IF(PRSA/=TIEW) GO TO 6400            ! tie rope?
      IF(PRSI/=RAILI) GO TO 6300            ! to railing?
      IF(DOMEF.OR.(TTIE/=0)) GO TO 6250      ! already tied?
      DOMEF=.TRUE.                        ! no, now tied.
      CALL NEWSTA(ROPE,137,DOME,0,0)            ! put in dome room.
6225      OFLAG1(ROPE)=IOR(OFLAG1(ROPE), NDSCBT)
      OFLAG2(ROPE)=IOR(OFLAG2(ROPE), CLMBBT)      ! now climbable
      RETURN
C
6250      CALL RSPEAK(136)                  ! already tied.
      RETURN
C
6300      IF((PRSI/=TIMBE).AND.(PRSI/=COFFI)) GO TO 10
      IF(DOMEF.OR.(TTIE/=0)) GO TO 6250      ! already done?
      IF(OROOM(PRSI)/=0) GO TO 6350            ! target on ground?
      CALL RSPEAK(1025)                  ! too clumsy.
      RETURN
C
6350      CALL RSPSUB(961,ODI2)                  ! now tied to object.
      TTIE=PRSI
      IF(PRSI==TIMBE) ODESC1(TIMBE)=1030      ! change description
      IF(PRSI==COFFI) ODESC1(COFFI)=1031      ! of target.
      IF(HERE==CPANT) CALL RSPEAK(1056)      ! room-specific words.
      IF(HERE==SLIDE) CALL RSPEAK(339)
      IF(HERE==SLIDE) OFLAG1(PRSI)=IOR(OFLAG1(PRSI), NDSCBT)
      CALL NEWSTA(ROPE,0,HERE,0,0)            ! put rope in room.
      GO TO 6225                        ! rope now climbable.
C
6400      IF(PRSA/=UNTIEW) GO TO 6600            ! untie rope?
      IF(.NOT.DOMEF.AND.(TTIE==0)) GO TO 6500 ! tied to obj or railing?
      CALL RSPEAK(139)                  ! report and then
      GO TO 6050                        ! clean up all status.
C
6500      CALL RSPEAK(134)                  ! not tied to anything.
      RETURN
C
6600      IF(DOMEF.OR.(PRSA/=DROPW).OR.
     &      (HERE/=DOME)) GO TO 6700      ! drop & untied from dome?
      CALL NEWSTA(ROPE,140,MTORC,0,0)            ! yes, drop.
      RETURN
C
6700      IF((PRSA/=TAKEW).OR..NOT.DOMEF) GO TO 6800
      CALL RSPEAK(141)                  ! take & tied.
      RETURN
C
6800      IF((PRSA/=TAKEW).OR.(TTIE==0)) GO TO 10
      CALL RSPSUB(926,ODESC2(TTIE))            ! take & tied.
      RETURN
C
C O5--      Sword
C
7000      IF((PRSA==TAKEW).AND.(WINNER==PLAYER))
     &      SWDACT=.TRUE.                  ! turn on demon.
      GO TO 10
C
C O6--      Lantern
C
8000      IF((PRSA/=THROWW).OR.(PRSO/=LAMP)) GO TO 8100      ! throw?
      CALL NEWSTA(LAMP,0,0,0,0)            ! kill lamp,
      CALL NEWSTA(BLAMP,142,HERE,0,0)            ! replace with broken.
      IF(HERE==MTREE) CALL NEWSTA(BLAMP,0,FORE3,0,0)
      IF(LASTIT==LAMP) LASTIT=BLAMP            ! fix last it reference.
      CFLAG(CEVLNT)=.FALSE.                  ! turn off timer.
      GO TO 50                        ! go see if now dark.
C
8100      IF(PRSA==TRNONW) CFLAG(CEVLNT)=.TRUE.
      IF(PRSA==TRNOFW) CFLAG(CEVLNT)=.FALSE.
      GO TO 10

! O7--      Rug

9000      IF(PRSA/=RAISEW) GO TO 9100            ! raise?
      CALL RSPEAK(143)                  ! cant
      RETURN

9100      IF(PRSA/=TAKEW) GO TO 9200            ! take?
      CALL RSPEAK(144)                  ! cant
      RETURN
C
9200      IF(PRSA/=MOVEW) GO TO 9300            ! move?
      CALL RSPEAK(145+ORRUG)
      ORRUG=1
      OFLAG1(DOOR)=IOR(OFLAG1(DOOR), VISIBT)      ! reveal door.
      RETURN
C
9300      IF((PRSA/=LOOKUW).OR.(ORRUG/=0).OR.
     &      QOPEN(DOOR)) GO TO 10            ! look under rug?
      CALL RSPEAK(345)
      RETURN

C SOBJS, PAGE 5
C
C O8--      Skeleton
C
13000      I=ROBRM(HERE,100,LLD2,0,0)+ROBADV(WINNER,LLD2,0,0)
      CALL RSPEAK(162)                  ! curses.
      RETURN
C
C O9--      Mirror
C
14000      IF(MIRRMF.OR.(PRSA/=RUBW)) GO TO 14500
      MROOM=IEOR(HERE, 1)                  ! calculate new rm.
      DO 14100 I=1,OLNT                  ! interchange objs.
        IF(OROOM(I)==HERE) OROOM(I)=-1
        IF(OROOM(I)==MROOM) OROOM(I)=HERE
        IF(OROOM(I)==-1) OROOM(I)=MROOM
14100      CONTINUE
      F=MOVETO(MROOM,WINNER)
      CALL RSPEAK(163)                  ! shake world.
      RETURN
C
14500      IF((PRSA/=LOOKW).AND.(PRSA/=LOOKIW).AND.
     &      (PRSA/=EXAMIW)) GO TO 14600
      I=164                              ! mirror ok.
      IF(MIRRMF) I=165                  ! mirror dead.
      CALL RSPEAK(I)
      RETURN
C
14600      IF(PRSA/=TAKEW) GO TO 14700            ! take?
      CALL RSPEAK(166)                  ! joke.
      RETURN
C
14700      IF((PRSA/=MUNGW).AND.(PRSA/=THROWW)) GO TO 10
      I=167                              ! mirror breaks.
      IF(MIRRMF) I=168                  ! mirror already broken.
      MIRRMF=.TRUE.
      BADLKF=.TRUE.
      CALL RSPEAK(I)
      RETURN

C SOBJS, PAGE 6
C
C O10--      Dumbwaiter
C
16000      IF(PRSA/=RAISEW) GO TO 16100            ! raise?
      IF(CAGETF) GO TO 16400                  ! already at top?
      CALL NEWSTA(TBASK,175,TSHAF,0,0)      ! no, raise basket.
      CALL NEWSTA(FBASK,0,BSHAF,0,0)
      IF(HERE==TSHAF) LASTIT=TBASK            ! fix last it reference.
      IF(HERE==BSHAF) LASTIT=FBASK
      CAGETF=.TRUE.                        ! at top.
      RETURN
C
16100      IF(PRSA/=LOWERW) GO TO 16200            ! lower?
      IF(.NOT.CAGETF) GO TO 16400            ! already at bottom?
      CALL NEWSTA(TBASK,176,BSHAF,0,0)      ! no, lower basket.
      CALL NEWSTA(FBASK,0,TSHAF,0,0)
      IF(HERE==TSHAF) LASTIT=FBASK            ! fix last it reference.
      IF(HERE==BSHAF) LASTIT=TBASK
      CAGETF=.FALSE.
      GO TO 50                        ! go see if now dark.
C
16200      IF((PRSO/=FBASK).AND.(PRSI/=FBASK)) GO TO 16300
      CALL RSPEAK(130)                  ! wrong basket.
      RETURN
C
16300      IF(PRSA/=TAKEW) GO TO 10            ! take?
      CALL RSPEAK(177)                  ! joke.
      RETURN
C
16400      CALL RSPEAK(125+RND(3))                  ! dummy.
      RETURN
C
C O11--      Ghost
C
17000      IF(PRSO/=GHOST) GO TO 17100            ! direct object?
      CALL RSPEAK(178)                  ! joke.
      RETURN
C
17100      CALL RSPEAK(179)                  ! joke.
      GO TO 10                        ! don't handle.

C SOBJS, PAGE 7
C
C O12--      Tube
C
21000      IF((PRSA/=PUTW).OR.(PRSI/=TUBE)) GO TO 21100
      CALL RSPEAK(186)                  ! cant put back in.
      RETURN
C
21100      IF(PRSA/=SQUEEW) GO TO 10            ! squeeze?
      IF(QOPEN(PRSO)) GO TO 21200            ! tube open?
      CALL RSPEAK(909)                  ! no, can't do it.
      RETURN
C
21200      IF(OCAN(PUTTY)==PRSO) GO TO 21300      ! putty inside?
      CALL RSPEAK(910)                  ! no, doesn't work.
      RETURN
C
21300      CALL NEWSTA(PUTTY,911,0,0,WINNER)      ! putty now in hand.
      RETURN
C
C O13--      Chalice
C
23000      IF((PRSA/=TAKEW).OR.(OCAN(PRSO)/=0).OR.
     &      (OROOM(PRSO)/=TREAS).OR.(OROOM(THIEF)/=TREAS).OR.
     &      (IAND(OFLAG2(THIEF), FITEBT)==0).OR.
     &      .NOT. THFACT) GO TO 10
      CALL RSPEAK(204)                  ! cant take.
      RETURN
C
C O14--      Painting
C
24000      IF(PRSA/=MUNGW) GO TO 10            ! mung?
      CALL RSPEAK(205)                  ! destroy painting.
      OFVAL(PRSO)=0
      OTVAL(PRSO)=0
      ODESC1(PRSO)=207
      ODESC2(PRSO)=206
      RETURN

C SOBJS, PAGE 8
C
C O15--      Bolt
C
27000      IF(PRSA/=TURNW) GO TO 27600            ! turn bolt?
      IF(PRSI/=WRENC) GO TO 27500            ! with wrench?
      IF(GATEF) GO TO 27100                  ! proper button pushed?
      CALL RSPEAK(210)                  ! no, lose.
      RETURN
C
27100      IF(LWTIDF) GO TO 27200                  ! low tide now?
      LWTIDF=.TRUE.                        ! no, empty dam.
      CALL RSPEAK(211)
      OFLAG2(COFFI)=IAND(OFLAG2(COFFI), NOT(SCRDBT))
      OFLAG1(TRUNK)=IOR(OFLAG1(TRUNK), VISIBT)      ! materialize trunk.
      RFLAG(RESER)=IAND(IOR(RFLAG(RESER), RLAND),
     &      NOT(RWATER+RSEEN))      ! keep thief away.
      RETURN
C
27200      LWTIDF=.FALSE.                        ! yes, fill dam.
      CALL RSPEAK(212)
      IF(OROOM(TRUNK)==RESER) OFLAG1(TRUNK)=IAND(OFLAG1(TRUNK),
     &      NOT(VISIBT))
      RFLAG(RESER)=IAND(IOR(RFLAG(RESER), RWATER), NOT(RLAND))
      RETURN
C
27500      CALL RSPSUB(299,ODI2)                  ! not with that.
      RETURN
C
27600      IF(PRSA/=OILW) GO TO 10            ! oil?
      CALL RSPEAK(906)                  ! trouble.
      RETURN
C
C O16--      Grating
C
28000      IF((PRSA/=OPENW).AND.(PRSA/=CLOSEW)) GO TO 10
      IF(GRUNLF) GO TO 28100                  ! unlocked?
      CALL RSPEAK(214)                  ! no, locked.
      RETURN
C
28100      I=215                              ! unlocked, view frm below.
      IF(HERE==CLEAR) I=216                  ! view from clearing
      SOBJS=OPNCLS(GRATE,I,885)            ! open/close.
      RFLAG(MGRAT)=IAND(RFLAG(MGRAT), NOT(RLIGHT))      ! set light/dark.
      IF(.NOT.QOPEN(GRATE)) GO TO 50            ! if not open, done.
      RFLAG(MGRAT)=IOR(RFLAG(MGRAT), RLIGHT)      ! now lit.
      RVCLR=1                              ! leaves shoved aside.
      CALL NEWSTA(GRATE,0,CLEAR,0,0)            ! grating in clearing.
      RETURN
C
C O17--      Trap door
C
29000      IF(HERE/=LROOM) GO TO 29100            ! from living room?
      SOBJS=OPNCLS(DOOR,218,219)            ! open/close.
      RETURN
C
29100      IF(HERE/=CELLA) GO TO 10            ! from cellar?
      IF((PRSA/=OPENW).OR.QOPEN(DOOR)) GO TO 29200
      CALL RSPEAK(220)                  ! cant open closed door.
      RETURN
C
29200      SOBJS=OPNCLS(DOOR,0,22)                  ! normal open/close.
      RETURN
C
C O18--      Durable door
C
30000      I=0                              ! assume no appl.
      IF(PRSA==OPENW) I=221                  ! open?
      IF(PRSA==BURNW) I=222                  ! burn?
      IF(PRSA==MUNGW) I=223+RND(3)            ! mung?
      IF(I==0) GO TO 10
      CALL RSPEAK(I)
      RETURN
C
C O19--      Master switch
C
31000      IF(PRSA/=TURNW) GO TO 10            ! turn?
      IF(PRSI/=SCREW) GO TO 31500            ! with screwdriver?
      IF(QOPEN(MACHI)) GO TO 31600            ! lid up?
      CALL RSPEAK(226)                  ! no, activate.
      IF(OCAN(COAL)/=MACHI) GO TO 31400      ! coal inside?
      CALL NEWSTA(COAL,0,0,0,0)            ! kill coal,
      CALL NEWSTA(DIAMO,0,0,MACHI,0)            ! replace with diamond.
      RETURN
C
31400      DO 31450 I=1,OLNT                  ! kill noncoal objects.
        IF(OCAN(I)/=MACHI) GO TO 31450      ! inside machine?
        CALL NEWSTA(I,0,0,0,0)            ! kill object and contents.
        CALL NEWSTA(GUNK,0,0,MACHI,0)            ! reduce to gunk.
31450      CONTINUE
      RETURN
C
31500      CALL RSPSUB(300,ODI2)                  ! cant turn with that.
      RETURN
C
31600      CALL RSPEAK(227)                  ! lid is up.
      RETURN

C SOBJS, PAGE 9
C
C O20--      Leak
C
33000      IF((PRSO/=LEAK).OR.(PRSA/=PLUGW).OR.(RVMNT<=0))
     &      GO TO 10                  ! plug active leak?
      IF(PRSI/=PUTTY) GO TO 33100            ! with putty?
      RVMNT=-1                        ! disable leak.
      CTICK(CEVMNT)=0
      CALL RSPEAK(577)
      RETURN
C
33100      CALL RSPSUB(301,ODI2)                  ! cant with that.
      RETURN
C
C O21--      Drowning buttons
C
34000      IF(PRSA/=PUSHW) GO TO 10            ! push?
      GO TO (34100,34200,34300,34400),(PRSO-RBUTT+1)
      GO TO 10                        ! not a button.
C
34100      RFLAG(HERE)=IEOR(RFLAG(HERE), RLIGHT)      ! red, zap lights.
      I=230
      IF(IAND(RFLAG(HERE), RLIGHT)/=0) I=231
      CALL RSPEAK(I)
      GO TO 50                        ! go see if now dark.
C
34200      GATEF=.TRUE.                        ! yellow, release gate.
      CALL RSPEAK(232)
      RETURN
C
34300      GATEF=.FALSE.                        ! brown, interlock gate.
      CALL RSPEAK(232)
      RETURN
C
34400      IF(RVMNT/=0) GO TO 34500            ! blue, leak already started?
      CALL RSPEAK(233)                  ! no, start leak.
      RVMNT=1
      CFLAG(CEVMNT)=.TRUE.
      CTICK(CEVMNT)=-1
      RFLAG(HERE)=IOR(RFLAG(HERE), RFILL)      ! water present.
      OFLAG1(LEAK)=IOR(OFLAG1(LEAK), VISIBT)      ! bring on the leak.
      RETURN
C
34500      CALL RSPEAK(234)                  ! button jammed.
      RETURN
C
C O22--      Inflatable boat
C
36000      IF(PRSA/=INFLAW) GO TO 10            ! inflate?
      IF(OROOM(IBOAT)/=0) GO TO 36100      ! in room?
      CALL RSPEAK(235)                  ! no, joke.
      RETURN
C
36100      IF(PRSI/=PUMP) GO TO 36200            ! with pump?
      CALL NEWSTA(IBOAT,0,0,0,0)            ! kill defl boat,
      CALL NEWSTA(RBOAT,236,HERE,0,0)            ! repl with inf.
      IF(LASTIT==IBOAT) LASTIT=RBOAT      ! fix last it reference.
      DEFLAF=.FALSE.
      RETURN
C
36200      I=237                              ! jokes.
      IF(PRSI/=LUNGS) I=303
      CALL RSPSUB(I,ODI2)
      RETURN
C
C O23--      Deflated boat
C
37000      IF(PRSA/=INFLAW) GO TO 37100            ! inflate?
      CALL RSPEAK(238)                  ! joke.
      RETURN
C
37100      IF(PRSA/=PLUGW) GO TO 10            ! plug?
      IF(PRSI/=PUTTY) GO TO 33100            ! with putty?
      CALL NEWSTA(IBOAT,239,OROOM(DBOAT),OCAN(DBOAT),OADV(DBOAT))
      CALL NEWSTA(DBOAT,0,0,0,0)            ! kill defl boat, repl.
      IF(LASTIT==DBOAT) LASTIT=IBOAT      ! fix last it reference.
      RETURN

C SOBJS, PAGE 10
C
C O24--      Rubber boat
C
38000      IF(ARG/=0) GO TO 10                  ! dismiss readin, out.
      IF((PRSA/=BOARDW).OR.(OADV(STICK)/=WINNER)) GO TO 38100
      CALL NEWSTA(RBOAT,0,0,0,0)            ! kill infl boat,
      CALL NEWSTA(DBOAT,240,HERE,0,0)            ! repl with dead.
      IF(LASTIT==RBOAT) LASTIT=DBOAT      ! fix last it reference.
      DEFLAF=.TRUE.
      GO TO 50                        ! go see if now dark.
C
38100      IF(PRSA/=INFLAW) GO TO 38200            ! inflate?
      CALL RSPEAK(367)                  ! yes, joke.
      RETURN
C
38200      IF(PRSA/=DEFLAW) GO TO 10            ! deflate?
      IF(AV==RBOAT) GO TO 38300            ! in boat?
      IF(OROOM(RBOAT)==0) GO TO 38400      ! on ground?
      CALL NEWSTA(RBOAT,0,0,0,0)            ! kill infl boat,
      CALL NEWSTA(IBOAT,241,HERE,0,0)            ! repl with defl.
      IF(LASTIT==RBOAT) LASTIT=IBOAT      ! fix last it reference.
      DEFLAF=.TRUE.
      GO TO 50                        ! go see if now dark.
C
38300      CALL RSPEAK(242)                  ! in boat.
      RETURN
C
38400      CALL RSPEAK(243)                  ! not on ground.
      RETURN
C
C O25--      Braided rope (also balloon receptacle, cloth bag)
C
41000      IF((PRSA/=TIEW).OR.(PRSO/=BROPE).OR.
     &      ((PRSI/=HOOK1).AND.(PRSI/=HOOK2)))
     &      GO TO 41100                  ! tie to hook?
      BTIEF=PRSI                        ! record location.
      ODESC1(BTIEF)=1072                  ! change description.
      CFLAG(CEVBAL)=.FALSE.                  ! stall ascent.
      CALL RSPEAK(248)
      RETURN
C
41100      IF((PRSA/=UNTIEW).OR.(PRSO/=BROPE)) GO TO 41300
      IF(BTIEF/=0) GO TO 41200            ! tied up?
      CALL RSPEAK(249)                  ! no, joke.
      RETURN
C
41200      CALL RSPEAK(250)
      ODESC1(BTIEF)=1073                  ! restore description.
      BTIEF=0                              ! untie.
      CFLAG(CEVBAL)=.TRUE.
      CTICK(CEVBAL)=3                        ! restart clock.
      RETURN
C
41300      IF((PRSA/=FINDW).AND.(PRSA/=EXAMIW)) GO TO 41400
      CALL RSPSUB(1063,ODO2)                  ! describe.
      RETURN
C
41400      IF(PRSA/=TAKEW) GO TO 10            ! take?
      CALL RSPSUB(1064,ODO2)                  ! can't.
      IF(PRSO==BROPE) CALL RSPEAK(1065)      ! rope can be tied.
      RETURN
C
C O26--      Safe
C
42000      I=0                              ! assume unprocessed.
      IF(PRSA==TAKEW) I=251                  ! take?
      IF((PRSA==OPENW).AND.SAFEF) I=253      ! open after blast?
      IF((PRSA==OPENW).AND..NOT.SAFEF) I=254 ! open before blast?
      IF((PRSA==CLOSEW).AND.SAFEF) I=253      ! close after?
      IF((PRSA==CLOSEW).AND..NOT.SAFEF) I=255
      IF(I==0) GO TO 10
      CALL RSPEAK(I)
      RETURN
C
C O27--      Fuse
C
43000      IF(PRSA/=BURNW) GO TO 10            ! burn?
      CALL RSPEAK(256)
      CFLAG(CEVFUS)=.TRUE.
      CTICK(CEVFUS)=2                        ! start countdown.
      RETURN
C
C O28--      Gnome
C
44000      IF((PRSA/=GIVEW).AND.(PRSA/=THROWW)) GO TO 44500
      IF(OTVAL(PRSO)==0) GO TO 44100      ! treasure?
      CALL RSPSUB(257,ODO2)                  ! yes, get door.
      CALL NEWSTA(PRSO,0,0,0,0)
      CALL NEWSTA(GNOME,0,0,0,0)            ! vanish gnome.
      GNODRF=.TRUE.
      GO TO 50                        ! go see if now dark.
C
44100      IF((PRSO/=BRICK).OR.(OCAN(FUSE)/=BRICK).OR.
     &      (CTICK(CEVFUS)==0)) GO TO 44200 ! a bomb?
      CALL NEWSTA(GNOME,927,0,0,0)            ! gnome leaves.
      CALL NEWSTA(BRICK,0,HERE,0,0)            ! brick on floor.
      CFLAG(CEVVLG)=.FALSE.                  ! turn off gnome clocks.
      CFLAG(CEVGNO)=.FALSE.
      RETURN
C
44200      CALL RSPSUB(258,ODO2)                  ! no, lose object.
      CALL NEWSTA(PRSO,0,0,0,0)
      GO TO 50                        ! go see if now dark.
C
44500      CALL RSPEAK(259)                  ! nervous gnome.
      IF(GNOMEF) RETURN
      CFLAG(CEVGNO)=.TRUE.
      CTICK(CEVGNO)=5                        ! schedule byebye.
      GNOMEF=.TRUE.
      RETURN
C
C O29--      Coke bottles
C
46000      IF((PRSA/=THROWW).AND.(PRSA/=MUNGW)) GO TO 10
      CALL NEWSTA(COKES,262,0,0,0)            ! mung bottles.
      IF(PRSI/=COKES) RETURN            ! with cokes?
      CALL RSPSUB(1066,ODO2)                  ! kill direct object, too.
      CALL NEWSTA(PRSO,0,0,0,0)
      RETURN

C SOBJS, PAGE 11
C
C O30--      Robot
C
53000      IF(PRSA/=GIVEW) GO TO 53200            ! give?
      IF(PRSO/=WATER) GO TO 53100            ! water?
      CALL NEWSTA(WATER,1081,0,0,0)            ! slips through fingers.
      RETURN
C
53100      CALL NEWSTA(PRSO,0,0,0,AROBOT)            ! put on robot.
      CALL RSPSUB(302,ODO2)
      RETURN
C
53200      IF(((PRSA/=MUNGW).AND.(PRSA/=THROWW)).OR.
     &  ((PRSO/=ROBOT).AND.(PRSI/=ROBOT))) GO TO 10
      CALL NEWSTA(ROBOT,285,0,0,0)            ! kill robot.
      GO TO 50                        ! go see if now dark.
C
C O31--      Grue
C
56000      IF(PRSA/=EXAMIW) GO TO 56100            ! examine?
      CALL RSPEAK(288)
      RETURN
C
56100      IF(PRSA/=FINDW) GO TO 10            ! find?
      CALL RSPEAK(289)

      END function SOBJS

C NOBJS-      New objects processor
C
C Declarations
C
      LOGICAL FUNCTION NOBJS(RI,ARG)
      use state
      use subr,only: qhere,newsta,bug,qempty,lit,moveto,mrhere,rnd,
     &  opncls,cpgoto,cpinfo,jigsup,princr
      use io,only:rspsub,rspeak

      integer,intent(in) :: RI,ARG

      LOGICAL QOPEN,F
      LOGICAL  WASLIT
      integer r,av,i,j,k,nxt,obj,odi2,odo2,svflag,svhere,target,wl
C
C Functions and data
C
      QOPEN(R)=IAND(OFLAG2(R), OPENBT)/=0
C
      ODO2=0
      ODI2=0
      IF((PRSO/=0).AND.(PRSO<=OMAX)) ODO2=ODESC2(PRSO)
      IF(PRSI/=0) ODI2=ODESC2(PRSI)
      AV=AVEHIC(WINNER)
      NOBJS=.TRUE.
      WASLIT=LIT(HERE)
C
      GO TO (1000,2000,3000,4000,5000,6000,7000,8000,9000,
     & 10000,11000,12000,13000,14000,15000,16000,17000,
     & 18000,19000,20000,21000,22000,23000,24000,25000,
     & 26000,27000,28000,29000,30000,31000,32000,33000,
     & 34000,35000,36000,37000,38000,39000,40000,41000,
     & 42000,43000,44000,45000,46000),
     &      (RI-31)
      CALL BUG(6,RI)
C
C Return here to declare false result.
C
10      NOBJS=.FALSE.
      RETURN
C
C Return here to test for light source change.
C
50      IF(WASLIT.AND..NOT.LIT(HERE)) CALL RSPEAK(406)
      RETURN

C NOBJS, PAGE 3
C
C O32--      Bills
C
1000      IF(PRSA/=EATW) GO TO 1100            ! eat?
      CALL RSPEAK(639)                  ! joke.
      RETURN
C
1100      IF(PRSA==BURNW) CALL RSPEAK(640)      ! burn?  joke.
      GO TO 10                        ! let it be handled.
C
C O33--      Screen of light
C
2000      TARGET=SCOL                        ! target is scol.
2100      IF(PRSO/=TARGET) GO TO 2400            ! prso==target?
      IF((PRSA/=PUSHW).AND.(PRSA/=MOVEW).AND.
     &      (PRSA/=TAKEW).AND.(PRSA/=RUBW)) GO TO 2200
      CALL RSPEAK(673)                  ! hand passes thru.
      RETURN
C
2200      IF((PRSA/=KILLW).AND.(PRSA/=ATTACW).AND.
     &      (PRSA/=MUNGW)) GO TO 2400      ! aggressive?
      CALL RSPSUB(674,ODI2)                  ! passes thru.
      RETURN
C
2400      IF((PRSA/=THROWW).OR.(PRSI/=TARGET)) GO TO 10
      IF(HERE==BKBOX) GO TO 2600            ! thru scol?
      CALL NEWSTA(PRSO,0,BKBOX,0,0)            ! no, thru wall.
      CALL RSPSUB(675,ODO2)                  ! ends up in box room.
      CTICK(CEVSCL)=0                        ! cancel alarm.
      SCOLRM=0                        ! reset scol room.
      GO TO 50                        ! go see if now dark.
C
2600      IF(SCOLRM==0) GO TO 2900            ! tried to go thru?
      CALL NEWSTA(PRSO,0,SCOLRM,0,0)            ! success.
      CALL RSPSUB(676,ODO2)                  ! ends up somewhere.
      CTICK(CEVSCL)=0                        ! cancel alarm.
      SCOLRM=0                        ! reset scol room.
      GO TO 50                        ! go see if now dark.
C
2900      CALL RSPEAK(213)                  ! cant do it.
      RETURN

C NOBJS, PAGE 4
C
C O34--      Gnome of Zurich
C
3000      IF((PRSA/=GIVEW).AND.(PRSA/=THROWW)) GO TO 3200
      IF(OTVAL(PRSO)/=0) GO TO 3100            ! throw a treasure?
      IF((PRSO/=BRICK).OR.(OCAN(FUSE)/=BRICK).OR.
     &      (CTICK(CEVFUS)==0)) GO TO 3050 ! a bomb?
      CALL NEWSTA(ZGNOM,931,0,0,0)            ! gnome leaves.
      CALL NEWSTA(BRICK,0,HERE,0,0)            ! brick on floor.
      CFLAG(CEVZGO)=.FALSE.                  ! stop gnome timers.
      CFLAG(CEVZGI)=.FALSE.
      RETURN
C
3050      CALL NEWSTA(PRSO,641,0,0,0)            ! no, go pop.
      RETURN
C
3100      CALL NEWSTA(PRSO,0,0,0,0)            ! yes, bye bye treasure.
      CALL RSPSUB(642,ODO2)
      CALL NEWSTA(ZGNOM,0,0,0,0)            ! bye bye gnome.
      CFLAG(CEVZGO)=.FALSE.                  ! cancel exit.
      F=MOVETO(BKENT,WINNER)                  ! now in bank entrance.
      RETURN
C
3200      IF((PRSA/=ATTACW).AND.(PRSA/=KILLW).AND.
     &      (PRSA/=MUNGW)) GO TO 3300      ! aggressive?
      CALL NEWSTA(ZGNOM,643,0,0,0)            ! vanish gnome.
      CFLAG(CEVZGO)=.FALSE.                  ! cancel exit.
      RETURN
C
3300      CALL RSPEAK(644)                  ! gnome is impatient.
      RETURN
C
C O35--      Egg
C
4000      IF((PRSA/=OPENW).OR.(PRSO/=EGG)) GO TO 4500
      IF(.NOT.QOPEN(EGG)) GO TO 4100            ! open already?
      CALL RSPEAK(649)                  ! yes.
      RETURN
C
4100      IF(PRSI/=0) GO TO 4200            ! with something?
      CALL RSPEAK(650)                  ! no, cant.
      RETURN
C
4200      IF(PRSI/=HANDS) GO TO 4300            ! with hands?
      CALL RSPEAK(651)                  ! not recommended.
      RETURN
C
4300      I=652                              ! mung message.
      IF((IAND(OFLAG1(PRSI), TOOLBT)/=0).OR.
     &      (IAND(OFLAG2(PRSI), WEAPBT)/=0)) GO TO 4600
      I=653                              ! novelty 1.
      IF(IAND(OFLAG2(PRSO), FITEBT)/=0) I=654 ! novelty 2.
      OFLAG2(PRSO)=IOR(OFLAG2(PRSO), FITEBT)
      CALL RSPSUB(I,ODI2)
      RETURN
C
4500      IF(PRSA/=MUNGW) GO TO 10            ! mung?
      I=655                              ! you blew it.
4600      CALL NEWSTA(BEGG,I,OROOM(EGG),OCAN(EGG),OADV(EGG))
      CALL NEWSTA(EGG,0,0,0,0)            ! vanish egg.
      IF(LASTIT==EGG) LASTIT=BEGG            ! fix last it reference.
      OTVAL(BEGG)=2                        ! bad egg has value.
      IF(OCAN(CANAR)/=EGG) GO TO 4700      ! was canary inside?
      CALL RSPEAK(ODESCO(BCANA))            ! yes, describe result.
      OTVAL(BCANA)=1
      RETURN
C
4700      CALL NEWSTA(BCANA,0,0,0,0)            ! no, vanish it.
      RETURN

C NOBJS, PAGE 5
C
C O36--      Canaries, good and bad
C
5000      IF(PRSA/=WINDW) GO TO 10            ! wind em up?
      IF(PRSO==CANAR) GO TO 5100            ! right one?
      CALL RSPEAK(645)                  ! no, bad news.
      RETURN
C
5100      IF(.NOT.SINGSF.AND.((HERE==MTREE).OR.
     &      ((HERE>=FORE1).AND.(HERE<CLEAR))))
     &      GO TO 5200                  ! first song in for?
      CALL RSPEAK(646)                  ! no, mediocre news.
      RETURN
C
5200      SINGSF=.TRUE.                        ! sang song.
      CALL NEWSTA(BAUBL,647,HERE,0,0)            ! place bauble.
      IF(HERE==MTREE) CALL NEWSTA(BAUBL,0,FORE3,0,0)
      RETURN
C
C O37--      White cliffs
C
6000      IF((PRSA/=CLMBW).AND.(PRSA/=CLMBUW).AND.
     &      (PRSA/=CLMBDW)) GO TO 10      ! climb?
      CALL RSPEAK(648)                  ! oh yeah?
      RETURN
C
C O38--      Wall
C
7000      IF((IABS(HERE-MLOC)/=1).OR.(MRHERE(HERE)/=0).OR.
     &      .NOT.ENDGMF) GO TO 7100            ! mirror wall in endgame?
      IF(PRSA/=PUSHW) GO TO 10            ! pushed?
      CALL RSPEAK(860)                  ! pushed mirror wall.
      RETURN
C
7100      IF(IAND(RFLAG(HERE), RNWALL)==0) GO TO 10
      CALL RSPEAK(662)                  ! no wall.
      RETURN

C NOBJS, PAGE 6
C
C O39--      Global bird
C
8000      IF(PRSA/=FINDW) GO TO 8100            ! find?
      CALL RSPEAK(666)
      RETURN
C
8100      IF(PRSA/=EXAMIW) GO TO 10            ! examine?
      CALL RSPEAK(667)
      RETURN
C
C O40--      Puzzle/Scol walls
C
9000      IF(HERE/=CPUZZ) GO TO 9500            ! puzzle walls?
      IF(PRSA/=PUSHW) GO TO 10            ! push?
      DO 9100 I=1,8,2                        ! locate wall.
        IF(PRSO==CPWL(I)) GO TO 9200
9100      CONTINUE
      CALL BUG(80,PRSO)                  ! what?
C
9200      J=CPWL(I+1)                        ! get directional offset.
      NXT=CPHERE+J                        ! get next state.
      WL=CPVEC(NXT)                        ! get c(next state).
      GO TO (9300,9300,9300,9250,9350),(WL+4)      ! process.
C
9250      CALL RSPEAK(876)                  ! clear corridor.
      RETURN
C
9300      IF(CPVEC(NXT+J)==0) GO TO 9400      ! movable, room to move?
9350      CALL RSPEAK(877)                  ! immovable, no room.
      RETURN
C
9400      I=878                              ! assume first push.
      IF(CPUSHF) I=879                  ! not?
      CPUSHF=.TRUE.
      CPVEC(NXT+J)=WL                        ! move wall.
      CPVEC(NXT)=0                        ! vacate next state.
      CALL CPGOTO(NXT)                  ! onward.
      CALL CPINFO(I,NXT)                  ! describe.
      CALL PRINCR(1,HERE)            ! print rooms contents.
      RETURN
C
9500      IF(HERE/=SCOLAC) GO TO 9700            ! in scol active room?
      DO 9600 I=1,12,3
        TARGET=SCOLWL(I+1)                  ! assume target.
        IF(SCOLWL(I)==HERE) GO TO 2100      ! treat if found.
9600      CONTINUE
C
9700      IF(HERE/=BKBOX) GO TO 10            ! in box room?
      TARGET=WNORT
      GO TO 2100

C NOBJS, PAGE 7
C
C O41--      Short pole
C
10000      IF(PRSA/=RAISEW) GO TO 10100            ! lift?
      I=749                              ! assume up.
      IF(POLEUF==2) I=750                  ! already up?
      CALL RSPEAK(I)
      POLEUF=2                        ! pole is raised.
      RETURN
C
10100      IF((PRSA/=LOWERW).AND.(PRSA/=PUSHW)) GO TO 10
      IF(POLEUF/=0) GO TO 10200            ! already lowered?
      CALL RSPEAK(751)                  ! cant do it.
      RETURN
C
10200      IF(MOD(MDIR,180)/=0) GO TO 10300      ! mirror n-s?
      POLEUF=0                        ! yes, lower into
      CALL RSPEAK(752)                  ! channel.
      RETURN
C
10300      IF((MDIR/=270).OR.(MLOC/=MRB)) GO TO 10400
      POLEUF=0                        ! lower into hole.
      CALL RSPEAK(753)
      RETURN
C
10400      CALL RSPEAK(753+POLEUF)                  ! poleuf = 1 or 2.
      POLEUF=1                        ! now on floor.
      RETURN
C
C O42--      Mirror switch
C
11000      IF(PRSA/=PUSHW) GO TO 10            ! push?
      IF(MRPSHF) GO TO 11300                  ! already pushed?
      CALL RSPEAK(756)                  ! button goes in.
      DO 11100 I=1,OLNT                  ! blocked?
        IF(QHERE(I,MREYE).AND.(I/=RBEAM)) GO TO 11200
11100      CONTINUE
      CALL RSPEAK(757)                  ! nothing in beam.
      RETURN
C
11200      CFLAG(CEVMRS)=.TRUE.                  ! mirror opens.
      CTICK(CEVMRS)=7
      MRPSHF=.TRUE.
      MROPNF=.TRUE.
      RETURN
C
11300      CALL RSPEAK(758)                  ! mirror already open.
      RETURN

C NOBJS, PAGE 8
C
C O43--      Beam function
C
12000      IF((PRSA/=TAKEW).OR.(PRSO/=RBEAM)) GO TO 12100
      CALL RSPEAK(759)                  ! take beam, joke.
      RETURN
C
12100      I=PRSO                              ! assume blk with dirobj.
      IF((PRSA==PUTW).AND.(PRSI==RBEAM)) GO TO 12200
      IF((PRSA/=MUNGW).OR.(PRSO/=RBEAM).OR.
     &      (PRSI==0)) GO TO 10            ! break beam with x?
      I=PRSI
12200      IF(OADV(I)/=WINNER) GO TO 12300      ! carrying?
      CALL NEWSTA(I,0,HERE,0,0)            ! drop obj.
      CALL RSPSUB(760,ODESC2(I))
      RETURN
C
12300      J=761                              ! assume not in room.
      IF(QHERE(I,HERE)) J=762                  ! in room?
      CALL RSPSUB(J,ODESC2(I))            ! describe.
      RETURN
C
C O44--      Bronze door
C
13000      IF((HERE==NCELL).OR.((LCELL==4).AND.
     &      ((HERE==CELL).OR.(HERE==SCORR))))
     &      GO TO 13100
      CALL RSPEAK(763)                  ! door not there.
      RETURN
C
13100      IF(.NOT.OPNCLS(ODOOR,764,765)) GO TO 10      ! open/close?
      IF((HERE==NCELL).AND.QOPEN(ODOOR))
     &      CALL RSPEAK(766)            ! descr view.
      RETURN
C
C O45--      Quiz door
C
14000      IF((PRSA/=OPENW).AND.(PRSA/=CLOSEW)) GO TO 14100
      CALL RSPEAK(767)                  ! door wont move.
      RETURN
C
14100      IF(PRSA/=KNOCKW) GO TO 10            ! knock?
      IF(INQSTF) GO TO 14200                  ! tried it already?
      INQSTF=.TRUE.                        ! start inquisition.
      CFLAG(CEVINQ)=.TRUE.
      CTICK(CEVINQ)=2
      QUESNO=RND(8)                        ! select question.
      NQATT=0
      CORRCT=0
      CALL RSPEAK(768)                  ! announce rules.
      CALL RSPEAK(769)
      CALL RSPEAK(770+QUESNO)                  ! ask question.
      RETURN
C
14200      CALL RSPEAK(798)                  ! no reply.
      RETURN
C
C O46--      Locked door
C
15000      IF(PRSA/=OPENW) GO TO 10            ! open?
      CALL RSPEAK(778)                  ! cant.
      RETURN
C
C O47--      Cell door
C
16000      NOBJS=OPNCLS(CDOOR,779,780)            ! open/close?
      RETURN

C NOBJS, PAGE 9
C
C O48--      Dialbutton
C
17000      IF(PRSA/=PUSHW) GO TO 10            ! push?
      CALL RSPEAK(809)                  ! click.
      IF(QOPEN(CDOOR)) CALL RSPEAK(810)      ! close cell door.
      OFLAG2(CDOOR)=IAND(OFLAG2(CDOOR), NOT(OPENBT))
      OFLAG2(ODOOR)=IAND(OFLAG2(ODOOR), NOT(OPENBT))
      IF(LCELL==PNUMB) RETURN            ! any change?
C
      DO 17100 I=1,OLNT                  ! relocate old to hyper.
        IF((OROOM(I)==CELL).AND.(IAND(OFLAG1(I), DOORBT)==0))
     &      CALL NEWSTA(I,0,LCELL*HFACTR,0,0)
        IF(OROOM(I)==(PNUMB*HFACTR))
     &      CALL NEWSTA(I,0,CELL,0,0)      ! move in new hyper.
17100      CONTINUE
C
      OFLAG1(ODOOR)=IAND(OFLAG1(ODOOR), NOT(VISIBT))
      IF(PNUMB==4) OFLAG1(ODOOR)=IOR(OFLAG1(ODOOR), VISIBT)
C
      IF(AROOM(PLAYER)/=CELL) GO TO 17400      ! player in cell?
      IF(LCELL/=4) GO TO 17200            ! in right cell?
      OFLAG1(ODOOR)=IOR(OFLAG1(ODOOR), VISIBT)
      F=MOVETO(NCELL,PLAYER)                  ! yes, moveto ncell.
      GO TO 17400
17200      F=MOVETO(PCELL,PLAYER)                  ! no, moveto pcell.
C
17400      LCELL=PNUMB
      RETURN

C NOBJS, PAGE 10
C
C O49--      Dial indicator
C
18000      IF((PRSA/=OPENW).OR.(PRSO/=BOOK)) GO TO 18100      ! open book?
      PNUMB=RND(8)+1                        ! whee!
      CALL RSPSUB(797,712+PNUMB)
      RETURN
C
18100      IF((PRSA/=MOVEW).AND.(PRSA/=PUTW).AND.
     &      (PRSA/=TRNTOW)) GO TO 10
      IF(PRSI/=0) GO TO 18200            ! turn dial to x?
      CALL RSPEAK(806)                  ! must specify.
      RETURN
C
18200      IF((PRSI>=NUM1).AND.(PRSI<=NUM8)) GO TO 18300
      CALL RSPEAK(807)                  ! must be digit.
      RETURN
C
18300      PNUMB=PRSI-NUM1+1                  ! set up new.
      CALL RSPSUB(808,712+PNUMB)
      RETURN
C
C O50--      Global mirror
C
19000      NOBJS=MIRPAN(832,.FALSE.)
      RETURN
C
C O51--      Global panel
C
20000      IF(HERE/=FDOOR) GO TO 20100            ! at front door?
      IF((PRSA/=OPENW).AND.(PRSA/=CLOSEW)) GO TO 10
      CALL RSPEAK(843)                  ! panel in door, nogo.
      RETURN
C
20100      NOBJS=MIRPAN(838,.TRUE.)
      RETURN
C
C O52--      Puzzle Room slit
C
21000      IF((PRSA/=PUTW).OR.(PRSI/=CSLIT)) GO TO 10
      IF(PRSO/=GCARD) GO TO 21100            ! put card in slit?
      CALL NEWSTA(PRSO,863,0,0,0)            ! kill card.
      CPOUTF=.TRUE.                        ! open door.
      RETURN
C
21100      IF((IAND(OFLAG1(PRSO), VICTBT)==0).AND.
     &  (IAND(OFLAG2(PRSO), VILLBT)==0)) GO TO 21200
      CALL RSPEAK(552+RND(6))                  ! joke for vill, vict.
      RETURN
C
21200      CALL NEWSTA(PRSO,0,0,0,0)            ! kill object.
      CALL RSPSUB(864,ODO2)                  ! describe.
      RETURN

C NOBJS, PAGE 11
C
C O53--      Global brochure or stamp
C
22000      IF(PRSO/=STAMP) GO TO 22100            ! stamp?
      IF(PRSA==TAKEW) OFLAG1(BROCH)=IAND(OFLAG1(BROCH), NOT(CONTBT))
      GO TO 10                        ! do normal take.
C
22100      IF((PRSO/=BROCH).OR.
     &  ((PRSA/=EXAMIW).AND.(PRSA/=READW)))
     &  GO TO 22200                        ! examine brochure?
      CALL RSPEAK(942)                  ! describe.
      IF(OCAN(STAMP)==BROCH) CALL RSPEAK(943)
      RETURN
C
22200      IF((PRSA/=FINDW).OR..NOT.BROC1F) GO TO 22300
      CALL RSPEAK(944)                  ! on the way.
      RETURN
C
22300      IF(PRSA/=SENDW) GO TO 22400            ! send?
      IF(BROC2F) CALL RSPEAK(945)            ! already got it.
      IF(BROC1F.AND..NOT.BROC2F) CALL RSPEAK(944)      ! on the way.
      IF(.NOT.BROC1F) CALL RSPEAK(947)      ! postal service.
      BROC1F=.TRUE.                        ! send for brochure.
      RETURN
C
22400      IF(PRSO/=GBROCH) GO TO 10            ! global brochure?
      CALL RSPEAK(1071)                  ! pretend it's not there.
      RETURN
C
C O54--      Global ground
C
23000      IF(HERE==SBEACH) GO TO 36000            ! at sandy beach? then sand.
      IF(PRSA/=DIGW) GO TO 10            ! dig?
      CALL RSPEAK(924)                  ! can't do it.
      RETURN
C
C O55--      Global granite wall
C
24000      I=916                              ! joke for take.
      IF(PRSA==TAKEW) GO TO 24100            ! take?
      I=918                              ! temple, treasure find.
      IF(HERE==SLIDE) I=917                  ! slide find.
      IF(PRSA/=FINDW) GO TO 10            ! find?
24100      CALL RSPEAK(I)                        ! tell all.
      RETURN

C NOBJS, PAGE 12
C
C O56--      Global house
C
25000      IF((HERE>=WHOUS).AND.(HERE<=EHOUS)) GO TO 25200
      IF(PRSA/=FINDW) GO TO 25100            ! find, not at house?
      I=892                              ! joke 1.
      IF(HERE==CLEAR) I=893                  ! joke 2 for clearing.
      CALL RSPEAK(I)
      RETURN
C
25100      CALL RSPEAK(894)                  ! not there.
      RETURN
C
25200      IF(PRSA/=FINDW) GO TO 25300            ! find, at house?
      CALL RSPEAK(895)                  ! right there.
      RETURN
C
25300      IF(PRSA/=EXAMIW) GO TO 25400            ! look at?
      CALL RSPEAK(896)                  ! a nice house.
      RETURN
C
25400      IF(PRSA/=BURNW) GO TO 25500            ! burn?
      CALL RSPEAK(897)                  ! bad boy.
      RETURN
C
25500      IF(PRSA/=GTHROW) GO TO 10            ! go through?
      IF(HERE==EHOUS) GO TO 25600            ! at east side?
      CALL RSPEAK(898)                  ! can't do it
      RETURN
C
25600      IF(QOPEN(WINDO)) GO TO 25700            ! window open?
      CALL RSPEAK(899)                  ! no, shut
      RETURN
C
25700      F=MOVETO(KITCH,WINNER)                  ! move into kitchen.
      F=RMDESC(0)                        ! describe room
      RETURN
C
C O57--      Barred window in white house
C
26000      IF((PRSA/=OPENW).AND.(PRSA/=LOOKIW).AND.
     &  (PRSA/=GTHROW)) GO TO 26100      ! open, look in, enter?
      CALL RSPEAK(1039)                  ! window barred.
      RETURN
C
26100      IF(PRSA/=CLOSEW) GO TO 10            ! close?
      CALL RSPEAK(1040)                  ! already closed and locked.
      RETURN

C NOBJS, PAGE 13
C
C O58--      Global well
C
27000      IF((IAND(OFLAG1(PRSO), TAKEBT)==0).OR.(PRSO==WATER).OR.
     &  ((PRSA/=THROWW).AND.(PRSA/=PUTW).AND.(PRSA/=DROPW)))
     &  GO TO 10                        ! throw, put, drop x in well?
      CALL RSPSUB(939,ODO2)
      CALL NEWSTA(PRSO,0,BWELL,0,0)            ! put in well bottom.
      GO TO 50                        ! go see if now dark.
C
C O59--      Global rope
C
28000      IF(PRSA/=TAKEW) GO TO 28100            ! take?
      CALL RSPEAK(1006)                  ! not a good idea.
      RETURN
C
28100      IF(PRSA/=DROPW) GO TO 28300            ! drop?
      CALL RSPEAK(1007)                  ! you lose.
28200      F=MOVETO(CELLA,WINNER)                  ! drop into cellar.
      F=RMDESC(3)                        ! describe.
      RETURN
C
28300      IF((PRSA==CLMBW).OR.(PRSA==CLMBUW).OR.
     &  (PRSA==CLMBDW)) GO TO 10            ! normal climb.
      CALL RSPEAK(1008)                  ! not a good idea.
      RETURN
C
C O60--      Global slide
C
29000      IF((PRSA/=GTHROW).AND.
     & ((PRSA/=PUTW).OR.(PRSO/=AOBJ(PLAYER)))) GO TO 29100
      CALL RSPEAK(1010)                  ! down the slide.
      GO TO 28200
C
29100      IF(PRSA/=PUTW) GO TO 10            ! put in slide?
      IF(IAND(OFLAG1(PRSO), TAKEBT)/=0) GO TO 29200
      CALL RSPEAK(552+RND(6))                  ! can't take it.
      RETURN
C
29200      IF(PRSO/=TTIE) GO TO 29300            ! tied object?
      OFLAG1(TTIE)=IAND(OFLAG1(TTIE), NOT(NDSCBT))
      OFLAG1(ROPE)=IAND(OFLAG1(ROPE), NOT(NDSCBT))
      OFLAG2(ROPE)=IAND(OFLAG2(ROPE), NOT(CLMBBT))
      ODESC1(TIMBE)=1032                  ! restored untied desc.
      ODESC1(COFFI)=1033
      TTIE=0                              ! not tied any more.
      CALL NEWSTA(ROPE,0,CELLA,0,0)            ! rope is now here.
29300      CALL RSPSUB(1011,ODO2)
      CALL NEWSTA(PRSO,0,CELLA,0,0)            ! put in cellar,
      IF(PRSO==WATER) CALL NEWSTA(PRSO,0,0,0,0)      ! unless water.
      GO TO 50                        ! go see if now dark.

C NOBJS, PAGE 14
C
C O61--      Barrel
C
30000      IF(ARG/=1) GO TO 10                  ! read in?
      I=0                              ! default.
      IF(PRSA==WALKW) I=920                  ! walk?
      IF(PRSA==LOOKW) I=921                  ! look?
      IF(PRSA==BURNW) I=922                  ! burn?
      IF(PRSA==TAKEW) I=552+RND(6)            ! take?
      CALL RSPEAK(I)
      NOBJS=I/=0                        ! handled?
      RETURN
C
C O62--      Hot bell
C
31000      IF(PRSA/=TAKEW) GO TO 31100            ! take?
      CALL RSPEAK(972)                  ! too hot.
      RETURN
C
31100      IF(PRSA/=RINGW) GO TO 31400            ! ring?
      IF(PRSI/=0) GO TO 31200            ! with something?
      CALL RSPEAK(973)                  ! too hot.
      RETURN
C
31200      IF(IAND(OFLAG1(PRSI), BURNBT)==0) GO TO 31300
      CALL RSPSUB(974,ODI2)                  ! burnable is consumed.
      CALL NEWSTA(PRSI,0,0,0,0)            ! vanish it.
      RETURN
C
31300      I=975                              ! joke 1.
      IF(PRSI==HANDS) I=973                  ! joke for hands.
      CALL RSPEAK(I)
      RETURN
C
31400      IF(PRSA/=PORONW) GO TO 10            ! pour on?
      CALL NEWSTA(HBELL,0,0,0,0)            ! vanish bell.
      CALL NEWSTA(BELL,976,LLD1,0,0)            ! insert real bell.
      IF(LASTIT==HBELL) LASTIT=BELL            ! fix last it reference.
      CALL NEWSTA(PRSO,0,0,0,0)            ! vanish water.
      CTICK(CEVXBH)=0                        ! cancel cooling.
      RETURN
C
C O63--      Axe
C
32000      IF(PRSA/=TAKEW) GO TO 10            ! take?
      CALL RSPEAK(891)                  ! too hot.
      RETURN

C NOBJS, PAGE 15
C
C O64--      Timber (also coffin)
C
33000      IF((PRSA/=TAKEW).OR.(PRSO/=TTIE)) GO TO 10
      CALL RSPSUB(1009,ODO2)                  ! rope becomes untied.
      OFLAG1(TTIE)=IAND(OFLAG1(TTIE), NOT(NDSCBT))
      OFLAG1(ROPE)=IAND(OFLAG1(ROPE), NOT(NDSCBT))
      OFLAG2(ROPE)=IAND(OFLAG2(ROPE), NOT(CLMBBT))
      ODESC1(TIMBE)=1032                  ! restored untied desc.
      ODESC1(COFFI)=1033
      TTIE=0                              ! not tied any more.
      CALL NEWSTA(ROPE,0,HERE,0,0)            ! rope is now here.
      GO TO 10                        ! don't handle.
C
C O65--      Guano
C
34000      IF(PRSA/=DIGW) GO TO 10            ! dig?
      RVGUA=MIN0(4,RVGUA+1)                  ! go to next state.
      CALL RSPEAK(91+RVGUA)                  ! describe.
      RETURN
C
C O66--      Alice room leak
C
35000      IF(PRSA/=TAKEW) GO TO 35100            ! take?
      CALL RSPEAK(552+RND(6))                  ! joke.
      RETURN
C
35100      IF((PRSA/=PLUGW).OR.(PRSO/=PLEAK)) GO TO 10      ! plug leak?
      CALL RSPEAK(929)                  ! can't reach.
      RETURN

C NOBJS, PAGE 16
C
C O67--      Sand
C
36000      IF(PRSA/=DIGW) GO TO 10            ! dig?
      RVSND=RVSND+1                        ! go to next state.
      GO TO (36100,36100,36100,36400,36500),RVSND      ! process
      CALL BUG(2,RVSND)
C
36100      CALL RSPEAK(85+RVSND)                  ! 1..3, describe.
      RETURN
C
36400      IF(IAND(OFLAG1(STATU), VISIBT)==0) CALL RSPEAK(89)
      OFLAG1(STATU)=IOR(OFLAG1(STATU), VISIBT)! 4, statue appears.
      RETURN
C
36500      RVSND=0                              ! 5, sand collapses.
      IF(OROOM(STATU)==HERE)
     &      OFLAG1(STATU)=IAND(OFLAG1(STATU), NOT(VISIBT))
      CALL JIGSUP(90)                        ! gonzo.
      RETURN
C
C O68--      Torch
C
37000      IF(PRSA/=TRNOFW) GO TO 10            ! extinguish?
      CALL RSPEAK(900)                  ! can't do it.
      RETURN
C
C O69--      Tool chests
C
38000      IF(PRSA/=EXAMIW) GO TO 38100            ! examine?
      CALL RSPEAK(907)                  ! describe.
      RETURN
C
38100      IF(PRSA/=TAKEW) GO TO 10            ! take?
      CALL RSPEAK(908)                  ! can't do it.
      RETURN

C NOBJS, PAGE 17
C
C O70--      Palantir door
C
39000      IF((PRSA/=LOOKUW).OR..NOT.MATF) GO TO 39100
      CALL RSPEAK(995)                  ! mat under door.
      RETURN
C
39100      IF(PRSA/=UNLOKW) GO TO 39500            ! unlock?
      IF(PRSI/=PKEY) GO TO 39400            ! with rusty key?
39200      IF((OCAN(PKEY)==(HERE-PRM+PKH1)).OR.
     &      QEMPTY(HERE-PRM+PKH1)) GO TO 39300      ! keyhole empty?
39250      CALL RSPEAK(991)                  ! no
      RETURN
C
39300      CALL RSPEAK(996)                  ! now unlocked.
      PUNLKF=.TRUE.
      RETURN
C
39400      I=997                              ! joke 1.
      IF(PRSI==KEYS) I=998                  ! joke 2 for keys.
      CALL RSPEAK(I)
      RETURN
C
39500      IF(PRSA/=LOCKW) GO TO 39700            ! lock?
      IF(PRSI==PKEY) GO TO 39600            ! with rusty key?
      CALL RSPEAK(999)                  ! no
      RETURN
C
39600      IF((OCAN(PKEY)/=(HERE-PRM+PKH1)).AND.
     &      .NOT.QEMPTY(HERE-PRM+PKH1)) GO TO 39250
      CALL RSPEAK(1000)                  ! now locked.
      PUNLKF=.FALSE.
      RETURN
C
39700      IF((PRSA/=PUTUNW).OR.((PRSO/=BLABE).AND.
     &  (PRSO/=LABEL).AND.(PRSO/=CARD).AND.
     &  (PRSO/=WARNI).AND.(PRSO/=RBTLB).AND.
     &  (PRSO/=GUIDE))) GO TO 39800      ! put small paper?
      CALL NEWSTA(PRSO,1001,IEOR(HERE, 1),0,0)! put in other room.
      RETURN
C
39800      IF((PRSA/=OPENW).AND.(PRSA/=CLOSEW)) GO TO 10
      IF(.NOT.PUNLKF) GO TO 39900            ! open or close, unlocked?
      NOBJS=OPNCLS(PRSO,1002,1003)            ! open or close.
      RETURN
C
39900      CALL RSPEAK(1000)                  ! door locked.
      RETURN
C
C O71--      Palantir window
C
40000      IF(PRSA/=GTHROW) GO TO 40100            ! go through?
      CALL RSPEAK(1004)                  ! can't do it.
      RETURN
C
40100      IF(PRSA/=LOOKIW) GO TO 10            ! look in?
      IF(QOPEN(PDOOR)) GO TO 40200            ! door open?
      PLOOKF=.TRUE.                        ! set window flag.
      SVFLAG=RFLAG(IEOR(HERE, 1))            ! save room flags from other.
      F=MOVETO(IEOR(HERE, 1),WINNER)            ! go to other room.
      F=RMDESC(3)                        ! describe it.
      F=MOVETO(IEOR(HERE, 1),WINNER)            ! come back.
      RFLAG(IEOR(HERE, 1))=SVFLAG            ! restore flags.
      RETURN
C
40200      CALL RSPEAK(1005)                  ! door open, dummy.
      RETURN

C NOBJS, PAGE 18
C
C O72--      Keyhole lids
C
41000      IF((PRSA/=OPENW).AND.(PRSA/=RAISEW)) GO TO 41100
      CALL RSPEAK(985)                  ! open lid.
      OFLAG2(PRSO)=IOR(OFLAG2(PRSO), OPENBT)
      RETURN
C
41100      IF((PRSA/=CLOSEW).AND.(PRSA/=LOWERW)) GO TO 10
      IF(QEMPTY(PRSO-PLID1+PKH1)) GO TO 41200      ! keyhole empty?
      CALL RSPEAK(986)                  ! can't do it.
      RETURN
C
41200      CALL RSPEAK(987)                  ! close lid.
      OFLAG2(PRSO)=IAND(OFLAG2(PRSO), NOT(OPENBT))
      RETURN
C
C O73--      Keyholes
C
42000      IF(PRSA/=LOOKIW) GO TO 42200            ! look in?
      I=988                              ! doesn't work.
      IF(QOPEN(PLID1).AND.QOPEN(PLID2).AND.
     &      QEMPTY(PKH1).AND.QEMPTY(PKH2).AND.
     &      LIT(IEOR(HERE, 1))) I=989            ! does work
      CALL RSPEAK(I)
      RETURN
C
42200      IF(PRSA/=PUTW) GO TO 10            ! put?
      IF(QOPEN(PRSI-PKH1+PLID1)) GO TO 42300      ! lid open?
      CALL RSPEAK(990)                  ! no.
      RETURN
C
42300      IF(QEMPTY(PRSI)) GO TO 42400            ! keyhole empty?
      CALL RSPEAK(991)                  ! no.
      RETURN
C
42400      IF((PRSO/=SCREW).AND.(PRSO/=KEYS).AND.
     &  (PRSO/=STICK).AND.(PRSO/=PKEY)) GO TO 42700
      IF(QEMPTY(IEOR(PRSI, 1))) GO TO 10            ! nothing to shove.
      DO 42500 I=1,OLNT
        IF(OCAN(I)==IEOR(PRSI, 1)) GO TO 42600      ! find obj in keyhole.
42500      CONTINUE
      CALL BUG(67,IEOR(PRSI, 1))
C
42600      CALL NEWSTA(I,992,IEOR(HERE, 1),0,0)      ! obj falls to floor.
      IF(MATF) MATOBJ=I                  ! if mat, falls on that.
      GO TO 10                        ! finish put.
C
42700      CALL RSPSUB(993,ODO2)                  ! doesn't fit.
      RETURN

C NOBJS, PAGE 19
C
C O74--      Rusty key
C
43000      IF(PRSA/=TURNW) GO TO 10            ! turn?
      IF(PUNLKF) GO TO 39600                  ! unlock?
      GO TO 39200                        ! otherwise lock.
C
C O75--      Palantirs
C
44000      IF(PRSA/=LOOKIW) GO TO 10            ! look in?
      OBJ=PALAN                        ! assume dest = palan.
      IF(PRSO==PALAN) OBJ=PAL3            ! if palan, then pal3.
      IF(PRSO==PAL3) OBJ=SPHER            ! if pal3, then sphere.
      J=HERE                              ! save here.
      K=OCAN(OBJ)                        ! get dest container.
      IF(OROOM(OBJ)/=0) J=OROOM(OBJ)      ! if dest in room, use.
      IF(K/=0) J=OROOM(K)                  ! if cont, use cont room.
      IF((J==0).OR.(OADV(OBJ)==-THIEF)) GO TO 44200
      IF(.NOT.LIT(J)) GO TO 44200            ! see destination?
      IF(K==0) GO TO 44100                  ! contained?
      IF((IAND(OFLAG1(K), TRANBT)==0).AND..NOT.QOPEN(K))
     &      GO TO 44200                  ! see out of it?
44100      CALL RSPEAK(1024)                  ! start vision.
      OFLAG1(OBJ)=IAND(OFLAG1(OBJ), NOT(VISIBT))      ! object not visible.
      SVHERE=HERE                        ! save state.
      SVFLAG=RFLAG(J)
      F=MOVETO(J,WINNER)                  ! move to new room.
      F=RMDESC(3)                        ! describe.
      IF(J==SVHERE) CALL RSPEAK(1026)      ! was it here?
      F=MOVETO(SVHERE,WINNER)                  ! come back.
      RFLAG(J)=SVFLAG                        ! restore flags.
      OFLAG1(OBJ)=IOR(OFLAG1(OBJ), VISIBT)      ! restore object.
      RETURN
C
44200      CALL RSPEAK(1023)                  ! nothing to see.
      RETURN
C
C O76--      Mat
C
45000      IF((PRSA/=PUTUNW).OR.(PRSI/=PDOOR)) GO TO 45100
      MATF=.TRUE.                        ! put under right door.
      CALL NEWSTA(PRSO,983,HERE,0,0)
      RETURN
C
45100      IF(((PRSA/=TAKEW).AND.(PRSA/=MOVEW)).OR.(MATOBJ==0))
     &      GO TO 10                  ! take or move?
      CALL NEWSTA(MATOBJ,0,HERE,0,0)            ! materialize mat object.
      CALL RSPSUB(984,ODESC2(MATOBJ))
      MATOBJ=0
      MATF=.FALSE.
      IF(PRSA==TAKEW) GO TO 10            ! do normal take.
      RETURN                              ! move is done.
C
C O77--      Stove
C
46000      IF((PRSA/=TAKEW).AND.(PRSA/=RUBW).AND.
     &  (PRSA/=ATTACW).AND.(PRSA/=MUNGW))
     &  GO TO 46100                        ! take, feel, attack, mung?
      CALL RSPEAK(994)                  ! too hot.
      RETURN
C
46100      IF(PRSA/=THROWW) GO TO 10            ! throw at stove?
      IF(PRSO/=WATER) GO TO 46200            ! water?
      CALL NEWSTA(WATER,978,0,0,0)            ! evaporates.
      RETURN
C
46200      IF(IAND(OFLAG1(PRSO), BURNBT)==0) GO TO 10      ! burnable?
      CALL RSPSUB(974,ODO2)                  ! burns up.
      CALL NEWSTA(PRSO,0,0,0,0)            ! vanishes.

      END FUNCTION NOBJS

C MIRPAN--      Processor for global mirror/panel
C
C Declarations
C
      LOGICAL FUNCTION MIRPAN(ST,PNF)
      use state
      use subr
      use io,only: rspeak
      integer,intent(in) :: ST
      LOGICAL,intent(in) :: PNF
      
      integer mrbf,num
C
      MIRPAN=.TRUE.
      NUM=MRHERE(HERE)                  ! get mirror num.
      IF(NUM/=0) GO TO 100                  ! any here?
      CALL RSPEAK(ST)                        ! no, lose.
      RETURN
C
100      IF((PRSA/=MOVEW).AND.(PRSA/=OPENW)) GO TO 200
      CALL RSPEAK(ST+1)                  ! cant open or move.
      RETURN
C
200   MRBF=0                              ! assume mirror ok.
      IF(((NUM==1).AND..NOT.MR1F).OR.
     &  ((NUM==2).AND..NOT.MR2F)) MRBF=1
      IF(PNF.OR.((PRSA/=LOOKIW).AND.(PRSA/=EXAMIW).AND.
     &      (PRSA/=LOOKW))) GO TO 300
      CALL RSPEAK(844+MRBF)                  ! look in mirror.
      RETURN
C
300      IF(PRSA/=MUNGW) GO TO 400            ! break?
      CALL RSPEAK(ST+2+MRBF)                  ! do it.
      IF((NUM==1).AND..NOT.PNF) MR1F=.FALSE.
      IF((NUM==2).AND..NOT.PNF) MR2F=.FALSE.
      RETURN
C
400      IF(PNF.OR.(MRBF==0)) GO TO 500      ! broken mirror?
      CALL RSPEAK(846)
      RETURN
C
500      IF(PRSA/=PUSHW) GO TO 600            ! push?
      CALL RSPEAK(ST+3+NUM)
      RETURN
C
600      MIRPAN=.FALSE.                        ! cant handle it.

      END Function MIRPAN

C BALLOP-      Balloon function
C
C Declarations
C
      LOGICAL FUNCTION BALLOP(ARG)
      use state
      use subr
      use io,only: rspeak,rspsub
      integer, intent(in) :: arg

      BALLOP=.TRUE.                        ! assume wins.
      IF(ARG/=2) GO TO 200                  ! readout?
      IF(PRSA/=LOOKW) GO TO 10            ! only process look.
      IF(BINFF/=0) GO TO 50                  ! inflated?
      CALL RSPEAK(543)                  ! no.
      GO TO 100
50      CALL RSPSUB(544,ODESC2(BINFF))            ! yes.
100      IF(BTIEF/=0) CALL RSPEAK(545)            ! hooked?
      GO TO 10
C
200      IF(ARG/=1) GO TO 500                  ! readin?
      IF(PRSA/=WALKW) GO TO 300            ! walk?
      IF(FINDXT(PRSO,HERE)) GO TO 250            ! valid exit?
      CALL RSPEAK(546)                  ! no, joke.
      RETURN
C
250      IF(BTIEF==0) GO TO 275            ! tied up?
      CALL RSPEAK(547)                  ! yes, joke.
      RETURN
C
275      IF(XTYPE/=XNORM) GO TO 10            ! normal exit?
      IF(IAND(RFLAG(XROOM1), RMUNG)/=0) GO TO 10
      BLOC=XROOM1
      CFLAG(CEVBAL)=.TRUE.
      CTICK(CEVBAL)=3
10      BALLOP=.FALSE.
      RETURN
C
300      IF((PRSA/=TAKEW).OR.(PRSO/=BINFF)) GO TO 350
      CALL RSPSUB(548,ODESC2(BINFF))            ! recep cont too hot.
      RETURN
C
350      IF((PRSA/=PUTW).OR.(PRSI/=RECEP).OR.QEMPTY(RECEP))
     &      GO TO 10                  ! recep already full.
      CALL RSPEAK(549)
      RETURN
C
500      IF((PRSA/=BURNW).OR.(OCAN(PRSO)/=RECEP)) GO TO 10
      CALL RSPSUB(550,ODESC2(PRSO))            ! light fire in recep.
      CFLAG(CEVBRN)=.TRUE.
      CTICK(CEVBRN)=OSIZE(PRSO)*20
      OFLAG1(PRSO)=IAND(IOR(OFLAG1(PRSO), (ONBT+FLAMBT+LITEBT)),
     &      NOT(TAKEBT+READBT))            ! burn it.
      IF(BINFF/=0) RETURN                  ! already inflated?
      IF(.NOT.BLABF) CALL NEWSTA(BLABE,0,0,BALLO,0)      ! insert label.
      BLABF=.TRUE.                        ! only once.
      BINFF=PRSO
      CFLAG(CEVBAL)=.TRUE.
      CTICK(CEVBAL)=3                        ! start countdown.
      CALL RSPEAK(551)

      END FUnction BALLOP

C TROLLP-      Troll function
C

      LOGICAL FUNCTION TROLLP()
      use state
      use subr
      use io,only: rspsub,rspeak

      integer i

      TROLLP=.TRUE.                        ! assume wins.
      IF(PRSA/=FIGHTW) GO TO 1100            ! fight?
      IF(OCAN(AXE)==TROLL) GO TO 10            ! got axe?  nothing.
      I=433                              ! assume cant get.
      IF(.NOT.QHERE(AXE,HERE).OR.PROB(25,10)) GO TO 1050      ! here?
      I=434                              ! yes, recover.
      CALL NEWSTA(AXE,0,0,TROLL,0)
1050      IF(QHERE(TROLL,HERE)) CALL RSPEAK(I)      ! if player here.
      RETURN
C
1100      IF(PRSA/=DEADXW) GO TO 1200            ! dead?
      TROLLF=.TRUE.                        ! permit exits.
      RETURN
C
1200      IF(PRSA/=OUTXW) GO TO 1300            ! out?
      TROLLF=.TRUE.                        ! permit exits.
      OFLAG1(AXE)=IAND(OFLAG1(AXE), NOT(VISIBT))
      ODESC1(TROLL)=435                  ! troll out.
      RETURN
C
1300      IF((PRSA/=INXW).AND.                  ! wake from fight demon?
     &  (((PRSA/=ALARMW).AND.(PRSA/=KICKW)).OR.
     &    (OCAPAC(TROLL)>=0))) GO TO 1400      ! wake, kick while out?
      OCAPAC(TROLL)=IABS(OCAPAC(TROLL))      ! yes, wake him.
      OFLAG1(AXE)=IOR(OFLAG1(AXE), VISIBT)
      TROLLF=.FALSE.                        ! forbid exits.
      ODESC1(TROLL)=436                  ! troll in.
      IF(QHERE(TROLL,HERE)) CALL RSPEAK(437)
      RETURN
C
1400      IF(PRSA/=FRSTQW) GO TO 1500            ! first encounter?
      TROLLP=PROB(33,66)                  ! 33% true unless badlk.
      RETURN
C
1500      IF((PRSA/=MOVEW).AND.(PRSA/=TAKEW).AND.(PRSA/=MUNGW)
     &      .AND.(PRSA/=THROWW).AND.(PRSA/=GIVEW)) GO TO 2000
      IF(OCAPAC(TROLL)>=0) GO TO 1550      ! troll out?
      OCAPAC(TROLL)=IABS(OCAPAC(TROLL))      ! yes, wake him.
      OFLAG1(AXE)=IOR(OFLAG1(AXE), VISIBT)
      TROLLF=.FALSE.                        ! forbid exits.
      ODESC1(TROLL)=436                  ! troll in.
      CALL RSPEAK(437)
C
1550      IF((PRSA/=TAKEW).AND.(PRSA/=MOVEW)) GO TO 1600
      CALL RSPEAK(438)                  ! joke.
      RETURN
C
1600      IF(PRSA/=MUNGW) GO TO 1700            ! mung?
      CALL RSPEAK(439)                  ! joke.
      RETURN
C
1700      IF(PRSO==0) GO TO 10                  ! no object?
      I=440                              ! assume throw.
      IF(PRSA==GIVEW) I=441                  ! give?
      CALL RSPSUB(I,ODESC2(PRSO))            ! troll takes.
      IF(PRSO==KNIFE) GO TO 1900            ! obj knife?
      CALL NEWSTA(PRSO,442,0,0,0)            ! no, eats it.
      RETURN
C
1900      CALL RSPEAK(443)                  ! knife, throws it back
      OFLAG2(TROLL)=IOR(OFLAG2(TROLL), FITEBT)      ! and gets mad.
      RETURN
C
2000      IF(.NOT.TROLLF.OR.(PRSA/=HELLOW)) GO TO 10
      CALL RSPEAK(366)                  ! troll out.
      RETURN
C
10      TROLLP=.FALSE.                        ! couldnt handle it.

      END function trollp

C CYCLOP-      Cyclops function
C
C Declarations
C
      LOGICAL FUNCTION CYCLOP()
      use state
      use subr,only: newsta
      use io,only: rspeak

      integer i

      CYCLOP=.TRUE.                        ! assume wins.
      IF(.NOT.CYCLOF) GO TO 100            ! asleep?
      IF((PRSA/=ALARMW).AND.(PRSA/=MUNGW).AND.(PRSA/=KICKW).AND.
     &      (PRSA/=BURNW).AND.(PRSA/=KILLW).AND.(PRSA/=ATTACW))
     &       GO TO 10
      CYCLOF=.FALSE.                        ! wake cyclops.
      CALL RSPEAK(187)                  ! describe.
      RVCYC=IABS(RVCYC)
      OFLAG2(CYCLO)=IOR(OFLAG2(CYCLO), FITEBT)
      RETURN
C
100      IF(PRSA/=GIVEW) GO TO 500            ! give?
      IF(PRSO/=FOOD) GO TO 300             ! food?
      IF(RVCYC<0) GO TO 200            ! already eaten?
      CALL NEWSTA(FOOD,189,0,0,0)            ! eats peppers.
      RVCYC=MIN0(-1,-RVCYC)                  ! gets thirsty.
200      CFLAG(CEVCYC)=.TRUE.                  ! turn on cyclops timer.
      CTICK(CEVCYC)=-1
      RETURN
C
300      IF(PRSO/=WATER) GO TO 400            ! drink when thirsty?
      IF(RVCYC>=0) GO TO 350
      CALL NEWSTA(PRSO,190,0,0,0)            ! drinks and
      CYCLOF=.TRUE.                        ! falls asleep.
      OFLAG2(CYCLO)=IAND(OFLAG2(CYCLO), NOT(FITEBT))
      CFLAG(CEVCYC)=.FALSE.                  ! turn off cyclops timer.
      RETURN
C
350      CALL RSPEAK(191)                  ! not thirsty.
10      CYCLOP=.FALSE.                        ! fails.
      RETURN
C
400      I=192                              ! assume inedible.
      IF(PRSO==GARLI) I=193                  ! garlic is joke.
450      CALL RSPEAK(I)                        ! disdain it.
      RETURN
C
500      IF((PRSA/=KILLW).AND.(PRSA/=ATTACW).AND.
     &  (PRSA/=MUNGW).AND.(PRSA/=THROWW)) GO TO 600
      CFLAG(CEVCYC)=.TRUE.                  ! turn on cyclops timer.
      CTICK(CEVCYC)=-1
      I=201                              ! assume not poke.
      IF(PRSA==MUNGW) I=912                  ! poke joke.
      GO TO 450                        ! go remark and return.
C
600      I=202                              ! assume take.
      IF(PRSA==TAKEW) GO TO 450
      I=203                              ! assume tie.
      IF(PRSA==TIEW) GO TO 450
      CYCLOP=.FALSE.

      END function cyclop

C THIEFP-      Thief function
C
C Declarations
C
      LOGICAL FUNCTION THIEFP()
      use state
      use subr,only: qhere,newsta,prob,qempty,princo
      use io,only: rspeak,rspsub

  
      integer i,j

      THIEFP=.TRUE.                        ! assume wins.
      IF(PRSA/=FIGHTW) GO TO 100            ! fight?
      IF(OCAN(STILL)==THIEF) GO TO 10      ! got stilletto?  f.
      IF(QHERE(STILL,THFPOS)) GO TO 50      ! can he recover it?
      CALL NEWSTA(THIEF,0,0,0,0)            ! no, vanish.
      IF(QHERE(THIEF,HERE)) CALL RSPEAK(498)      ! if hero, tell.
      RETURN
C
50      CALL NEWSTA(STILL,0,0,THIEF,0)            ! yes, recover.
      IF(QHERE(THIEF,HERE)) CALL RSPEAK(499)      ! if hero, tell.
      RETURN
C
100      IF(PRSA/=DEADXW) GO TO 200            ! dead?
      THFACT=.FALSE.                        ! disable demon.
      IF(HERE/=TREAS) GO TO 150            ! in treasure room?
      J=501
      DO 125 I=1,OLNT                        ! loop.
        IF((I==CHALI).OR.(I==THIEF).OR..NOT.QHERE(I,HERE))
     &      GO TO 125                  ! is it here?
        OFLAG1(I)=IOR(OFLAG1(I), VISIBT)      ! rematerialize objects.
        CALL RSPSUB(J,ODESC2(I))            ! describe.
        J=502
        IF(.NOT.QEMPTY(I).AND.
     &   ((IAND(OFLAG1(I), TRANBT)/=0).OR.
     &    (IAND(OFLAG2(I), OPENBT)/=0))) CALL PRINCO(I,573,.TRUE.)
125      CONTINUE
C
150      J=500
      DO 175 I=1,OLNT                        ! carrying anything?
        IF(OADV(I)/=-THIEF) GO TO 175
        CALL NEWSTA(I,0,HERE,0,0)            ! drop in room.
        CALL RSPSUB(J,ODESC2(I))            ! describe.
        J=502
        IF(.NOT.QEMPTY(I).AND.
     &   ((IAND(OFLAG1(I), TRANBT)/=0).OR.
     &    (IAND(OFLAG2(I), OPENBT)/=0))) CALL PRINCO(I,573,.TRUE.)
175      CONTINUE
      RETURN
C
200      IF(PRSA/=FRSTQW) GO TO 250            ! first encounter?
      THIEFP=PROB(20,75)
      RETURN
C
250      IF((PRSA/=HELLOW).OR.(OCAPAC(THIEF)>=0)) GO TO 300
      CALL RSPEAK(626)                  ! hello to out thief?
      RETURN
C
300      IF(PRSA/=OUTXW) GO TO 400            ! out?
      THFACT=.FALSE.                        ! disable demon.
      ODESC1(THIEF)=504                  ! change description.
      OFLAG1(STILL)=IAND(OFLAG1(STILL), NOT(VISIBT))
      RETURN
C
400      IF((PRSA/=INXW).AND.                  ! wake from fight demon?
     &  (((PRSA/=ALARMW).AND.(PRSA/=KICKW)).OR.
     &    (OCAPAC(THIEF)>=0))) GO TO 500      ! wake, kick while out?
      OCAPAC(THIEF)=IABS(OCAPAC(THIEF))      ! wake him up.
      IF(QHERE(THIEF,HERE)) CALL RSPEAK(505)      ! can hero see?
      THFACT=.TRUE.                        ! enable demon.
      ODESC1(THIEF)=503                  ! change description.
      OFLAG1(STILL)=IOR(OFLAG1(STILL), VISIBT)
      RETURN
C
500      IF(PRSA/=TAKEW) GO TO 600            ! take?
      CALL RSPEAK(506)                  ! joke.
      RETURN
C
600      IF((PRSA/=THROWW).OR.(PRSO/=KNIFE).OR.
     &      (IAND(OFLAG2(THIEF), FITEBT)/=0)) GO TO 700
      IF(PROB(10,10)) GO TO 650            ! threw knife, 10%?
      CALL RSPEAK(507)                  ! no, just makes
      OFLAG2(THIEF)=IOR(OFLAG2(THIEF), FITEBT)      ! thief mad.
      RETURN
C
650      J=508                              ! thief drops stuff.
      DO 675 I=1,OLNT
        IF(OADV(I)/=-THIEF) GO TO 675      ! thief carrying?
        J=509
        CALL NEWSTA(I,0,HERE,0,0)
675      CONTINUE
      CALL NEWSTA(THIEF,J,0,0,0)            ! thief vanishes.
      RETURN
C
700      IF(((PRSA/=THROWW).AND.(PRSA/=GIVEW)).OR.(PRSO==0).OR.
     &      (PRSO==THIEF)) GO TO 10      ! throw/give to thief?
      IF(OCAPAC(THIEF)>=0) GO TO 750
      OCAPAC(THIEF)=IABS(OCAPAC(THIEF))      ! wake him up.
      THFACT=.TRUE.                        ! enable demon.
      ODESC1(THIEF)=503                  ! change description.
      OFLAG1(STILL)=IOR(OFLAG1(STILL), VISIBT)
      CALL RSPEAK(510)
C
750      IF((PRSO/=BRICK).OR.(OCAN(FUSE)/=BRICK).OR.
     &      (CTICK(CEVFUS)==0)) GO TO 800
      CALL RSPEAK(511)                  ! thief refuses bomb.
      RETURN
C
800      IF(PRSO/=WATER) GO TO 850            ! water?
      CALL NEWSTA(WATER,1081,0,0,0)            ! slips through fingers.
      RETURN
C
850      CALL NEWSTA(PRSO,0,0,0,-THIEF)            ! thief takes gift.
      IF(OTVAL(PRSO)>0) GO TO 900            ! a treasure?
      CALL RSPSUB(512,ODESC2(PRSO))
      RETURN
C
900      CALL RSPSUB(627,ODESC2(PRSO))            ! thief engrossed.
      THFENF=.TRUE.
      RETURN
C
10      THIEFP=.FALSE.

      END Function THIEFP
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

C LITINT-      Light interrupt processor
C
C Declarations
C
      SUBROUTINE LITINT(OBJ,CTR,CEV,TICKS,TICKLN)
      use state
      use io,only: rspsub,rspeak
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
      use subr,only: fights,fwim,prob,rnd,vilstr,jigsup,newsta
      use io,only: rspeak,rspsub

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
      use io,only: rspeak
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
                             ! disable demon.
      END SUBROUTINE SWORDD

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
      use io,only: rspeak
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
      use subr,only:robrm,prob,lit,qhere,winnin,robadv,newsta
      use io,only: rspeak,rspsub
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

      END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	
C CLOCKD- Intermove clock events demon
C
C Declarations


      LOGICAL FUNCTION CLOCKD()
      use state,only: clnt,cflag,ctick,cactio

      integer i

      CLOCKD=.FALSE.                        ! assume no action.
      DO 100 I=1,CLNT
        IF(.NOT.CFLAG(I) .OR.(CTICK(I)==0)) GO TO 100
        IF(CTICK(I)<0) GO TO 50            ! permanent entry?
        CTICK(I)=CTICK(I)-1
        IF(CTICK(I)/=0) GO TO 100            ! timer expired?
50        CLOCKD=.TRUE.
        CALL CEVAPP(CACTIO(I))            ! do action.
100      CONTINUE

      END FUNCTION CLOCKD

C CEVAPP- Clock event applicables
C
C Declarations
C
      SUBROUTINE CEVAPP(RI)
      use state
      use subr,only: lit,prob,moveto,mrhere,qhere,findxt,newsta,jigsup,
     & scrupd,bug
      use io,only: rspeak,rspsub
      integer,intent(in) :: ri


      INTEGER CNDTCK(10),LMPTCK(12),r,j,i,bc,br
      LOGICAL QOPEN
      LOGICAL F,QLEDGE,WASLIT
C
C Functions and data
C
      QOPEN(R)=IAND(OFLAG2(R), OPENBT)/=0
      QLEDGE(R)=(R==LEDG2).OR.(R==LEDG3).OR.(R==LEDG4)
      DATA CNDTCK/50,20,10,5,0,156,156,156,157,0/
      DATA LMPTCK/50,30,20,10,4,0,154,154,154,154,155,0/
C
      IF(RI==0) RETURN                  ! ignore disabled.
      WASLIT=LIT(HERE)
      GO TO (1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,
     & 11000,12000,13000,14000,15000,16000,17000,18000,19000,
     & 20000,21000,22000,23000,24000,25000,26000,27000,28000,
     & 29000,30000),RI
      CALL BUG(3,RI)
C
C Return here to test for change in light.
C
50      IF(WASLIT.AND..NOT.LIT(HERE)) CALL RSPEAK(406)
      RETURN

C CEVAPP, PAGE 2
C
C CEV1--      Cure clock.  Let player slowly recover.
C
1000      ASTREN(PLAYER)=MIN0(0,ASTREN(PLAYER)+1)      ! recover.
      IF(ASTREN(PLAYER)>=0) RETURN            ! fully recovered?
      CFLAG(CEVCUR)=.TRUE.
      CTICK(CEVCUR)=30                  ! no, wait some more.
      RETURN
C
C CEV2--      Maint-room with leak.  Raise the water level.
C
2000      IF(HERE==MAINT) CALL RSPEAK(71+(RVMNT/2)) ! describe.
      RVMNT=RVMNT+1                        ! raise water level.
      IF(RVMNT<=16) RETURN                  ! if not full, exit.
      CTICK(CEVMNT)=0                        ! full, disable clock.
      RFLAG(MAINT)=IOR(RFLAG(MAINT), RMUNG)      ! mung room.
      RDESC1(MAINT)=80                  ! say it is full of water.
      IF(HERE==MAINT) CALL JIGSUP(81)      ! drown him if present.
      RETURN
C
C CEV3--      Lantern.  Describe growing dimness.
C
3000      CALL LITINT(LAMP,ORLAMP,CEVLNT,LMPTCK,12) ! do light interrupt.
      GO TO 50                        ! go see if now dark.
C
C CEV4--      Match.  Out it goes.
C
4000      CALL RSPEAK(153)                  ! match is out.
      OFLAG1(MATCH)=IAND(OFLAG1(MATCH), NOT(ONBT+FLAMBT+LITEBT))
      GO TO 50                        ! go see if now dark.
C
C CEV5--      Candle.  Describe growing dimness.
C
5000      CALL LITINT(CANDL,ORCAND,CEVCND,CNDTCK,10) ! do candle interrupt.
      GO TO 50                        ! go see if now dark.

C CEVAPP, PAGE 3
C
C CEV6--      Balloon.
C
6000      CFLAG(CEVBAL)=.TRUE.
      CTICK(CEVBAL)=3                        ! reschedule interrupt.
      F=AVEHIC(WINNER)==BALLO            ! see if in balloon.
      IF(BLOC==VLBOT) GO TO 6800            ! at bottom?
      IF(QLEDGE(BLOC)) GO TO 6700            ! on ledge?
      IF(QOPEN(RECEP).AND.(BINFF/=0))
     &      GO TO 6500                  ! inflated and recep open?
C
C Balloon is in midair and is deflated (or has receptacle closed).
C Fall to next room.
C
      IF(BLOC/=VAIR1) GO TO 6300            ! in vair1?
      BLOC=VLBOT                        ! yes, now at vlbot.
      CALL NEWSTA(BALLO,0,BLOC,0,0)
      IF(F) GO TO 6200                  ! in balloon?
      IF(QLEDGE(HERE).OR.(HERE==VLBOT))
     &      CALL RSPEAK(530)            ! if can see, describe.
      RETURN
C
6200      F=MOVETO(BLOC,WINNER)                  ! move him.
      IF(BINFF==0) GO TO 6250            ! in balloon.  inflated?
      CALL RSPEAK(531)                  ! yes, landed.
      F=RMDESC(0)                        ! describe.
      RETURN
C
6250      CALL NEWSTA(BALLO,532,0,0,0)            ! no, balloon & contents die.
      CALL NEWSTA(DBALL,0,BLOC,0,0)            ! insert dead balloon.
      IF(LASTIT==BALLO) LASTIT=DBALL      ! fix last it reference.
      AVEHIC(WINNER)=0                  ! not in vehicle.
      CFLAG(CEVBAL)=.FALSE.                  ! disable interrupts.
      CFLAG(CEVBRN)=.FALSE.
      RETURN
C
6300      BLOC=BLOC-1                        ! not in vair1, descend.
      CALL NEWSTA(BALLO,0,BLOC,0,0)
      IF(F) GO TO 6400                  ! is he in balloon?
      IF(QLEDGE(HERE).OR.(HERE==VLBOT))
     &      CALL RSPEAK(533)            ! if can see, describe.
      RETURN
C
6400      F=MOVETO(BLOC,WINNER)                  ! in balloon, move him.
      CALL RSPEAK(534)                  ! describe.
      F=RMDESC(0)
      RETURN
C
C Balloon is in midair and is inflated, up-up-and-away!
c
6500      IF(BLOC/=VAIR4) GO TO 6600            ! at vair4?
      CFLAG(CEVBRN)=.FALSE.                  ! disable interrupts.
      CFLAG(CEVBAL)=.FALSE.
      BINFF=0
      BLOC=VLBOT                        ! fall to bottom.
      CALL NEWSTA(BALLO,0,0,0,0)            ! balloon & contents die.
      CALL NEWSTA(DBALL,0,BLOC,0,0)            ! substitute dead balloon.
      IF(LASTIT==BALLO) LASTIT=DBALL      ! fix last it reference.
      IF(F) GO TO 6550                  ! was he in it?
      IF(QLEDGE(HERE)) CALL RSPEAK(535)      ! if can see, describe.
      IF(HERE==VLBOT) CALL RSPEAK(925)      ! if at bottom, describe
      RETURN
C
6550      CALL JIGSUP(536)                  ! in balloon at crash, die.
      RETURN
C
6600      BLOC=BLOC+1                        ! not at vair4, go up.
      CALL NEWSTA(BALLO,0,BLOC,0,0)
      IF(F) GO TO 6650                  ! in balloon?
      IF(QLEDGE(HERE).OR.(HERE==VLBOT))
     &      CALL RSPEAK(537)            ! if can see, describe.
      RETURN
C
6650      F=MOVETO(BLOC,WINNER)                  ! move player.
      CALL RSPEAK(538)                  ! describe.
      F=RMDESC(0)
      RETURN
C
C On ledge, goes to midair room whether inflated or not.
C
6700      BLOC=BLOC+(VAIR2-LEDG2)                  ! move to midair.
      CALL NEWSTA(BALLO,0,BLOC,0,0)
      IF(F) GO TO 6750                  ! in balloon?
      IF(QLEDGE(HERE).OR.(HERE==VLBOT))
     &      CALL RSPEAK(539)            ! if can see, describe.
      CFLAG(CEVVLG)=.TRUE.                  ! stranded.
      CTICK(CEVVLG)=10                  ! materialize gnome.
      RETURN
C
6750      F=MOVETO(BLOC,WINNER)                  ! move to new room.
      CALL RSPEAK(540)                  ! describe.
      F=RMDESC(0)
      RETURN
C
C At bottom, go up if inflated, do nothing if deflated.
C
6800      IF((BINFF==0).OR..NOT.QOPEN(RECEP)) RETURN
      BLOC=VAIR1                        ! inflated and open,
      CALL NEWSTA(BALLO,0,BLOC,0,0)            ! go up to vair1.
      IF(F) GO TO 6850                  ! in balloon?
      IF(QLEDGE(HERE).OR.(HERE==VLBOT))
     &      CALL RSPEAK(541)            ! if can see, describe.
      RETURN
C
6850      F=MOVETO(BLOC,WINNER)                  ! move player.
      CALL RSPEAK(542)
      F=RMDESC(0)
      RETURN

C CEVAPP, PAGE 4
C
C CEV7--      Balloon burnup.
C
7000      DO 7100 I=1,OLNT                  ! find burning object
        IF((RECEP==OCAN(I)).AND.(IAND(OFLAG1(I), FLAMBT)/=0))
     &      GO TO 7200                  ! in receptacle.
7100      CONTINUE
      CALL BUG(4,0)
C
7200      CALL NEWSTA(I,0,0,0,0)                  ! vanish object.
      BINFF=0                              ! uninflated.
      IF(HERE==BLOC) CALL RSPSUB(292,ODESC2(I))      ! describe.
      RETURN
C
C CEV8--      Fuse function.
C
8000      IF(OCAN(FUSE)/=BRICK) GO TO 8500      ! ignited brick?
      BR=OROOM(BRICK)                        ! get brick room.
      BC=OCAN(BRICK)                        ! get container.
      IF((BR==0).AND.(BC/=0)) BR=OROOM(BC)
      IF(BR==0) BR=HERE                  ! it's here...
      CALL NEWSTA(FUSE,0,0,0,0)            ! kill fuse.
      CALL NEWSTA(BRICK,0,0,0,0)            ! kill brick.
      IF(BR/=HERE) GO TO 8100            ! brick elsewhere?
C
      RFLAG(HERE)=IOR(RFLAG(HERE), RMUNG)      ! blew self.
      RDESC1(HERE)=114                  ! mung room.
      CALL JIGSUP(150)                  ! dead.
      RETURN
C
8100      CALL RSPEAK(151)                  ! boom.
      MUNGRM=BR                        ! save room that blew.
      CFLAG(CEVSAF)=.TRUE.
      CTICK(CEVSAF)=5                        ! set safe interrupt.
      IF(BR/=MSAFE) GO TO 8200            ! blew safe room?
      IF(BC/=SSLOT) RETURN                  ! was brick in safe?
      CALL NEWSTA(SSLOT,0,0,0,0)            ! kill slot.
      OFLAG2(SAFE)=IOR(OFLAG2(SAFE), OPENBT)      ! open safe.
      SAFEF=.TRUE.                        ! indicate safe blown.
      RETURN
C
8200      DO 8250 I=1,OLNT                  ! blew wrong room.
        IF(QHERE(I,BR) .AND. (IAND(OFLAG1(I), TAKEBT)/=0))
     &      CALL NEWSTA(I,0,0,0,0)            ! vanish contents.
8250      CONTINUE
      IF(BR/=LROOM) RETURN                  ! blew living room?
      DO 8300 I=1,OLNT
        IF(OCAN(I)==TCASE) CALL NEWSTA(I,0,0,0,0) ! kill trophy case.
8300      CONTINUE
      RETURN
C
8500      IF(QHERE(FUSE,HERE).OR.(OADV(FUSE)==WINNER))
     &      CALL RSPEAK(152)
      CALL NEWSTA(FUSE,0,0,0,0)            ! kill fuse.
      RETURN

C CEVAPP, PAGE 5
C
C CEV9--      Ledge munge.
C
9000      RFLAG(LEDG4)=IOR(RFLAG(LEDG4), RMUNG)      ! ledge collapses.
      RDESC1(LEDG4)=109
      IF(HERE==LEDG4) GO TO 9100            ! was he there?
      CALL RSPEAK(110)                  ! no, narrow escape.
      RETURN
C
9100      IF(AVEHIC(WINNER)/=0) GO TO 9200      ! in vehicle?
      CALL JIGSUP(111)                  ! no, dead.
      RETURN
C
9200      IF(BTIEF/=0) GO TO 9300            ! tied to ledge?
      CALL RSPEAK(112)                  ! no, no place to land.
      RETURN
C
9300      BLOC=VLBOT                        ! yes, crash balloon.
      CALL NEWSTA(BALLO,0,0,0,0)            ! balloon & contents die.
      CALL NEWSTA(DBALL,0,BLOC,0,0)            ! insert dead balloon.
      IF(LASTIT==BALLO) LASTIT=DBALL      ! fix last it reference.
      ODESC1(BTIEF)=1073                  ! restore description.
      BTIEF=0
      BINFF=0
      CFLAG(CEVBAL)=.FALSE.
      CFLAG(CEVBRN)=.FALSE.
      CALL JIGSUP(113)                  ! dead.
      RETURN
C
C CEV10--      Safe munge.
C
10000      RFLAG(MUNGRM)=IOR(RFLAG(MUNGRM), RMUNG)      ! mung target.
      RDESC1(MUNGRM)=114
      IF(HERE==MUNGRM) GO TO 10100            ! is he present?
      CALL RSPEAK(115)                  ! let him know.
      IF(MUNGRM/=MSAFE) RETURN
      CFLAG(CEVLED)=.TRUE.
      CTICK(CEVLED)=8                        ! start ledge clock.
      RETURN
C
10100      I=116                              ! he's dead,
      IF(IAND(RFLAG(HERE), RHOUSE)/=0) I=117      ! one way or another.
      CALL JIGSUP(I)                        ! let him know.
      RETURN

C CEVAPP, PAGE 6
C
C CEV11--      Volcano gnome entrance.
C
11000      IF(QLEDGE(HERE)) GO TO 11100            ! is he on ledge?
      CFLAG(CEVVLG)=.TRUE.
      CTICK(CEVVLG)=1                        ! no, wait a while.
      RETURN
C
11100      CALL NEWSTA(GNOME,118,HERE,0,0)            ! yes, materialize gnome.
      RETURN
C
C CEV12--      Volcano gnome exit.
C
12000      IF(OROOM(GNOME)==HERE) CALL RSPEAK(149) ! player here to hear?
      CALL NEWSTA(GNOME,0,0,0,0)            ! disappear the gnome.
      RETURN
C
C CEV13--      Bucket.
C
13000      IF(OCAN(WATER)==BUCKE)
     &      CALL NEWSTA(WATER,0,0,0,0)      ! water leaks out.
      RETURN
C
C CEV14--      Sphere.  If expires, he's trapped.
C
14000      RFLAG(CAGER)=IOR(RFLAG(CAGER), RMUNG)      ! mung room.
      RDESC1(CAGER)=147
      WINNER=PLAYER                        ! kill player, not robot.
      CALL JIGSUP(148)                  ! mung player.
      RETURN
C
C CEV15--      END GAME HERALD.
C
15000      ENDGMF=.TRUE.                        ! we're in endgame.
      CALL RSPEAK(119)                  ! inform of endgame.
      RETURN

C CEVAPP, PAGE 7
C
C CEV16--      Forest murmurs.
C
16000      CFLAG(CEVFOR)=(HERE==MTREE).OR.
     &      ((HERE>=FORE1).AND.(HERE<CLEAR))
      IF(CFLAG(CEVFOR).AND.PROB(10,10)) CALL RSPEAK(635)
      RETURN
C
C CEV17--      Scol alarm.
C
17000      IF(HERE==BKVAU) CALL JIGSUP(636)      ! if in vau, dead.
      IF(ZGNOMF.OR.(HERE/=BKTWI)) RETURN      ! if not in twi, nothing.
      ZGNOMF=.TRUE.                        ! gnome only comes once
      CFLAG(CEVZGI)=.TRUE.                  ! turn on gnome timer
      CTICK(CEVZGI)=5
      RETURN
C
C CEV18--      Gnome of Zurich entrance.
C
18000      IF(HERE/=BKTWI) RETURN            ! player here?
      CFLAG(CEVZGO)=.TRUE.                  ! exits, too.
      CTICK(CEVZGO)=12
      CALL NEWSTA(ZGNOM,637,BKTWI,0,0)      ! place in twi.
      RETURN
C
C CEV19--      Gnome of Zurich exits.
C
19000      CALL NEWSTA(ZGNOM,0,0,0,0)            ! vanish.
      IF(HERE==BKTWI) CALL RSPEAK(638)      ! announce.
      RETURN

C CEVAPP, PAGE 8
C
C CEV20--      Start of endgame.
C
20000      IF(SPELLF) GO TO 20200                  ! spell his way in?
      IF(HERE/=CRYPT) RETURN            ! no, still in tomb?
      IF(.NOT.LIT(HERE)) GO TO 20100            ! lights off?
      CFLAG(CEVSTE)=.TRUE.
      CTICK(CEVSTE)=3                        ! reschedule.
      RETURN
C
20100      CALL RSPEAK(727)                  ! announce.
20200      DO 20300 I=1,OLNT                  ! strip him of objs.
        CALL NEWSTA(I,0,OROOM(I),OCAN(I),0)
20300      CONTINUE
      CALL NEWSTA(LAMP,0,0,0,PLAYER)            ! give him lamp.
      CALL NEWSTA(SWORD,0,0,0,PLAYER)            ! give him sword.
C
      OFLAG1(LAMP)=IAND(IOR(OFLAG1(LAMP), LITEBT), NOT(ONBT))
      OFLAG2(LAMP)=IOR(OFLAG2(LAMP), TCHBT)
      CFLAG(CEVLNT)=.FALSE.                  ! lamp is good as new.
      CTICK(CEVLNT)=350
      ORLAMP=0
      OFLAG2(SWORD)=IOR(OFLAG2(SWORD), TCHBT)      ! recreate sword.
      SWDACT=.TRUE.
      SWDSTA=0
C
      THFACT=.FALSE.                        ! thief gone.
      ENDGMF=.TRUE.                        ! endgame running.
      CFLAG(CEVEGH)=.FALSE.                  ! herald gone,
      CFLAG(CEVMAT)=.FALSE.                  ! matches gone,
      CFLAG(CEVCND)=.FALSE.                  ! candles gone.
C
      CALL SCRUPD(RVAL(CRYPT))            ! score crypt,
      RVAL(CRYPT)=0                        ! but only once.
      F=MOVETO(TSTRS,WINNER)                  ! to top of stairs,
      F=RMDESC(3)                        ! and describe.
      RETURN                              ! bam!
C
C CEV21--      Mirror closes.
C
21000      MRPSHF=.FALSE.                        ! button is out.
      MROPNF=.FALSE.                        ! mirror is closed.
      IF(HERE==MRANT) CALL RSPEAK(728)      ! describe button.
      IF((HERE==INMIR).OR.(MRHERE(HERE)==1))
     &      CALL RSPEAK(729)            ! describe mirror.
      RETURN

C CEVAPP, PAGE 9
C
C CEV22--      Door closes.
C
22000      IF(WDOPNF) CALL RSPEAK(730)            ! describe.
      WDOPNF=.FALSE.                        ! closed.
      RETURN
C
C CEV23--      Inquisitor's question.
C
23000      IF(AROOM(PLAYER)/=FDOOR) RETURN      ! if player left, die.
      CALL RSPEAK(769)
      CALL RSPEAK(770+QUESNO)
      CFLAG(CEVINQ)=.TRUE.
      CTICK(CEVINQ)=2
      RETURN
C
C CEV24--      Master follows.
C
24000      IF(AROOM(AMASTR)==HERE) RETURN      ! no movement, done.
      IF((HERE/=CELL).AND.(HERE/=PCELL)) GO TO 24100
      IF(FOLLWF) CALL RSPEAK(811)            ! wont go to cells.
      FOLLWF=.FALSE.
      RETURN
C
24100      FOLLWF=.TRUE.                        ! following.
      I=812                              ! assume catches up.
      DO 24200 J=XMIN,XMAX,XMIN
        IF(FINDXT(J,AROOM(AMASTR)).AND.(XROOM1==HERE))
     &      I=813                        ! assume follows.
24200      CONTINUE
      CALL RSPEAK(I)
      CALL NEWSTA(MASTER,0,HERE,0,0)            ! move master object.
      AROOM(AMASTR)=HERE                  ! move master actor.
      RETURN
C
C CEV25--      Brochure arrives.
C
25000      CALL NEWSTA(BROCH,948,0,MAILB,0)      ! put brochure in mailbox
      BROC2F=.TRUE.                        ! flag arrival
      RETURN

C CEVAPP, PAGE 10
C
C CEV26--      Cyclops.
C
26000      IF(HERE/=MCYCL.OR.MAGICF) GO TO 26500      ! player or cyclops gone?
      IF(CYCLOF) RETURN                  ! if asleep, check later
      IF(IABS(RVCYC)<=5) GO TO 26200      ! cyclops overly annoyed?
      CFLAG(CEVCYC)=.FALSE.                  ! disable cyclops timer
      CALL JIGSUP(188)                  ! player munched for lunch
      RETURN
C
26200      IF(RVCYC<0) RVCYC=RVCYC-1            ! cyclops gets more annoyed
      IF(RVCYC>=0) RVCYC=RVCYC+1
      CALL RSPEAK(193+IABS(RVCYC))            ! report cyclops state
      RETURN
C
26500      CFLAG(CEVCYC)=.FALSE.                  ! disable cyclops timer
      RETURN
C
C CEV27--      Slippery slide.
C
27000      IF((HERE<SLID1).OR.(HERE>=SLEDG)) RETURN      ! in slide?
      CALL RSPEAK(1034)                  ! slide to cellar
      F=MOVETO(CELLA,WINNER)                  ! into cellar
      F=RMDESC(3)                        ! describe
      RETURN
C
C CEV28--      Exorcism bell.
C
28000      IF(.NOT.EXORCF.AND.(HERE==LLD1)) CALL RSPEAK(970)
      EXORBF=.FALSE.                        ! spell broken
      RETURN
C
C CEV29--      Exorcism candles.
C
29000      EXORCF=.FALSE.                        ! spell broken
      GO TO 28000
C
C CEV30--      Hot bell cools down.
C
30000      CALL NEWSTA(HBELL,0,0,0,0)            ! banish hot bell
      CALL NEWSTA(BELL,0,LLD1,0,0)            ! get normal bell
      IF(LASTIT==HBELL) LASTIT=BELL            ! fix last it reference.
      IF(HERE==LLD1) CALL RSPEAK(971)      ! tell player if here

      END SUBROUTINE CEVAPP


	! RMDESC-- Print room description
C
C RMDESC prints a description of the current room.
C It is also the processor for verbs 'LOOK' and 'EXAMINE'
C when there is no direct object.
C
      LOGICAL FUNCTION RMDESC(FULL)
      use state
      use subr,only: lit,prob,princr
      use io,only: rspeak,rspsub
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

      END function rmdesc

      SUBROUTINE RAPPLI(RI)
      use state
      use subr,only:fights,qhere,qempty,moveto,lit,prob,rnd,newsta,
     & jigsup,bug,scrupd,score,cpinfo
      use io,only: rspeak,rspsub,rspsb2
	  integer, intent(in) :: RI
    
      
      integer i,r,j

	LOGICAL QOPEN,QON,F
	
	QOPEN(R)=IAND(OFLAG2(R), OPENBT)/=0
	QON(R)=IAND(OFLAG1(R), ONBT) /= 0
C
	IF(RI==0) RETURN			! return if naught.
	GO TO (  1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000,
	1 10000,11000,12000,13000,14000,15000,16000,17000,18000,19000,
	2 20000,21000,22000,23000,24000,25000,26000,27000,28000,29000,
	3 30000,31000,32000,33000,34000,35000,36000,37000,38000,39000,
	4 40000,41000,42000,43000,44000,45000,46000,47000,48000,49000,
	5 50000,51000,52000,53000,54000,55000,56000,57000,58000,59000,
	7 60000,61000,62000,63000,64000),RI
	CALL BUG(1,RI)

C RAPPLI, PAGE 2
C
C R1--	East of house
C
1000	IF(PRSA.NE.LOOKW) RETURN		! look?
	I=13					! assume closed.
	IF(QOPEN(WINDO)) I=12			! if open, ajar.
	CALL RSPSUB(11,I)			! describe.
	RETURN
C
C R2--	Kitchen
C
2000	IF(PRSA.NE.LOOKW) GO TO 2100		! look?
	I=13					! assume closed.
	IF(QOPEN(WINDO)) I=12			! if open, ajar.
	CALL RSPSUB(14,I)			! describe.
	RETURN
C
2100	IF((PRSA.NE.WALKIW).OR.DEADF.OR..NOT.BROC1F.OR.BROC2F) RETURN
	CFLAG(CEVBRO)=.TRUE.			! send for brochure.
	CTICK(CEVBRO)=3
	RETURN
C
C R3--	Living Room
C
3000	IF(PRSA.NE.LOOKW) GO TO 3500		! look?
	I=15					! assume no hole.
	IF(MAGICF) I=16				! if magicf, cyclops hole.
	CALL RSPEAK(I)				! describe.
	I=17+ORRUG				! assume initial state.
	IF(QOPEN(DOOR)) I=I+2			! door open?
	CALL RSPEAK(I)				! describe.
	RETURN
C
C Not a look word, reevaluate trophy case.
C
3500	IF((PRSA.NE.TAKEW).AND.((PRSA.NE.PUTW).OR.(PRSI.NE.TCASE)))
	1	RETURN				! take or put in?
	ASCORE(WINNER)=RWSCOR			! score trophy case.
	DO 3600 I=1,OLNT			! retain raw score as well.
	  J=I					! find out if in case.
3550	  J=OCAN(J)				! trace ownership.
	  IF(J.EQ.0) GO TO 3600
	  IF(J.NE.TCASE) GO TO 3550		! do all levels.
	  ASCORE(WINNER)=ASCORE(WINNER)+OTVAL(I)
3600	CONTINUE
	CALL SCRUPD(0)				! see if endgame trig.
	RETURN

C RAPPLI, PAGE 3
C
C R4--	Cellar
C
4000	IF(PRSA.NE.LOOKW) GO TO 4500		! look?
	CALL RSPEAK(21)				! describe cellar.
	IF(QOPEN(DOOR)) CALL RSPEAK(623)	! describe trap door if open.
	RETURN
C
4500	IF(PRSA.NE.WALKIW) RETURN		! walkin?
	IF(IAND(OFLAG2(DOOR), (OPENBT+TCHBT)).NE.OPENBT) RETURN
	OFLAG2(DOOR)=IAND(IOR(OFLAG2(DOOR), TCHBT), NOT(OPENBT))
	CALL RSPEAK(22)				! slam and bolt door.
	RETURN
C
C R5--	Grating Room
C
5000	IF(PRSA.NE.LOOKW) RETURN		! look?
	CALL RSPEAK(23)				! describe.
	I=24					! assume locked.
	IF(GRUNLF) I=26				! unlocked?
	IF(QOPEN(GRATE)) I=25			! open?
	CALL RSPEAK(I)				! describe grate.
	RETURN
C
C R6--	Clearing
C
6000	IF(PRSA.NE.LOOKW) RETURN		! look?
	CALL RSPEAK(27)				! describe.
	I=0					! assume no grating.
	IF(RVCLR.NE.0) I=28			! leaves moved?
	IF(QOPEN(GRATE)) I=29			! grate open?
	CALL RSPEAK(I)				! describe grate.
	RETURN

C RAPPLI, PAGE 4
C
C R7--	Reservoir south
C
7000	IF(PRSA.NE.LOOKW) RETURN		! look?
	I=31					! assume full.
	IF(LWTIDF) I=32				! if low tide, empty.
	CALL RSPEAK(I)				! describe.
	CALL RSPEAK(33)				! describe exits.
	RETURN
C
C R8--	Reservoir
C
8000	IF(PRSA.NE.LOOKW) RETURN		! look?
	I=34					! assume full.
	IF(LWTIDF) I=35				! if low tide, emtpy.
	CALL RSPEAK(I)				! describe.
	RETURN
C
C R9--	Reservoir north
C
9000	IF(PRSA.NE.LOOKW) RETURN		! look?
	I=36					! you get the idea.
	IF(LWTIDF) I=37
	CALL RSPEAK(I)
	CALL RSPEAK(38)
	RETURN
C
C R10--	Glacier Room
C
10000	IF(PRSA.NE.LOOKW) RETURN		! look?
	CALL RSPEAK(39)				! basic description.
	I=0					! assume no changes.
	IF(GLACMF) I=40				! partial melt?
	IF(GLACRF) I=41				! complete melt?
	CALL RSPEAK(I)				! describe.
	RETURN
C
C R11--	Forest Room
C
11000	IF(PRSA.NE.WALKIW) RETURN
	CFLAG(CEVFOR)=.TRUE.			! if walk in, birdie.
	CTICK(CEVFOR)=-1
	RETURN
C
C R12--	Mirror Room
C
12000	IF(PRSA.NE.LOOKW) RETURN		! look?
	CALL RSPEAK(42)				! describe.
	IF(MIRRMF) CALL RSPEAK(43)		! if broken, nasty remark.
	RETURN

C RAPPLI, PAGE 5
C
C R13--	Cave2
C
13000	IF(PRSA.NE.WALKIW) RETURN		! walkin?
	IF(PROB(50,20).OR.(OADV(CANDL).NE.WINNER).OR.
	1	.NOT.QON(CANDL)) RETURN		! blow em out?
	OFLAG1(CANDL)=IAND(OFLAG1(CANDL), NOT(ONBT))	! yes.
	CALL RSPEAK(47)				! tell of winds.
	CFLAG(CEVCND)=.FALSE.			! halt candle countdown.
	IF(.NOT.LIT(HERE)) CALL RSPEAK(406)	! now pitch black.
	RETURN
C
C R14--	Boom Room
C
14000	J=ODESC2(CANDL)				! assume candle.
	IF((OADV(CANDL).EQ.WINNER).AND.QON(CANDL)) GO TO 14100
	J=ODESC2(TORCH)				! assume torch.
	IF((OADV(TORCH).EQ.WINNER).AND.QON(TORCH)) GO TO 14100
	J=ODESC2(MATCH)
	IF((OADV(MATCH).EQ.WINNER).AND.QON(MATCH)) GO TO 14100
	RETURN					! safe
C
14100	IF((PRSA.NE.TRNONW).AND.(PRSA.NE.BURNW))
	1	GO TO 14200			! turn on or burn?
	CALL RSPSUB(294,J)			! boom!
	CALL JIGSUP(44)
	RETURN
C
14200	IF(PRSA.NE.WALKIW) RETURN		! walkin?
	CALL RSPSUB(295,J)			! boom!
	CALL JIGSUP(44)
	RETURN
C
C R15--	No-objs
C
15000	EMPTHF=.TRUE.				! assume true.
	DO 15100 I=1,OLNT			! see if carrying.
	  IF(OADV(I).EQ.WINNER) EMPTHF=.FALSE.
15100	CONTINUE
C
	IF((HERE.NE.BSHAF).OR.(.NOT.LIT(HERE))) RETURN
	CALL SCRUPD(LTSHFT)			! score light shaft.
	LTSHFT=0				! never again.
	RETURN

C RAPPLI, PAGE 6
C
C R16--	Machine Room
C
16000	IF(PRSA.NE.LOOKW) RETURN		! look?
	I=46					! assume lid closed.
	IF(QOPEN(MACHI)) I=12			! if open, open.
	CALL RSPSUB(45,I)			! describe.
	RETURN
C
C R17--	Bat Room
C
17000	IF(PRSA.NE.LOOKW) GO TO 17500		! look?
	CALL RSPEAK(48)				! describe room.
	IF(OADV(GARLI).EQ.WINNER) CALL RSPEAK(49) ! bat holds nose.
	RETURN
C
17500	IF((PRSA.NE.WALKIW).OR.(OADV(GARLI).EQ.WINNER)
	1	.OR.DEADF) RETURN		! garlic or dead?
	CALL RSPEAK(50)				! time to fly, jack.
	F=MOVETO(BATDRP(RND(9)+1),WINNER)	! select random dest.
	F=RMDESC(0)				! new room description.
	PRSCON=0				! kill parser.
	RETURN
C
C R18--	Dome Room
C
18000	IF(PRSA.NE.LOOKW) GO TO 18500		! look?
	CALL RSPEAK(51)				! describe.
	IF(DOMEF) CALL RSPEAK(52)		! if rope, describe.
	RETURN
C
18500	IF(PRSA.EQ.LEAPW) CALL JIGSUP(53)	! did he jump???
	RETURN
C
C R19--	Torch Room
C
19000	IF(PRSA.NE.LOOKW) RETURN		! look?
	CALL RSPEAK(54)				! describe.
	IF(DOMEF) CALL RSPEAK(55)		! if rope, describe.
	RETURN
C
C R20--	Carousel Room
C
20000	IF(PRSA.NE.LOOKW) GO TO 20500		! look?
	CALL RSPEAK(56)				! describe.
	IF(.NOT.(CAROFF.OR.DEADF)) CALL RSPEAK(57) ! if not flipped, spin.
	RETURN
C
20500	IF((PRSA.EQ.WALKIW).AND.CAROZF.AND..NOT.DEADF)
	1	CALL JIGSUP(58)			! walked in, zoom, not dead.
	RETURN

C RAPPLI, PAGE 7
C
C R21--	Land of the Living Dead
C
21000	IF(PRSA.NE.LOOKW) GO TO 21100		! look?
	CALL RSPEAK(59)				! describe.
	IF(.NOT.LLDF) CALL RSPEAK(60)		! if not vanished, ghosts.
	RETURN
C
21100	IF(LLDF.OR.(PRSA.NE.RINGW).OR.(PRSO.NE.BELL))
	1	GO TO 21200			! ring bell?
	EXORBF=.TRUE.				! set exorcism bell flag.
	CALL NEWSTA(BELL,0,0,0,0)		! vanish bell.
	CALL NEWSTA(HBELL,967,HERE,0,0)		! insert hot bell.
	IF(LASTIT.EQ.BELL) LASTIT=HBELL		! fix last it reference.
	IF(.NOT.QON(CANDL).OR.(OADV(CANDL).NE.WINNER))
	1	GO TO 21150			! carrying lit candles?
	CALL NEWSTA(CANDL,968,HERE,0,0)		! drop and go out.
	OFLAG1(CANDL)=IAND(OFLAG1(CANDL), NOT(ONBT))
	CFLAG(CEVCND)=.FALSE.			! disable candle timer.
21150	CFLAG(CEVXB)=.TRUE.			! start bell timer.
	CTICK(CEVXB)=6
	CFLAG(CEVXBH)=.TRUE.			! start cooling timer.
	CTICK(CEVXBH)=20
	RETURN
C
21200	IF(.NOT.EXORBF.OR.EXORCF.OR.(OADV(CANDL).NE.WINNER).OR.
	1	(IAND(OFLAG1(CANDL), ONBT).EQ.0)) GO TO 21300
	EXORCF=.TRUE.				! set exorcism candle flag.
	CALL RSPEAK(969)
	CFLAG(CEVXB)=.FALSE.			! turn off bell timer.
	CFLAG(CEVXC)=.TRUE.			! turn on candle timer.
	CTICK(CEVXC)=3
	RETURN
C
21300	IF(.NOT.EXORCF.OR.(PRSA.NE.READW).OR.(PRSO.NE.BOOK))
	1	GO TO 21400			! read book?
	CALL NEWSTA(GHOST,63,0,0,0)		! exorcism complete.
	LLDF=.TRUE.				! set flag.
	CFLAG(CEVXC)=.FALSE.			! turn off candle timer.
	RETURN
C
21400	IF(PRSA.NE.EXORCW) RETURN		! trying exorcism?
	IF(LLDF) GO TO 21600			! trying again?
	IF((OADV(BELL).EQ.WINNER).AND.(OADV(BOOK).EQ.WINNER).AND.
	1	(OADV(CANDL).EQ.WINNER).AND.QON(CANDL)) GO TO 21500
	CALL RSPEAK(62)				! not equipped.
	RETURN
C
21500	CALL RSPEAK(1044)			! must do it the hard way.
	RETURN
C
21600	CALL JIGSUP(61)				! twice, exorcise you.
	RETURN

C RAPPLI, PAGE 7A
C
C R22--	Land of the Living Dead interior
C
22000	IF(PRSA.NE.LOOKW) RETURN		! look?
	CALL RSPEAK(64)				! describe.
	IF(ONPOLF) CALL RSPEAK(65)		! on pole?
	RETURN
C
C R23--	Dam Room
C
23000	IF(PRSA.NE.LOOKW) RETURN		! look?
	CALL RSPEAK(66)				! describe.
	I=67
	IF(LWTIDF) I=68
	CALL RSPEAK(I)				! describe reservoir.
	CALL RSPEAK(69)				! describe panel.
	IF(GATEF) CALL RSPEAK(70)		! bubble is glowing.
	RETURN
C
C R24--	Tree Room
C
24000	IF(PRSA.NE.LOOKW) GO TO 24300		! look?
	CALL RSPEAK(660)			! describe.
	I=661					! set flag for below.
	DO 24200 J=1,OLNT			! describe obj in fore3.
	  IF(.NOT.QHERE(J,FORE3).OR.(J.EQ.FTREE)) GO TO 24200
	  CALL RSPEAK(I)			! set stage,
	  I=0
	  CALL RSPSUB(502,ODESC2(J))		! describe.
24200	CONTINUE
	RETURN
C
24300	IF(PRSA.NE.WALKIW) GO TO 24400		! walk in?
	CFLAG(CEVFOR)=.TRUE.			! start forest noise timer.
	CTICK(CEVFOR)=-1
	RETURN
C
24400	IF((PRSA.NE.DROPW).AND.(PRSA.NE.THROWW).AND.(PRSA.NE.SHAKEW))
	1	RETURN				! drop, throw, shake?
	DO 24600 I=1,OLNT			! loop through objects
	  IF((I.EQ.TTREE).OR.(I.EQ.NEST).OR.
	1	.NOT.QHERE(I,HERE)) GO TO 24600 !  is it here?
	  IF(I.EQ.EGG) GO TO 24500		! egg?
	  CALL NEWSTA(I,0,FORE3,0,0)		! no, drop to forest floor.
	  CALL RSPSUB(659,ODESC2(I))
	  GO TO 24600
C
24500	  CALL NEWSTA(EGG,0,0,0,0)		! vanish egg.
	  CALL NEWSTA(BEGG,658,FORE3,0,0)	! insert broken egg.
	  IF(LASTIT.EQ.EGG) LASTIT=BEGG		! fix last it reference.
	  OTVAL(BEGG)=2
	  IF(OCAN(CANAR).NE.EGG) GO TO 24550	! canary inside?
	  OTVAL(BCANA)=1
	  GO TO 24600
24550	  CALL NEWSTA(BCANA,0,0,0,0)		! no, vanish broken canary.
24600	CONTINUE
	RETURN

C RAPPLI, PAGE 8
C
C R25--	Cyclops Room
C
25000	IF(PRSA.NE.LOOKW) GO TO 25100		! look?
	CALL RSPEAK(606)			! describe.
	I=607					! assume basic state.
	IF(RVCYC.GT.0) I=608			! >0?  hungry.
	IF(RVCYC.LT.0) I=609			! <0?  thirsty.
	IF(CYCLOF) I=610			! asleep?
	IF(MAGICF) I=611			! gone?
	CALL RSPEAK(I)				! describe.
	RETURN
C
25100	IF((PRSA.NE.WALKIW).OR.(RVCYC.EQ.0).OR.DEADF) RETURN
	CFLAG(CEVCYC)=.TRUE.			! walked in, restart timer.
	CTICK(CEVCYC)=-1
	RETURN
C
C R26--	Bank Box Room
C
26000	IF(PRSA.NE.WALKIW) RETURN		! surprise him.
	DO 26100 I=1,8,2			! scolrm depends on
	  IF(FROMDR.EQ.SCOLDR(I)) SCOLRM=SCOLDR(I+1)
26100	CONTINUE				! entry direction.
	RETURN
C
C R27--	Treasure Room
C
27000	IF((PRSA.NE.WALKIW).OR.DEADF.OR..NOT.THFACT)
	1	RETURN				! walkin, thief active?
	IF(OROOM(THIEF).NE.HERE)
	1	CALL NEWSTA(THIEF,82,HERE,0,0)	! no, materialize him.
	THFPOS=HERE				! reset search pattern.
	OFLAG2(THIEF)=IOR(OFLAG2(THIEF), FITEBT)	! he's angry.
C
C Vanish everything in room.
C
	J=0					! assume nothing to vanish.
	DO 27200 I=1,OLNT
	  IF((I.EQ.CHALI).OR.(I.EQ.THIEF).OR..NOT.QHERE(I,HERE))
	1	GO TO 27200			! here?
	  J=83					! flag byebye.
	  OFLAG1(I)=IAND(OFLAG1(I), NOT(VISIBT))	! away it goes.
27200	CONTINUE
	CALL RSPEAK(J)				! describe.
	RETURN
C
C R28--	Cliff Room
C
28000	DEFLAF=OADV(RBOAT).NE.WINNER		! true if not carrying.
	RETURN

C RAPPLI, PAGE 9
C
C R29--	Rivr4 Room
C
29000	IF(BUOYF.OR.(OADV(BUOY).NE.WINNER)) RETURN
	CALL RSPEAK(84)				! give hint,
	BUOYF=.TRUE.				! then disable.
	RETURN
C
C R30--	Overfalls
C
30000	IF(PRSA.NE.LOOKW) CALL JIGSUP(85)	! over you go.
	RETURN
C
C R31--	Slide Ledge
C
31000	IF(PRSA.NE.WALKIW) RETURN		! walk in?
	CFLAG(CEVSLI)=.FALSE.			! disable slippery rope.
	RETURN
C
C R32--	Slide
C
32000	IF(PRSA.NE.LOOKW) RETURN		! look?
	CALL RSPEAK(1012)			! describe.
	IF(TTIE.EQ.0) RETURN			! timber tied?
	IF(OROOM(TTIE).EQ.HERE) CALL RSPSUB(1013,ODESC2(TTIE))
	RETURN
C
C R33--	Aragain Falls
C
33000	IF(PRSA.NE.LOOKW) RETURN		! look?
	CALL RSPEAK(96)				! describe.
	I=97					! assume no rainbow.
	IF(RAINBF) I=98				! got one?
	CALL RSPEAK(I)				! describe.
	RETURN

C RAPPLI, PAGE 10
C
C R34--	Ledge Room
C
34000	IF(PRSA.NE.LOOKW) RETURN		! look?
	CALL RSPEAK(100)			! describe.
	I=102					! assume safe room ok.
	IF(IAND(RFLAG(MSAFE), RMUNG).NE.0) I=101	! if munged, room gone.
	CALL RSPEAK(I)				! describe.
	RETURN
C
C R35--	Safe Room
C
35000	IF(PRSA.NE.LOOKW) RETURN		! look?
	CALL RSPEAK(104)			! describe.
	I=105					! assume ok.
	IF(SAFEF) I=106				! blown?
	CALL RSPEAK(I)				! describe.
	RETURN
C
C R36--	Magnet Room
C
36000	IF(PRSA.NE.LOOKW) GO TO 36500		! look?
	CALL RSPEAK(107)			! describe.
	RETURN
C
36500	IF((PRSA.NE.WALKIW).OR.DEADF.OR..NOT.CAROFF) RETURN	! walkin?
	IF(CAROZF) GO TO 36600			! zoom?
	IF(WINNER.EQ.PLAYER) CALL RSPEAK(108)	! no, spin his compass.
	RETURN
C
36600	I=58					! spin his insides.
	IF(WINNER.NE.PLAYER) I=99		! spin robot.
	CALL JIGSUP(I)				! dead.
	RETURN
C
C R37--	Cage Room
C
37000	IF(CAGESF) F=MOVETO(CAGER,WINNER)	! if solved, move.
	RETURN

C RAPPLI, PAGE 11
C
C R38--	Mirror D Room
C
38000	IF(PRSA.EQ.LOOKW) CALL LOOKTO(FDOOR,MRG,0,682,681)
	RETURN
C
C R39--	Mirror G Room
C
39000	IF(PRSA.EQ.WALKIW) CALL JIGSUP(685)
	RETURN
C
C R40--	Mirror C Room
C
40000	IF(PRSA.EQ.LOOKW) CALL LOOKTO(MRG,MRB,683,0,681)
	RETURN
C
C R41--	Mirror B Room
C
41000	IF(PRSA.EQ.LOOKW) CALL LOOKTO(MRC,MRA,0,0,681)
	RETURN
C
C R42--	Mirror A Room
C
42000	IF(PRSA.EQ.LOOKW) CALL LOOKTO(MRB,0,0,684,681)
	RETURN

C RAPPLI, PAGE 12
C
C R43--	Mirror C East/West
C
43000	IF(PRSA.EQ.LOOKW) CALL EWTELL(HERE,683)
	RETURN
C
C R44--	Mirror B East/West
C
44000	IF(PRSA.EQ.LOOKW) CALL EWTELL(HERE,686)
	RETURN
C
C R45--	Mirror A East/West
C
45000	IF(PRSA.EQ.LOOKW) CALL EWTELL(HERE,687)
	RETURN
C
C R46--	Inside Mirror
C
46000	IF(PRSA.NE.LOOKW) RETURN		! look?
	CALL RSPEAK(688)			! describe
C
C Now describe pole state.
C
C Cases 1,2--	MDIR=270 & MLOC=MRB, pole is up or in hole
C Cases 3,4--	MDIR=0 V MDIR=180, pole is up or in channel
C Case 5--	Pole is up
C
	I=689					! assume case 5.
	IF((MDIR.EQ.270).AND.(MLOC.EQ.MRB))
	1	I=690+MIN0(POLEUF,1)		! cases 1,2.
	IF(MOD(MDIR,180).EQ.0)
	1	I=692+MIN0(POLEUF,1)		! cases 3,4.
	CALL RSPEAK(I)				! describe pole.
	CALL RSPSUB(694,695+(MDIR/45))		! describe arrow.
	RETURN

C RAPPLI, PAGE 13
C
C R47--	Mirror Eye Room
C
47000	IF(PRSA.NE.LOOKW) RETURN		! look?
	I=704					! assume beam stop.
	DO 47100 J=1,OLNT
	  IF(QHERE(J,HERE).AND.(J.NE.RBEAM)) GO TO 47200
47100	CONTINUE
	I=703
47200	CALL RSPSUB(I,ODESC2(J))		! describe beam.
	CALL LOOKTO(MRA,0,0,0,0)		! look north.
	RETURN
C
C R48--	Inside Crypt
C
48000	IF(PRSA.NE.LOOKW) RETURN		! look?
	I=46					! crypt is open/closed.
	IF(QOPEN(TOMB)) I=12
	CALL RSPSUB(705,I)
	RETURN
C
C R49--	South Corridor
C
49000	IF(PRSA.NE.LOOKW) RETURN		! look?
	CALL RSPEAK(706)			! describe.
	I=46					! odoor is open/closed.
	IF(QOPEN(ODOOR)) I=12
	IF(LCELL.EQ.4) CALL RSPSUB(707,I)	! describe odoor if there.
	RETURN
C
C R50--	Behind Door
C
50000	IF(PRSA.NE.WALKIW) GO TO 50100		! walk in?
	CFLAG(CEVFOL)=.TRUE.			! master follows.
	CTICK(CEVFOL)=-1
	RETURN
C
50100	IF(PRSA.NE.LOOKW) RETURN		! look?
	I=46					! qdoor is open/closed.
	IF(QOPEN(QDOOR)) I=12
	CALL RSPSUB(708,I)
	RETURN

C RAPPLI, PAGE 14
C
C R51--	Front Door
C
51000	IF(PRSA.EQ.WALKIW) CTICK(CEVFOL)=0	! if exits, kill follow.
	IF(PRSA.NE.LOOKW) RETURN		! look?
	CALL LOOKTO(0,MRD,709,0,0)		! describe south.
	I=46					! panel is open/closed.
	IF(CFLAG(CEVINQ).AND.(CTICK(CEVINQ).NE.0)) I=12	! open if inq in prog.
	J=46					! qdoor is open/closed.
	IF(QOPEN(QDOOR)) J=12
	CALL RSPSB2(710,I,J)
	RETURN
C
C R52--	North Corridor
C
52000	IF(PRSA.NE.LOOKW) RETURN		! look?
	I=46
	IF(QOPEN(CDOOR)) I=12			! cdoor is open/closed.
	CALL RSPSUB(711,I)
	RETURN
C
C R53--	Parapet
C
53000	IF(PRSA.EQ.LOOKW) CALL RSPSUB(712,712+PNUMB)
	RETURN
C
C R54--	Cell
C
54000	IF(PRSA.NE.LOOKW) RETURN		! look?
	I=721					! cdoor is open/closed.
	IF(QOPEN(CDOOR)) I=722
	CALL RSPEAK(I)
	I=46					! odoor is open/closed.
	IF(QOPEN(ODOOR)) I=12
	IF(LCELL.EQ.4) CALL RSPSUB(723,I)	! describe.
	RETURN
C
C R55--	Prison Cell
C
55000	IF(PRSA.EQ.LOOKW) CALL RSPEAK(724)	! look?
	RETURN
C
C R56--	Nirvana Cell
C
56000	IF(PRSA.NE.LOOKW) RETURN		! look?
	I=46					! odoor is open/closed.
	IF(QOPEN(ODOOR)) I=12
	CALL RSPSUB(725,I)
	RETURN

C RAPPLI, PAGE 15
C
C R57--	Nirvana and end of game
C
57000	IF(PRSA.NE.WALKIW) RETURN		! walkin?
	PAUSE '--More--'
	CALL RSPEAK(726)
	PAUSE '--More--'
	CALL SCORE(.FALSE.)
	CALL EXIT
C
C R58--	Tomb Room
C
58000	IF(PRSA.NE.LOOKW) RETURN		! look?
	I=46					! tomb is open/closed.
	IF(QOPEN(TOMB)) I=12
	CALL RSPSUB(792,I)
	RETURN
C
C R59--	Puzzle Side Room
C
59000	IF(PRSA.NE.LOOKW) RETURN		! look?
	I=861					! assume door closed.
	IF(CPOUTF) I=862			! open?
	CALL RSPEAK(I)				! describe.
	RETURN
C
C R60--	Puzzle Room
C
60000	IF(PRSA.NE.LOOKW) RETURN		! look?
	IF(CPUSHF) GO TO 60100			! started puzzle?
	CALL RSPEAK(868)			! no, describe.
	IF(IAND(OFLAG2(WARNI), TCHBT).NE.0) CALL RSPEAK(869)
	RETURN
C
60100	CALL CPINFO(880,CPHERE)			! describe room.
	RETURN

C RAPPLI, PAGE 16
C
C R61--	Palantir Room
C
61000	IF(PRSA.NE.LOOKW) GO TO 62400		! look?
	CALL RSPEAK(1015)
	I=699					! string is south.
	GO TO 62100				! join common code.
C
C R62--	Prm Room
C
62000	IF(PRSA.NE.LOOKW) GO TO 62400		! look?
	CALL RSPEAK(1016)			! string is north.
	I=695
C
62100	IF(PLOOKF) GO TO 62400			! thru window? skip door.
	CALL RSPSUB(1017,I)
	I=1018					! assume lid open.
	IF(.NOT.QOPEN(HERE-PRM+PLID1)) I=1019	! closed.
	CALL RSPEAK(I)				! describe lock.
	DO 62200 I=1,OLNT			! loop through objects.
	  IF(OCAN(I).NE.(HERE-PRM+PKH1)) GO TO 62200
	  CALL RSPSUB(1020,ODESC2(I))		! object in keyhole.
	  GO TO 62300
62200	CONTINUE
C
62300	IF(QOPEN(PDOOR)) CALL RSPEAK(1042)	! door open?
	IF(.NOT.MATF) GO TO 62400		! mat under door?
	CALL RSPEAK(1021)
	IF((MATOBJ.NE.0).AND.((HERE.EQ.PALRM).OR.QOPEN(PDOOR)))
	1	CALL RSPSUB(1022,ODESC2(MATOBJ))	! obj on mat?
	GO TO 62400
C
62400	PLOOKF=.FALSE.				! clear window flag.
	IF(PRSO.EQ.0) RETURN			! any object?
	IF((PRSA.NE.TAKEW).OR..NOT.QEMPTY(HERE-PRM+PKH1).OR.
	1  ((PRSO.NE.SCREW).AND.(PRSO.NE.STICK).AND.
	2   (PRSO.NE.PKEY).AND.(PRSO.NE.KEYS))) GO TO 62500
	IF(.NOT.PTOUCF) GO TO 62450		! touched?
	IF(QOPEN(HERE-PRM+PLID1)) CALL RSPEAK(1043)   	! lid closes.
	OFLAG2(HERE-PRM+PLID1)=IAND(OFLAG2(HERE-PRM+PLID1), NOT(OPENBT))
62450	PTOUCF=.TRUE.				! touched now.
C
62500	OFLAG1(SCREW)=IAND(OFLAG1(SCREW), NOT(NDSCBT))
	IF((OCAN(SCREW).EQ.PKH1).OR.(OCAN(SCREW).EQ.PKH2))
	1	OFLAG1(SCREW)=IOR(OFLAG1(SCREW), NDSCBT)
	OFLAG1(STICK)=IAND(OFLAG1(STICK), NOT(NDSCBT))
	IF((OCAN(STICK).EQ.PKH1).OR.(OCAN(STICK).EQ.PKH2))
	1	OFLAG1(STICK)=IOR(OFLAG1(STICK), NDSCBT)
	OFLAG1(PKEY)=IAND(OFLAG1(PKEY), NOT(NDSCBT))
	IF((OCAN(PKEY).EQ.PKH1).OR.(OCAN(PKEY).EQ.PKH2))
	1	OFLAG1(PKEY)=IOR(OFLAG1(PKEY), NDSCBT)
	OFLAG1(KEYS)=IAND(OFLAG1(KEYS), NOT(NDSCBT))
	IF((OCAN(KEYS).EQ.PKH1).OR.(OCAN(KEYS).EQ.PKH2))
	1	OFLAG1(KEYS)=IOR(OFLAG1(KEYS), NDSCBT)
	IF((OROOM(MAT).NE.PRM).AND.(OROOM(MAT).NE.PALRM)) MATF=.FALSE.
	OFLAG1(MAT)=IAND(OFLAG1(MAT), NOT(NDSCBT))
	IF(.NOT.MATF) RETURN
	OFLAG1(MAT)=IOR(OFLAG1(MAT), NDSCBT)
	CALL NEWSTA(MAT,0,HERE,0,0)
	RETURN

C RAPPLI, PAGE 17
C
C R63--	Inslide
C
63000	DO 63100 I=1,OLNT			! loop through objects
	  IF(.NOT.QHERE(I,HERE).OR.
	1	(IAND(OFLAG1(I), TAKEBT).EQ.0)) GO TO 63100
	  CALL NEWSTA(I,0,CELLA,0,0)		! drop to cellar,
	  IF(I.EQ.WATER) CALL NEWSTA(I,0,0,0,0)	! unless water
	  CALL RSPSUB(1011,ODESC2(I))
63100	CONTINUE
	RETURN
C
C R64--	Puzzle Anteroom
C
64000	IF(PRSA.NE.LOOKW) RETURN		! look?
	I=1068					! not blocked.
	IF(CPVEC(10).NE.0) I=1069		! blocked.
	CALL RSPEAK(I)				! describe.

	END SUBROUTINE RAPPLI

C LOOKTO--	Describe view in mirror hallway
C
C Declarations
C
      SUBROUTINE LOOKTO(NRM,SRM,NT,ST,HT)
      use state, only: here,mdir,mloc,mr1f,mr2f,mropnf
      use subr
      use io,only: rspeak,rspsub,rspsb2
      integer, intent(in) :: NRM,SRM,NT,ST,HT

      integer dir,i,m1,mrbf

	CALL RSPEAK(HT)				! describe hall.
	CALL RSPEAK(NT)				! describe north view.
	CALL RSPEAK(ST)				! describe south view.
	DIR=0					! assume no direction.
	IF(IABS(MLOC-HERE).NE.1) GO TO 200	! mirror to n or s?
	IF(MLOC.EQ.NRM) DIR=695
	IF(MLOC.EQ.SRM) DIR=699			! dir=n/s.
	IF(MOD(MDIR,180).NE.0) GO TO 100	! mirror n-s?
	CALL RSPSUB(847,DIR)			! yes, he sees panel
	CALL RSPSB2(848,DIR,DIR)		! and narrow rooms.
	GO TO 200
C
100	M1=MRHERE(HERE)				! which mirror?
	MRBF=0					! assume intact.
	IF(((M1.EQ.1).AND..NOT.MR1F).OR.
	1  ((M1.EQ.2).AND..NOT.MR2F)) MRBF=1	! broken?
	CALL RSPSUB(849+MRBF,DIR)		! describe.
	IF((M1.EQ.1).AND.MROPNF) CALL RSPEAK(823+MRBF)
	IF(MRBF.NE.0) CALL RSPEAK(851)
C
200	I=0					! assume no more to do.
	IF((NT.EQ.0).AND.((DIR.EQ.0).OR.(DIR.EQ.699))) I=852
	IF((ST.EQ.0).AND.((DIR.EQ.0).OR.(DIR.EQ.695))) I=853
	IF((NT+ST+DIR).EQ.0) I=854
	IF(HT.NE.0) CALL RSPEAK(I)		! describe halls.

	END SUBROUTINE LOOKTO

C EWTELL--	Describe e/w narrow rooms
C
      SUBROUTINE EWTELL(RM,ST)
      use state,only: mdir,mr1f,mr2f,mrae,mropnf
      use io,only: rspeak
	  integer,intent(in) :: rm,st
    
      integer i

      LOGICAL M1
C
C Note that we are east or west of mirror, and
C mirror must be n-s.
C
	M1=(MDIR+(MOD(RM-MRAE,2)*180)) == 180
	I=MOD(RM-MRAE,2)			! get basic e/w flag.
	IF((M1.AND..NOT.MR1F).OR.(.NOT.M1.AND..NOT.MR2F))
	1	I=I+2				! mirror broken?
	CALL RSPEAK(819+I)
	IF(M1.AND.MROPNF) CALL RSPEAK(823+(I/2))
	CALL RSPEAK(825)
	CALL RSPEAK(ST)

	END SUBROUTINE EWTELL
	
	      

      end module
