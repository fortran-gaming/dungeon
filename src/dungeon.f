C Main program for DUNGEON
C
C COPYRIGHT 1980, 1990, INFOCOM COMPUTERS AND COMMUNICATIONS, CAMBRIDGE MA.
C ALL RIGHTS RESERVED, COMMERCIAL USAGE STRICTLY PROHIBITED
C WRITTEN BY R. M. SUPNIK
C
C 02-Dec-15     EMG     Compile using gfortran
C 16-Aug-94	RMS	Version 3.2.
C 30-Jun-92	RMS	Changed file names to lower case.
C 29-Jun-92	RMS	Changed OUTCH to 6 for VAX, UNIX compatibility.
C
      PROGRAM DUNGEO
      use dparam
      IMPLICIT INTEGER (A-Z)

      LOGICAL, external :: INITFL

      integer argc
      character(10) argv

C Data statements for constant arrays

      VMAJ=4; VMIN=0; VEDIT='A'

      BATDRP=[66,67,68,69,70,71,72,65,73]

      SCOLDR=[XNORTH,BKTWI,XSOUTH,BKVAU,XEAST,BKVE,XWEST,BKVW]
      SCOLWL=[BKVW,271,XEAST,BKVE,272,XWEST,
     &         BKTWI,270,XSOUTH,BKVAU,269,XNORTH]

      CPDR=[XNORTH,-8,XNE,-7,XEAST,1,XSE,9,
     &       XSOUTH,8,XSW,7,XWEST,-1,XNW,-9]
      CPWL=[269,-8,270,8,271,1,272,-1]
      CPVEC=[1,1,1,1,1,1,1,1,
     &        1,0,-1,0,0,-1,0,1,
     &        1,-1,0,1,0,-2,0,1,
     &        1,0,0,0,0,1,0,1,
     &        1,-3,0,0,-1,-1,0,1,
     &        1,0,0,-1,0,0,0,1,
     &        1,1,1,0,0,0,1,1,
     &        1,1,1,1,1,1,1,1]

      XELNT=[1,2,3,3]

C DUNGEON, PAGE 2
C
C 1) Initialize data structures
C 2) Initialize files
C 3) Play game
C
	MLNT=0					! init array counters.
	OLNT=0					! array limits are parameters
	RLNT=0
	VLNT=0
	ALNT=0
	CLNT=0
	XLNT=1
	R2LNT=0
C
	LTSHFT=10				! set up state variables.
	MXSCOR=LTSHFT
	EGSCOR=0
	EGMXSC=0
	MXLOAD=100
	RWSCOR=0
	DEATHS=0
	MOVES=0
	PLTIME=0
	MUNGRM=0
	HS=0
	PRSA=0					! clear parse vector.
	PRSI=0
	PRSO=0
	PRSCON=1
	OFLAG=0					! clear orphans.
	OACT=0
	OPREP1=0
	OOBJ1=0
	OPREP=0
	ONAME=' '
	OPREP2=0
	OOBJ2=0
	BUNLNT=0				! clear bunch vector.
	BUNSUB=0
	DO 100 I=1,BUNMAX
	  BUNVEC(I)=0
100	CONTINUE
	THFFLG=.FALSE.				! thief not introduced but
	THFACT=.TRUE.				! is active.
	SWDACT=.FALSE.				! sword is inactive.
	SWDSTA=0				! sword is off.
C
	MBASE=0					! init melee base.
	DBCH=2			! data base.

C DUNGEON, PAGE 3
C
C Init all arrays.
C
	DO 200 I=1,CMAX				! clear clock events
	  CFLAG(I)=.FALSE.
	  CCNCEL(I)=.FALSE.
	  CTICK(I)=0
	  CACTIO(I)=0
200	CONTINUE
C
	DO 300 I=1,FMAX				! clear flags.
	  FLAGS(I)=.FALSE.
300	CONTINUE
	EGYPTF=.TRUE.				! some start as true.
	CAGETF=.TRUE.
	MR1F=.TRUE.
	MR2F=.TRUE.
	FOLLWF=.TRUE.
	DO 400 I=1,SMAX				! clear switches.
	  SWITCH(I)=0
400	CONTINUE
	ORMTCH=4				! number of matches.
	LCELL=1
	PNUMB=1
	MDIR=270
	MLOC=MRB
	CPHERE=10
C
	DO 500 I=1,R2MAX			! clear room 2 array.
	  R2(I)=0
	  O2(I)=0
500	CONTINUE
C
	DO 600 I=1,XXMAX			! clear travel array.
	  TRAVEL(I)=0
600	CONTINUE
C
	DO 700 I=1,VMAX				! clear villains arrays.
	  VOPPS(I)=0
	  VPROB(I)=0
	  VILLNS(I)=0
	  VBEST(I)=0
	  VMELEE(I)=0
700	CONTINUE

C DUNGEON, PAGE 4
C
	DO 800 I=1,OMAX				! clear object arrays.
	  ODESC1(I)=0
	  ODESC2(I)=0
	  ODESCO(I)=0
	  OREAD(I)=0
	  OACTIO(I)=0
	  OFLAG1(I)=0
	  OFLAG2(I)=0
	  OFVAL(I)=0
	  OTVAL(I)=0
	  OSIZE(I)=0
	  OCAPAC(I)=0
	  OCAN(I)=0
	  OADV(I)=0
	  OROOM(I)=0
800	CONTINUE
C
	RDESC2=0				! clear desc base ptr.
	DO 900 I=1,RMAX				! clear room arrays.
	  RDESC1(I)=0
	  RACTIO(I)=0
	  RFLAG(I)=0
	  RVAL(I)=0
	  REXIT(I)=0
900	CONTINUE
C
	DO 1000 I=1,MMAX			! clear message directory.
	  RTEXT(I)=0
1000	CONTINUE
C
	DO 1100 I=1,AMAX			! clear adventurer's arrays.
	  AROOM(I)=0
	  ASCORE(I)=0
	  AVEHIC(I)=0
	  AOBJ(I)=0
	  AACTIO(I)=0
	  ASTREN(I)=0
	  AFLAG(I)=0
1100	CONTINUE
C
	DBGFLG=0
	PRSFLG=0
	GDTFLG=1
C
	FROMDR=0				! init scol goodies.
	SCOLRM=0
	SCOLAC=0

        argc = command_argument_count()
        do i = 1,argc
            call get_command_argument(i,argv)
            if (argv=='-d') debug=.true.
            if (argv=='-c') then
                cheat=.true.
                open(ucheat,file='cheat.asc',
     &               status='old',action='read')
            endif
        enddo


	IF(INITFL(X)) CALL GAME			! if init files, play game.


	END

