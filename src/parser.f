C Parser for DUNGEON
C
C COPYRIGHT 1980, 1990, INFOCOM COMPUTERS AND COMMUNICATIONS, CAMBRIDGE MA.
C ALL RIGHTS RESERVED, COMMERCIAL USAGE STRICTLY PROHIBITED
C WRITTEN BY R. M. SUPNIK
C
C 02-Dec-15     EMG     Compile using gfortran
C 29-Sep-94	RMS	Fixed bugs in PLAY WITH, ALL BUT, GWIM, THISIT, IT.
C			Fixed vocabularly for ROCK, LIGHT, GATES, STACK,
C			BLIND.  Added COUNT, PERUSE, BLESSING, GHOSTS,
C			SPIRITS, CLIFFS, CORPSES, OUTPUT, CHIMNEY,
C			ZORKMID adjective, DIGBT flag.
C 30-Jan-94	RMS	Fixed bug in error message.
C 30-Jun-92	RMS	Changed file names to lower case.
C 29-Jun-92	RMS	Removed extraneous declaration from SPARSE.
C			Added dummy argument to SYNMCH.
C
C RDLINE-	Read input line
C
C Declarations
C
	SUBROUTINE RDLINE(INLINE,INLEN,WHO)
      use, intrinsic:: iso_fortran_env, only: input_unit,output_unit
	use dparam
	IMPLICIT INTEGER(A-Z)

	CHARACTER*(TEXLNT) INLINE
C
	LUCVT=ICHAR('A')-ICHAR('a')		! case conversion factor.
5	GO TO (90,10),WHO+1			! see who to prompt for.
10	WRITE(output_unit,'(A)',advance='no') ' >'	! prompt for game.
	flush(output_unit)

C
90	READ(input_unit,100,END=5) INLINE		! get input.
100	FORMAT(A)
C
	INLEN=NBLEN(INLINE)			! len w/o trailing blanks.
	IF(INLEN.LE.0) GO TO 5			! anything left?
	DO 400 I=1,INLEN			! convert to upper case.
	  IF((INLINE(I:I).GE.'a').AND.(INLINE(I:I).LE.'z'))
	1	INLINE(I:I)=CHAR(ICHAR(INLINE(I:I))+LUCVT)
400	CONTINUE
	PRSCON=1				! restart lex scan.

	END SUBROUTINE RDLINE

C PARSE-	Top level parse routine
C
C Declarations
C
C This routine details on bit 0 of PRSFLG
C
	LOGICAL FUNCTION PARSE(INLINE,INLEN,VBFLAG)
	use dparam
	IMPLICIT INTEGER(A-Z)
	CHARACTER*(TEXLNT) INLINE
	CHARACTER*(WRDLNT) OUTBUF(LEXMAX),BAKBUF(LEXMAX)
	LOGICAL LEX,SYNMCH,DFLAG,VBFLAG
	SAVE BAKBUF,BAKLEN
	DATA BAKBUF(1)/'L'/,BAKLEN/1/
C
	DFLAG=IAND(PRSFLG, 1).NE.0
	PARSE=.FALSE.				! assume fails.
	PRSA=0					! zero outputs.
	PRSI=0
	PRSO=0
C
	IF(.NOT.LEX(INLINE,INLEN,OUTBUF,OUTLEN,VBFLAG)) GO TO 1000
	IF((OUTLEN.NE.1).OR.(OUTBUF(1).NE.'AGAIN')) GO TO 100
	DO 50 I=1,LEXMAX			! use previous
	  OUTBUF(I)=BAKBUF(I)
50	CONTINUE
	OUTLEN=BAKLEN				! buffer and length.
100	IF(SPARSE(OUTBUF,OUTLEN,VBFLAG)) 1000,200,300	! do syn scan.
C
C Parse requires validation
C
200	IF(.NOT.VBFLAG) GO TO 350		! echo mode, force fail.
	IF(.NOT.SYNMCH(X)) GO TO 1000		! do syn match.
	IF(PRSO.EQ.BUNOBJ) LASTIT=BUNVEC(1)	! record for "it".
	IF((PRSO.GT.0).AND.(PRSO.LT.BUNOBJ)) LASTIT=PRSO
C
C Successful parse or successful validation
C
300	PARSE=.TRUE.
350	CALL ORPHAN(0,0,0,0,0,' ',0,0)		! clear orphans.
	DO 400 I=1,LEXMAX			! save command
	  BAKBUF(I)=OUTBUF(I)
400	CONTINUE
	BAKLEN=OUTLEN				! save length
	IF(DFLAG) WRITE(output_unit,500) PARSE,PRSA,PRSO,PRSI
500	FORMAT(' PARSE RESULTS- ',L7,3I7)
	RETURN
C
C Parse fails, disallow continuation
C
1000	PRSCON=1
	IF(DFLAG) WRITE(output_unit,500) PARSE,PRSA,PRSO,PRSI

	END FUNCTION PARSE

C LEX-	Lexical analyzer
C
C Declarations
C
C This routine details on bit 1 of PRSFLG
C
	LOGICAL FUNCTION LEX(INLINE,INLEN,OUTBUF,OP,VBFLAG)
	use dparam
	IMPLICIT INTEGER(A-Z)
	CHARACTER*(TEXLNT) INLINE
	CHARACTER*(WRDLNT) OUTBUF(LEXMAX)
	CHARACTER*1 J
	LOGICAL DFLAG,VBFLAG
C
	DFLAG=IAND(PRSFLG, 2).NE.0
	LEX=.FALSE.				! assume lex fails.
	OP=0					! output ptr.
	DO 10 I=1,LEXMAX			! clear output buf.
	  OUTBUF(I)=' '
10	CONTINUE
C
50	OP=OP+1					! adv output ptr.
	CP=0					! char ptr=0.
C
200	IF(PRSCON.GT.INLEN) GO TO 2000		! end of input?
	J=INLINE(PRSCON:PRSCON)			! no, get character,
	IF((J.EQ.'"').OR.(J.EQ.'''')) GO TO 3000! substring?
	PRSCON=PRSCON+1				! advance ptr.
	IF(J.EQ.' ') GO TO 1000			! space?
	IF((J.EQ.'.').OR.(J.EQ.';').OR.
	1  (J.EQ.'!').or.(J.EQ.'?')) GO TO 2000	! end of command?
	IF(J.EQ.',') GO TO 4000			! comma?
	IF(OP.GT.LEXMAX) GO TO 5000		! too many tokens?
	CP=CP+1					! adv char ptr.
	IF(CP.LE.WRDLNT) OUTBUF(OP)(CP:CP)=J	! insert char in word.
	GO TO 200
C
C Space.
C
1000	IF(CP.EQ.0) GO TO 200			! any word yet?
	GO TO 50				! yes, adv op.
C
C End of input, see if partial word available.
C
2000	IF(PRSCON.GT.INLEN) PRSCON=1		! force parse restart.
	IF((CP.EQ.0).AND.(OP.EQ.1)) RETURN	! any results?
	IF(CP.EQ.0) OP=OP-1			! any last word?
	LEX=.TRUE.
	IF(DFLAG) WRITE(output_unit,2020) CP,OP,PRSCON,(OUTBUF(I),I=1,OP)
2020	FORMAT(' LEX RESULTS- ',3I7/1X,8(A,1X))
	RETURN
C
C Substring, J is delimiter.
C
3000	IF(SUBLNT.NE.0) GO TO 3400		! already got one?
3100	PRSCON=PRSCON+1				! skip initial quote.
	IF(PRSCON.GT.INLEN) GO TO 3500		! any more characters?
	IF(INLINE(PRSCON:PRSCON).EQ.' ') GO TO 3100	! skip blanks.
	K=INDEX(INLINE(PRSCON:INLEN),J)		! find closing quote.
	IF(K.LE.1) GO TO 3500			! none or empty?
	SUBBUF=INLINE(PRSCON:PRSCON+K-2)	! set up substring buffer,
	SUBLNT=K-1				! length.
	PRSCON=PRSCON+K				! skip over string.
	IF(DFLAG) WRITE(output_unit,3030) SUBLNT,SUBBUF(1:SUBLNT)
3030	FORMAT(' SUBSTRING- ',I7,' "',A,'"')
	GO TO 1000				! treat as end of word.
C
3400	IF(VBFLAG) CALL RSPEAK(1046)		! multiple substrings.
	RETURN
C
3500	IF(VBFLAG) CALL RSPEAK(616)		! bad substring.
	RETURN					! fails.
C
C Comma.
C
4000	IF(CP.NE.0) OP=OP+1			! if partial word, go to next.
	IF(OP.EQ.1) GO TO 4500			! no first word? die.
	IF(OP.GT.LEXMAX) GO TO 5000		! too many tokens?
	OUTBUF(OP)='AND'			! insert 'AND'.
	GO TO 50				! start new word
C
4500	IF(VBFLAG) CALL RSPEAK(1047)		! misplaced comma.
	RETURN
C
C Too many tokens.
C
5000	IF(VBFLAG) CALL RSPEAK(1048)		! too many tokens.

	END FUNCTION LEX

C SPARSE-	Start of parse
C
C Declarations
C
C This routine details on bit 2 of PRSFLG
C
	INTEGER FUNCTION SPARSE(LBUF,LLNT,VBFLAG)
	use dparam
	IMPLICIT INTEGER(A-Z)
	CHARACTER*(WRDLNT) LBUF(LEXMAX),WORD,LCWORD,LCIFY
	CHARACTER*(WRDLNT+2) LCWRD1
	LOGICAL LIT,DFLAG,VBFLAG,ANDFLG,BUNFLG


C SPARSE, PAGE 7
C
C Set up for parsing
C
	SPARSE=-1				! assume parse fails.
	ADJ=0					! clear parts holders.
	ACT=0
	PREP=0
	PPTR=0
	OBJ1=0
	OBJ2=0
	PREP1=0
	PREP2=0
	LOBJ=0
	ANDFLG=.FALSE.
	BUNFLG=.FALSE.
	DFLAG=IAND(PRSFLG,4).NE.0

C SPARSE, PAGE 8
C
C Now loop over input buffer of lexical tokens.
C
	I=0
10	I=I+1					! do 1000 i=1,llnt
	  WORD=LBUF(I)				! get current token.
	  ERRVOC=0				! assume won't find
	  IF(WORD.EQ.' ') GO TO 1000		! blank? ignore.
	  IF(WORD.EQ.'AND') GO TO 1500		! 'AND'?
	  IF((WORD.EQ.'EXCEPT').OR.(WORD.EQ.'BUT')) GO TO 2500
C
C Check for buzz word
C
	  DO 50 J=1,BWMAX
	    IF(WORD.EQ.BWORD(J)) GO TO 1000	! if match, ignore.
50	  CONTINUE
C
C Check for action or direction
C
	  J=1					! check for action.
	  DO 70 K=1,VWMAX
	    IF(VWORD(K)(1:1).EQ.'*') GO TO 65	! synonym?
	    IF(WORD.EQ.VWORD(K)) GO TO 2000	! match to base word?
	    J=J+VVOC(J)+1			! skip over syntax.
	    GO TO 70
65	    IF(WORD.EQ.VWORD(K)(2:WRDLNT)) GO TO 2000 ! synonym match?
70	  CONTINUE
C
75	  IF((ADJ.NE.0).OR.(PREP.NE.0).OR.(OBJ1.NE.0)) GO TO 200
	  IF(ACT.EQ.0) GO TO 80			! any verb yet?
	  IF(IAND(VVOC(ACT+1), SVMASK).NE.WALKW) GO TO 200	! walk?
80	  DO 100 J=1,DWMAX			! then chk for dir.
	    IF(WORD.EQ.DWORD(J)) GO TO 3000	! match to direction?
100	  CONTINUE
C
C Not an action, check for preposition, adjective, or object.
C
200	  DO 250 J=1,PWMAX			! look for preposition.
	    IF(WORD.EQ.PWORD(J)) GO TO 4000	! match to preposition?
250	  CONTINUE
C
	  J=1					! look for adjective.
	  DO 350 K=1,AWMAX
	    IF(WORD.EQ.AWORD(K)) GO TO 5000	! match to adjective?
300	    J=J+1				! advance to next entry.
	    IF(AVOC(J).LT.0) GO TO 300		! found next entry yet?
350	  CONTINUE
C
400	  J=1					! look for object.
	  DO 550 K=1,OWMAX
	    IF(WORD.EQ.OWORD(K)) GO TO 6000	! match to object?
500	    J=J+1				! advance to next entry.
	    IF(OVOC(J).LT.0) GO TO 500		! found next entry yet?
550	  CONTINUE
C
C Not recognizable
C
	  IF(.NOT.VBFLAG) RETURN		! if mute, return
	  LCWORD=LCIFY(WORD,1)			! convert to lower case
	  WRITE(output_unit,600) LCWORD(1:NBLEN(LCWORD)) ! don't recognize
600	  FORMAT(' I don''t understand "',A,'".')
	  CALL RSPEAK(ERRVOC)			! if extra verb, say so
800	  TELFLG=.TRUE.				! something said.
	  BUNSUB=0				! no valid EXCEPT clause.
	  RETURN

C SPARSE, PAGE 9
C
1000	IF(I.LT.LLNT) GO TO 10			! end of do loop
C
C At end of parse, check for:
C	1. dangling adjective
C	2. bunched object
C	3. simple directions
C	4. orphan preposition
C	5. dangling preposition
C
	IF(ADJ.NE.0) GO TO 4500			! dangling adjective?
	IF(BUNFLG) OBJ1=BUNOBJ			! bunched object?
	IF(BUNFLG.AND.(BUNSUB.NE.0).AND.(BUNLNT.EQ.0))
	1	GO TO 13200			! except for nothing?
	IF(ACT.EQ.0) ACT=IAND(OFLAG, OACT)	! if no action, take orphan.
	IF(ACT.EQ.0) GO TO 10000		! no action, punt.
	IF((IAND(VVOC(ACT+1), SVMASK).NE.WALKW).OR.(OBJ1.LT.XMIN))
	1	GO TO 1100			! simple direction?
	IF ((OBJ2.NE.0).OR.(PREP1.NE.0).OR.(PREP2.NE.0))
	1	GO TO 1050			! no extra junk?
	PRSA=WALKW				! yes, win totally.
	PRSO=OBJ1
	SPARSE=1				! special return value.
	RETURN
C
1050	IF(VBFLAG) CALL RSPEAK(618)		! direction+junk, fail.
	GO TO 800				! clean up state.
C
1100	IF((OFLAG.NE.0).AND.(OPREP.NE.0).AND.(PREP.EQ.0).AND.
	1	(OBJ1.NE.0).AND.(OBJ2.EQ.0).AND.(ACT.EQ.OACT))
	2	GO TO 11000
C
	IF(PREP.EQ.0) GO TO 1200		! if dangling prep,
	IF(PPTR.EQ.0) GO TO 12000		! and no object, die;
	IF(PRPVEC(PPTR).NE.0) GO TO 12000	! and prep already, die;
	PRPVEC(PPTR)=PREP			! cvt to 'pick up frob'.
1200	SPARSE=0				! parse succeeds.
	IF(DFLAG) WRITE(output_unit,1310) ACT,OBJ1,OBJ2,PREP1,PREP2
1310	FORMAT(' SPARSE RESULTS- ',5I7)
	RETURN

C SPARSE, PAGE 10
C
C 1500--	AND
C
1500	IF(ADJ.NE.0) GO TO 4100			! dangling adj? treat as obj.
	IF((PREP.NE.0).OR.(PPTR.NE.1)) GO TO 8000	! prep or not dir obj?
	ANDFLG=.TRUE.				! flag 'AND'.
	GO TO 1000				! done.
C
C 2000--	Action
C
2000	IF(ACT.EQ.0) GO TO 2100			! got one already?
	ERRVOC=624				! flag for error report.
	GO TO 75				! try to construe differently.
C
2100	ACT=J					! save index to verb.
	OACT=0					! no orphan.
	ANDFLG=.FALSE.				! clear 'AND' flag.
	IF(DFLAG) WRITE(output_unit,2020) J
2020	FORMAT(' SPARSE- ACT AT ',I6)
	GO TO 1000				! done.
C
C 2500--	EXCEPT/BUT
C
2500	IF(ADJ.NE.0) GO TO 4100			! dangling adjective?
	IF(ANDFLG.OR.BUNFLG.OR.(PPTR.NE.1).OR.
	1	(I.GE.LLNT)) GO TO 13000	! not in right place?
	IF(LBUF(I+1).NE.'FOR') GO TO 2600	! except for?
	I=I+1					! skip over.
	IF(I.GE.LLNT) GO TO 13000		! out of text?
2600	IF((OBJ1.NE.EVERY).AND.(OBJ1.NE.VALUA).AND.
	1  (OBJ1.NE.POSSE)) GO TO 13100		! "collective" EXCEPT?
	ANDFLG=.TRUE.				! force next object
	BUNFLG=.TRUE.				! into bunch vector.
	BUNLNT=0				! start at top.
	BUNSUB=OBJ1				! remember collective.
	GO TO 1000				! on to next word.
C
C 3000--	Direction
C 		Don't need to check for ambiguous use as adjective;
C		only possible overlap is north/south/east/west wall;
C		and global wall takes is found if no adjective given.
C
3000	OBJ=DVOC(J)				! save direction.
	ACT=1					! find value for action.
3600	IF(VVOC(ACT).EQ.0) CALL BUG(310,ACT)	! can't find walk.
	IF(IAND(VVOC(ACT+1), SVMASK).EQ.WALKW) GO TO 6300 ! treat as obj.
	ACT=ACT+VVOC(ACT)+1			! to next syntax entry.
	GO TO 3600
C
C 4000--	Preposition (or dangling adjective at end of parse)
C
4000	IF(ADJ.EQ.0) GO TO 4600			! dangling adjective?
4100	I=I-1					! back up parse stream.
4500	WORD=AWORD(ADJPTR)			! get adjective string.
	ADJ=0					! now an object.
	GO TO 400				! go search object words.
C
4600	IF(ANDFLG) GO TO 8000			! 'AND' pending?
	IF(PREP.NE.0) GO TO 1000		! already have one? ignore.
	PREP=PVOC(J)				! no, get index.
	IF(DFLAG) WRITE(output_unit,4030) J
4030	FORMAT(' SPARSE- PREP AT ',I6)
	GO TO 1000
C
C 5000--	Adjective
C
5000	ADJ=J					! save adjective.
	ADJPTR=K				! save string pointer.
	IF((I.LT.LLNT).OR.(OFLAG.EQ.0).OR.(ONAME.EQ.' '))
	1	GO TO 1000			! last word + orphan string?
	IF(DFLAG) WRITE(output_unit,5040) ADJ,ONAME	! have orphan.
5040	FORMAT(' SPARSE- ADJ AT ',I6,' ORPHAN= ',A)
	WORD=ONAME				! get object string.
	GO TO 400				! go search object names.
C
C 6000--	Object
C
6000	OBJ=GETOBJ(J,ADJ,0)			! identify object.
	IF(DFLAG) WRITE(output_unit,6010) J,OBJ
6010	FORMAT(' SPARSE- OBJ AT ',I6,'  OBJ= ',I6)
	IF(OBJ.LE.0) GO TO 7000			! if le, couldnt.
	IF(OBJ.NE.ITOBJ) GO TO 6100		! "it"?
	IF(IAND(OFLAG, OOBJ1).NE.0) LASTIT=IAND(OFLAG, OOBJ1)	! orphan?
	OBJ=GETOBJ(0,0,LASTIT)			! find it.
	IF(OBJ.LE.0) GO TO 7500			! if le, couldnt.
C
6100	IF(PREP.NE.9) GO TO 6200		! "of" obj?
	IF((LOBJ.EQ.OBJ).OR.(LOBJ.EQ.OCAN(OBJ))) GO TO 6500	! same as prev?
	IF((LOBJ.EQ.EVERY).AND.((OBJ.EQ.VALUA).OR.(OBJ.EQ.POSSE)))
	1	GO TO 6350			! all of "collective"?
6150	IF(VBFLAG) CALL RSPEAK(601)		! doesn't work
	GO TO 800				! clean up state.
C
6200	IF(.NOT.ANDFLG) GO TO 6300		! 'AND' pending?
	IF(BUNFLG) GO TO 6250			! first object?
	BUNVEC(1)=OBJVEC(PPTR)			! put preceding obj in vector.
	BUNLNT=1
	BUNFLG=.TRUE.				! flag bunch of objects.
	BUNSUB=0				! no EXCEPT/BUT clause.
6250	BUNLNT=BUNLNT+1				! advance bunch pointer.
	IF(BUNLNT.GT.BUNMAX) GO TO 9000		! too many objects?
	BUNVEC(BUNLNT)=OBJ			! add to bunch vector.
	GO TO 6500
C
6300	IF(PPTR.EQ.2) GO TO 9000		! too many objs?
	PPTR=PPTR+1
	PRPVEC(PPTR)=PREP
6350	OBJVEC(PPTR)=OBJ			! stuff into vector.
6500	PREP=0
	ADJ=0
	ANDFLG=.FALSE.				! no pending 'AND'.
	LOBJ=OBJ				! record last object.
	GO TO 1000

C SPARSE, PAGE 11
C
C 7000--	Unidentifiable object (index into OVOC is J)
C
7000	LCWORD=LCIFY(WORD,1)			! convert obj to lower case.
	LCWRD1=' '				! assume no adjective
	IF(ADJ.NE.0) LCWRD1=' '//LCIFY(AWORD(ADJPTR),1)//' '
	IF(OBJ.LT.0) GO TO 7200			! ambiguous or unreachable?
	IF(LIT(HERE)) GO TO 7100		! lit?
	IF(VBFLAG) CALL RSPEAK(579)		! not lit, report.
	GO TO 800				! go clean up state.
C
7100	IF(VBFLAG) WRITE(output_unit,7110)
	1	LCWRD1(1:NBLEN(LCWRD1)+1),LCWORD(1:NBLEN(LCWORD))
7110	FORMAT(' I can''t see any',A,A,' here.')
	GO TO 800				! go clean up state.
C
7200	IF(OBJ.NE.-10000) GO TO 7300		! inside vehicle?
	IF(VBFLAG) CALL RSPSUB(620,ODESC2(AVEHIC(WINNER)))
	GO TO 800				! go clean up state.
C
7300	IF(ACT.EQ.0) ACT=IAND(OFLAG, OACT)		! if no act, get orphan.
	CALL ORPHAN(-1,ACT,PREP1,OBJ1,PREP,WORD,0,0)	! orphan the world.
	IF(VBFLAG) WRITE(output_unit,7310)
	1	LCWRD1(1:NBLEN(LCWRD1)+1),LCWORD(1:NBLEN(LCWORD))
7310	FORMAT(' Which',A,A,' do you mean?')
	GO TO 800				! go clean up state.
C
C 7500--	Unidentifiable 'IT' (last direct object is LASTIT).
C
7500	IF(OBJ.LT.0) GO TO 7200			! if lt, must be unreachable.
	IF(LIT(HERE)) GO TO 7600		! lit?
	IF(VBFLAG) CALL RSPEAK(1076)		! lose.
	GO TO 800				! go clean up state.
C
7600	IF(VBFLAG) CALL RSPSUB(1077,ODESC2(LASTIT))	! don't see it.
	GO TO 800				! go clean up state.
C
C 8000--	Misplaced 'AND'.
C
8000	IF(VBFLAG) CALL RSPEAK(1049)
	GO TO 800				! go clean up state.
C
C 9000--	Too many objects.
C
9000	IF(VBFLAG) CALL RSPEAK(617)
	GO TO 800				! go clean up state.
C
C 10000--	No action, punt.
C
10000	IF(OBJ1.EQ.0) GO TO 10100		! any direct object?
	IF(VBFLAG) CALL RSPSUB(621,ODESC2(OBJ1))	! what to do?
	CALL ORPHAN(-1,0,PREP1,OBJ1,0,' ',0,0)
	RETURN
C
10100	IF(VBFLAG) CALL RSPEAK(622)		! huh?
	GO TO 800				! go clean up state.
C
C 11000--	Orphan preposition.  Conditions are
C		OBJ1.NE.0, OBJ2=0, PREP=0, ACT=OACT
C
11000	IF(OOBJ1.NE.0) GO TO 11500		! orphan object?
	PREP1=OPREP				! no, just use prep.
	GO TO 1200
C
11500	OBJ2=OBJ1				! yes, use as direct obj.
	PREP2=OPREP
	OBJ1=OOBJ1
	PREP1=OPREP1
	GO TO 1200
C
C 12000--	True hanging preposition, no objects yet.
C
12000	CALL ORPHAN(-1,ACT,0,0,PREP,' ',0,0)	! orphan prep.
	GO TO 1200
C
C 13000--	EXCEPT/BUT errors.
C
13000	LCWORD=LCIFY(WORD,1)
	IF(VBFLAG) WRITE(output_unit,13010) LCWORD(1:NBLEN(LCWORD))	! wrong place.
13010	FORMAT(' Misplaced "',A,'".')
	GO TO 800				! go clean up state.
C
13100	LCWORD=LCIFY(WORD,2)				! wrong case.
	IF(VBFLAG) WRITE(output_unit,13110) LCWORD(1:NBLEN(LCWORD))	! not coll.
13110	FORMAT(' "',A,'" can only be used with "everything",',
	1 ' "valuables", or "possessions".')
	GO TO 800				! go clean up state.
C
13200	IF(VBFLAG) CALL RSPEAK(619)		! no objects.
	GO TO 800				! go clean up state.

	END FUNCTION SPARSE

C GETOBJ--	Find obj described by adj, name pair
C
C Declarations
C
C This routine details on bit 3 of PRSFLG
C
	INTEGER FUNCTION GETOBJ(OIDX,AIDX,SPCOBJ)
	use dparam
	IMPLICIT INTEGER(A-Z)
	LOGICAL THISIT,GHERE,LIT,CHOMP,DFLAG,NOADJS
C
	DFLAG=IAND(PRSFLG, 8).NE.0
	CHOMP=.FALSE.
	AV=AVEHIC(WINNER)
	OBJ=0					! assume dark.
	IF(.NOT.LIT(HERE)) GO TO 200		! lit?
C
	OBJ=SCHLST(OIDX,AIDX,HERE,0,0,SPCOBJ)	! search room.
	IF(DFLAG) WRITE(output_unit,10) OBJ
10	FORMAT(' SCHLST- ROOM SCH ',I6)
	IF(OBJ) 1000,200,100			! test result.
100	IF((AV.EQ.0).OR.(AV.EQ.OBJ).OR.(OCAN(OBJ).EQ.AV).OR.
	1	(IAND(OFLAG2(OBJ), FINDBT).NE.0)) GO TO 200
	CHOMP=.TRUE.				! not reachable.
C
200	IF(AV.EQ.0) GO TO 400			! in vehicle?
	NOBJ=SCHLST(OIDX,AIDX,0,AV,0,SPCOBJ)	! search vehicle.
	IF(DFLAG) WRITE(output_unit,220) NOBJ
220	FORMAT(' SCHLST- VEH SCH  ',I6)
	IF(NOBJ) 800,400,300			! test result.
300	CHOMP=.FALSE.				! reachable.
	IF(OBJ.EQ.NOBJ) GO TO 400		! same as before?
	IF(OBJ.NE.0) NOBJ=-NOBJ			! amb result?
	OBJ=NOBJ
C
400	NOBJ=SCHLST(OIDX,AIDX,0,0,WINNER,SPCOBJ)	! search adventurer.
	IF(DFLAG) WRITE(output_unit,430) NOBJ
430	FORMAT(' SCHLST- ADV SCH  ',I6)
	IF(NOBJ) 800,900,500			! test result
500	IF(OBJ.EQ.0) GO TO 800			! any previous? no, use nobj.
	IF(AIDX.NE.0) GO TO 600			! yes, amb, any adj?
	IF(NOADJS(OBJ).NEQV.NOADJS(NOBJ)) GO TO 700 ! both adj or no adj?
600	OBJ=-NOBJ				! ambiguous result.
	GO TO 900
700	IF(NOADJS(OBJ)) GO TO 900		! if old no adj, use old.
800	OBJ=NOBJ				! return new object.
900	IF(CHOMP) OBJ=-10000			! unreachable.
1000	GETOBJ=OBJ
C
	IF(GETOBJ.NE.0) GO TO 1500		! got something?
	DO 1200 I=STRBIT+1,OLNT			! no, search globals.
	  IF(.NOT.THISIT(OIDX,AIDX,I,SPCOBJ)) GO TO 1200
	  IF(.NOT.GHERE(I,HERE)) GO TO 1200	! can it be here?
	  IF(GETOBJ.EQ.0) GO TO 1150		! got one yet?
	  IF(AIDX.NE.0) GO TO 1050		! yes, no adj?
	  IF(NOADJS(GETOBJ).NEQV.NOADJS(I)) GO TO 1100	! only one with no adj?
1050	  GETOBJ=-I				! ambiguous
	  GO TO 1200
1100	  IF(NOADJS(GETOBJ)) GO TO 1200		! if old no adj, retain.
1150	  GETOBJ=I				! new is target.
1200	CONTINUE
C
1500	CONTINUE				! end of search.
	IF(DFLAG) WRITE(output_unit,1540) GETOBJ
1540	FORMAT(' SCHLST- RESULT   ',I6)
	RETURN
	END

C SCHLST--	Search for object
C
C Declarations
C
	INTEGER FUNCTION SCHLST(OIDX,AIDX,RM,CN,AD,SPCOBJ)
	use dparam
	IMPLICIT INTEGER(A-Z)

	LOGICAL THISIT,QHERE,NOTRAN,NOVIS,AEMPTY,NOADJS
C
C Functions and data
C
	NOTRAN(O)=(IAND(OFLAG1(O),TRANBT).EQ.0).AND.
	1	(IAND(OFLAG2(O), OPENBT).EQ.0)
	NOVIS(O)=(IAND(OFLAG1(O), VISIBT).EQ.0)
C
	SCHLST=0				! no result.
	AEMPTY=.FALSE.				! no ambiguous empty.
	DO 1000 I=1,OLNT			! search objects.
	  IF(NOVIS(I).OR.
	1	(((RM.EQ.0).OR.(.NOT.QHERE(I,RM))).AND.
	2	 ((CN.EQ.0).OR.(OCAN(I).NE.CN)).AND.
	3	 ((AD.EQ.0).OR.(OADV(I).NE.AD)))) GO TO 1000
	  IF(.NOT.THISIT(OIDX,AIDX,I,SPCOBJ)) GO TO 200
	  IF(SCHLST.EQ.0) GO TO 150		! got one already?
	  IF(AIDX.NE.0) GO TO 2000		! adj? then ambiguous
	  IF(NOADJS(I)) GO TO 100		! new have no adj?
	  AEMPTY=.TRUE.				! no, old might, flag.
	  GO TO 200
100	  IF(NOADJS(SCHLST)) GO TO 2000		! old have no adj?
150	  SCHLST=I				! new is unique, or
						! new has no adj, old does.
C
C If open or transparent, search the object itself.
C
200	  IF(NOTRAN(I)) GO TO 1000
C
C Search is conducted in reverse.  All objects are checked to
C See if they are at some level of containment inside object 'I'.
C If they are at level 1, or if all links in the containment
C chain are open, visible, and have SEARCHME set, they can qualify
C as a potential match.
C
	  DO 500 J=1,OLNT			! search objects.
	    IF(NOVIS(J).OR. (.NOT.THISIT(OIDX,AIDX,J,SPCOBJ)))
	1	GO TO 500			! visible & match?
	    X=OCAN(J)				! get container.
300	    IF(X.EQ.I) GO TO 400		! inside target?
	    IF(X.EQ.0) GO TO 500		! inside anything?
	    IF(NOVIS(X).OR.NOTRAN(X).OR.
	1	(IAND(OFLAG2(X), SCHBT).EQ.0)) GO TO 500
	    X=OCAN(X)				! go another level.
	    GO TO 300
C
400	    IF(SCHLST.EQ.0) GO TO 450		! already got one?
	    IF(AIDX.NE.0) GO TO 2000		! adj? then ambiguous.
	    IF(NOADJS(J)) GO TO 425		! new have no adj?
	    AEMPTY=.TRUE.			! no, ambiguous empty.
	    GO TO 500
425	    IF(NOADJS(SCHLST)) GO TO 2000	! old have no adj? then amb.
450	    SCHLST=J				! new is unique, or
						! new has no adj, and old does.
500	  CONTINUE
C
1000	CONTINUE
	IF(.NOT.AEMPTY.OR.(SCHLST.EQ.0)) RETURN	! if none or not amb, done.
	IF(NOADJS(SCHLST)) RETURN		! if amb, and no adj, done.
2000	SCHLST=-SCHLST				! amb return.
	RETURN
C
	END

C THISIT--	Validate object vs description
C
C Declarations
C
	LOGICAL FUNCTION THISIT(OIDX,AIDX,OBJ,SPCOBJ)
	use dparam
	IMPLICIT INTEGER(A-Z)


	THISIT=.FALSE.				! assume no match.
	IF((SPCOBJ.NE.0).AND.(OBJ.EQ.SPCOBJ)) GO TO 500
C
C Check for object names
C
	IF(OIDX.EQ.0) RETURN			! no obj? lose.
	I=OIDX
100	IF(IABS(OVOC(I)).EQ.OBJ) GO TO 200	! found it?
	I=I+1					! adv to next.
	IF(OVOC(I).LT.0) GO TO 100		! still part of list?
	RETURN					! if done, lose.
C
200	IF(AIDX.EQ.0) GO TO 500			! no adj? done.
	I=AIDX
300	IF(IABS(AVOC(I)).EQ.OBJ) GO TO 500	! found it?
	I=I+1					! adv to next.
	IF(AVOC(I).LT.0) GO TO 300		! still part of list?
	RETURN					! if done, lose.
C
500	THISIT=.TRUE.
	RETURN
	END

C SYNMCH--	Syntax matcher
C
C Declarations
C
C This routine details on bit 4 of PRSFLG
C
	LOGICAL FUNCTION SYNMCH(X)
	use dparam
	IMPLICIT INTEGER(A-Z)
	LOGICAL SYNEQL,TAKEIT,DFLAG
	CHARACTER*(TEXLNT) STR
	CHARACTER*(WRDLNT) FINDVB,FINDPR,LCIFY,LCWORD
	CHARACTER*(WRDLNT+2) LCPRP1,LCPRP2

C SYNMCH, PAGE 2
C
	SYNMCH=.FALSE.
	DFLAG=IAND(PRSFLG, 16).NE.0
	J=ACT					! set up ptr to syntax.
	DRIVE=0					! no default.
	DFORCE=0				! no forced default.
	QPREP=IAND(OFLAG, OPREP)		! valid orphan prep flag.
	LIMIT=J+VVOC(J)+1			! compute limit.
	J=J+1					! advance to next.
C
200	CALL UNPACK(J,NEWJ)			! unpack syntax.
	IF(DFLAG) WRITE(output_unit,210) J,OBJ1,PREP1,DOBJ,DFL1,DFL2
210	FORMAT(' SYNMCH DOBJ INPUTS TO SYNEQL- ',6I7)
	SPREP=IAND(DOBJ, VPMASK)		! save expected prep.
	IF(SYNEQL(PREP1,OBJ1,DOBJ,DFL1,DFL2)) GO TO 1000
C
C Direct syntax match fails, try direct as indirect.
C
	IF((OBJ2.NE.0).OR.(OBJ1.EQ.0).OR.
	1	(.NOT.SYNEQL(PREP1,OBJ1,IOBJ,IFL1,IFL2)))
	2	GO TO 500			! try direct as indirect.
	OBJ2=OBJ1				! move direct to indirect.
	PREP2=PREP1
	OBJ1=0					! no direct.
	PREP1=0
	DRIVE=J					! save as driver.
	GO TO 3100				! go try to get direct obj
C
C Direct syntax match and direct-as-indirect fail.
C
500	IF(OBJ1.NE.0) GO TO 3000		! if direct obj, on to next.
	GO TO 2500				! go do defaults.
C
C Direct syntax match succeeded, try indirect.
C
1000	IF(DFLAG) WRITE(output_unit,1010) J,OBJ2,PREP2,IOBJ,IFL1,IFL2
1010	FORMAT(' SYNMCH IOBJ INPUTS TO SYNEQL- ',6I7)
	SPREP=IAND(IOBJ, VPMASK)		! save expected prep.
	IF(SYNEQL(PREP2,OBJ2,IOBJ,IFL1,IFL2)) GO TO 6000
C
C Indirect syntax match fails.
C
	IF(OBJ2.NE.0) GO TO 3000		! if ind object, on to next.
2500	IF((QPREP.EQ.0).OR.(QPREP.EQ.SPREP)) DFORCE=J	 ! if prep mch.
	IF(IAND(VFLAG, SDRIV).NE.0) DRIVE=J	! if driver, record.
	IF(DFLAG) WRITE(output_unit,2510) J,QPREP,SPREP,DFORCE,DRIVE
2510	FORMAT(' SYNMCH DEFAULT SYNTAXES- ',5I7)
3000	J=NEWJ
	IF(J.LT.LIMIT) GO TO 200		! more to do?

C SYNMCH, PAGE 3
C
C Match has failed.  If default syntax exists, try to snarf
C orphans or GWIMs, or make new orphans.
C
3100	IF(DFLAG) WRITE(output_unit,3110) DRIVE,DFORCE,OBJ1,OBJ2
3110	FORMAT(' SYNMCH, DRIVE=',2I6,'  OBJECTS =',2I6)
	IF(DRIVE.EQ.0) DRIVE=DFORCE		! no driver? use force.
	IF(DRIVE.EQ.0) GO TO 10000		! any driver?
	CALL UNPACK(DRIVE,DFORCE)		! unpack dflt syntax.
	LCWORD=LCIFY(FINDVB(DRIVE),2)		! get verb string.
	LCPRP1=' '//LCIFY(FINDPR(IAND(DOBJ, VPMASK)),1)//' '
	LCPRP2=' '//LCIFY(FINDPR(IAND(IOBJ, VPMASK)),1)//' '
C
C Try to fill direct object slot if that was the problem.
C
	IF((IAND(VFLAG, SDIR).EQ.0).OR.(OBJ1.NE.0)) GO TO 4000
	OBJ1=IAND(OFLAG, OOBJ1)
	IF(OBJ1.EQ.0) GO TO 3500		! any orphan?
	IF(SYNEQL(OPREP1,OBJ1,DOBJ,DFL1,DFL2)) GO TO 4000
C
C Orphan fails, try GWIM.
C
3500	OBJ1=GWIM(DOBJ,DFW1,DFW2)		! get gwim.
	IF(DFLAG) WRITE(output_unit,3530) OBJ1
3530	FORMAT(' SYNMCH- DO GWIM= ',I6)
	IF(OBJ1.GT.0) GO TO 4000		! test result.
	CALL ORPHAN(-1,ACT,0,0,IAND(DOBJ, VPMASK),' ',PREP2,OBJ2)	! fails, orphan.
	BUNSUB=0				! no EXCEPT clause.
	IF(OBJ2.GT.0) GO TO 3800		! if iobj, go print.
3700	WRITE(output_unit,3750)
	1	LCWORD(1:NBLEN(LCWORD)),LCPRP1(1:NBLEN(LCPRP1)+1)
3750	FORMAT(1X,A,A,'what?')
	TELFLG=.TRUE.
	RETURN
C
3800	X=IABS(ODESC2(OBJ2))			! get iobj description.
	READ(DBCH,REC=X) J,STR			! read data base.
	CALL TXCRYP(X,STR)			! decrypt the line.
	WRITE(output_unit,3880) LCWORD(1:NBLEN(LCWORD)),
	1	LCPRP1(1:NBLEN(LCPRP1)+1),
	2	LCPRP2(1:NBLEN(LCPRP2)+1),STR(1:NBLEN(STR))
3880	FORMAT(1X,A,A,'what',A,'the ',A,'?')
	TELFLG=.TRUE.
	RETURN

C SYNMCH, PAGE 4
C
C Try to fill indirect object slot if that was the problem.
C
4000	IF((IAND(VFLAG, SIND).EQ.0).OR.(OBJ2.NE.0)) GO TO 6000
	OBJ2=IAND(OFLAG, OOBJ2)
	IF(OBJ2.EQ.0) GO TO 4500		! any orphan?
	IF(SYNEQL(OPREP2,OBJ2,IOBJ,IFL1,IFL2)) GO TO 6000
C
C Orphan fails, try GWIM.
C
4500	OBJ2=GWIM(IOBJ,IFW1,IFW2)		! gwim.
	IF(DFLAG) WRITE(output_unit,4550) OBJ2
4550	FORMAT(' SYNMCH- IO GWIM= ',I6)
	IF(OBJ2.GT.0) GO TO 6000
	IF(OBJ1.GT.0) GO TO 4600		! if dobj, go print.
	CALL ORPHAN(-1,ACT,IAND(OFLAG, OPREP1),
	1	IAND(OFLAG, OOBJ1),IAND(IOBJ, VPMASK),' ',0,0)
	GO TO 3700
C
C Error with direct object available.
C
4600	CALL ORPHAN(-1,ACT,PREP1,OBJ1,IAND(IOBJ, VPMASK),' ',0,0)
	X=IABS(ODESC2(OBJ1))			! get dobj description.
	READ(DBCH,REC=X) J,STR			! read data base.
	CALL TXCRYP(X,STR)			! decrypt the line.
	WRITE(output_unit,4660) LCWORD(1:NBLEN(LCWORD)),
	1	LCPRP1(1:NBLEN(LCPRP1)+1),
	2	STR(1:NBLEN(STR)),LCPRP2(1:NBLEN(LCPRP2)+1)
4660	FORMAT(1X,A,A,'the ',A,A,'what?')
	TELFLG=.TRUE.
	RETURN
C
C Total chomp.
C
10000	CALL RSPEAK(601)			! cant do anything.
	BUNSUB=0
	RETURN

C SYNMCH, PAGE 5
C
C Now try to take individual objects and
C in general clean up the parse vector.
C
6000	IF(IAND(VFLAG, SFLIP).EQ.0) GO TO 7000	! flip?
	J=OBJ1					! yes.
	OBJ1=OBJ2
	OBJ2=J
C
7000	PRSA=IAND(VFLAG, SVMASK)		! get verb.
	PRSO=OBJ1				! get dir obj.
	PRSI=OBJ2				! get ind obj.
	IF(.NOT.TAKEIT(PRSO,DOBJ)) RETURN	! try take.
	IF(.NOT.TAKEIT(PRSI,IOBJ)) RETURN	! try take.
	SYNMCH=.TRUE.
	IF(DFLAG) WRITE(output_unit,7050)
     &    SYNMCH,PRSA,PRSO,PRSI,ACT,OBJ1,OBJ2
7050	FORMAT(' SYNMCH- RESULTS ',L1,6I7)

	END FUNCTION SYNMCH

C UNPACK-	Unpack syntax specification, adv pointer
C
C Declarations
C
	SUBROUTINE UNPACK(OLDJ,J)
	use dparam
	IMPLICIT INTEGER(A-Z)

C
	DO 10 I=1,11				! clear syntax.
	  SYN(I)=0
10	CONTINUE
C
	VFLAG=VVOC(OLDJ)
	J=OLDJ+1
	IF(IAND(VFLAG, SDIR).EQ.0) RETURN	! dir object?
	DFL1=-1					! assume std.
	DFL2=-1
	IF(IAND(VFLAG, SSTD).EQ.0) GO TO 100	! std object?
	DFW1=-1					! yes.
	DFW2=-1
	DOBJ=VABIT+VRBIT+VFBIT
	GO TO 200
C
100	DOBJ=VVOC(J)				! not std.
	DFW1=VVOC(J+1)
	DFW2=VVOC(J+2)
	J=J+3
	IF(IAND(DOBJ, VEBIT).EQ.0) GO TO 200	! vbit = vfwim?
	DFL1=DFW1				! yes.
	DFL2=DFW2
C
200	IF(IAND(VFLAG, SIND).EQ.0) RETURN	! ind object?
	IFL1=-1					! assume std.
	IFL2=-1
	IOBJ=VVOC(J)
	IFW1=VVOC(J+1)
	IFW2=VVOC(J+2)
	J=J+3
	IF(IAND(IOBJ, VEBIT).EQ.0) RETURN	! vbit = vfwim?
	IFL1=IFW1				! yes.
	IFL2=IFW2
	RETURN
C
	END

C SYNEQL-	Test for syntax equality
C
C Declarations
C
	LOGICAL FUNCTION SYNEQL(PREP,OBJ,SPREP,SFL1,SFL2)
	use dparam
	IMPLICIT INTEGER(A-Z)

C
	IF(OBJ.EQ.0) GO TO 100			! any object?
	SYNEQL=(PREP.EQ.IAND(SPREP, VPMASK)).AND.
	1	(IOR(IAND(SFL1, OFLAG1(OBJ)),
	2	  IAND(SFL2, OFLAG2(OBJ))).NE.0)
	RETURN
C
100	SYNEQL=(PREP.EQ.0).AND.(SFL1.EQ.0).AND.(SFL2.EQ.0)
	RETURN
C
	END

C TAKEIT-	Parser based take of object
C
C Declarations
C
	LOGICAL FUNCTION TAKEIT(OBJ,SFLAG)
	use dparam
	IMPLICIT INTEGER(A-Z)

	LOGICAL TAKE,LIT
C
	TAKEIT=.FALSE.				! assume loses.
	IF((OBJ.EQ.0).OR.(OBJ.GT.STRBIT).OR.DEADF)
	1	GO TO 4000			! null/stars/dead win.
	ODO2=ODESC2(OBJ)			! get desc.
	X=OCAN(OBJ)				! get container.
	IF((X.EQ.0).OR.(IAND(SFLAG, VFBIT).EQ.0)) GO TO 500
	IF(IAND(OFLAG2(X), OPENBT).NE.0) GO TO 500
	CALL RSPSUB(566,ODO2)			! cant reach.
	RETURN
C
500	IF(IAND(SFLAG, VRBIT).EQ.0) GO TO 1000	! shld be in room?
	IF(IAND(SFLAG, VTBIT).EQ.0) GO TO 2000	! can be taken?
C
C Should be in room (VRBIT NE 0) and can be taken (VTBIT NE 0)
C
	IF(SCHLST(0,0,HERE,0,0,OBJ).LE.0) GO TO 4000 ! if not, ok.
C
C Its in the room and can be taken.
C
	IF(IAND(OFLAG1(OBJ), TAKEBT).NE.0) GO TO 3000
C
C Not takeable.  If we care, fail.
C
	IF(IAND(SFLAG, VCBIT).EQ.0) GO TO 4000	! if no care, return.
	CALL RSPSUB(445,ODO2)
	RETURN
C
C 1000--	It should not be in the room.
C 2000--	It cant be taken.
C
2000	IF(IAND(SFLAG, VCBIT).EQ.0) GO TO 4000	! if no care, return
1000	IF(SCHLST(0,0,HERE,0,0,OBJ).LE.0) GO TO 4000
	I=665					! assume player.
	IF(WINNER.NE.PLAYER) I=1082
	CALL RSPSUB(I,ODO2)			! doesn't have it.
	RETURN
C
C 3000--	Take object.
C
3000	IF(LIT(HERE)) GO TO 3500		! lit?
	CALL RSPEAK(579)			! can't do it.
	RETURN
C
3500	SVA=PRSA				! save parse vector
	SVO=PRSO
	SVI=PRSI
	PRSA=TAKEW				! make 'take obj'
	PRSO=OBJ
	PRSI=0					! no indirect object
	TAKEIT=TAKE(.TRUE.)			! try to take object
	PRSA=SVA				! restore parse vector.
	PRSO=SVO
	PRSI=SVI
	RETURN
C
C 4000--	Win on general principles.
C
4000	TAKEIT=.TRUE.
	RETURN
C
	END

C GWIM- Get what I mean in ambiguous situations
C
C Declarations
C
	INTEGER FUNCTION GWIM(SFLAG,SFW1,SFW2)
	use dparam
	IMPLICIT INTEGER(A-Z)

	LOGICAL TAKEIT,NOCARE,LIT
C
	GWIM=0					! no result.
	IF(DEADF) RETURN			! dead? gwim disabled.
	AV=AVEHIC(WINNER)
	NOCARE=IAND(SFLAG, VCBIT).EQ.0
C
C First search adventurer
C
	IF(IAND(SFLAG, VABIT).NE.0)
	1	GWIM=FWIM(SFW1,SFW2,0,0,WINNER,NOCARE)
	IF((GWIM.LT.0).OR..NOT.LIT(HERE).OR.
	1  (IAND(SFLAG, VRBIT).EQ.0)) RETURN
C
C Also search room
C
100	ROBJ=FWIM(SFW1,SFW2,HERE,0,0,NOCARE)
	IF(ROBJ) 500,600,200			! test result.
C
C ROBJ > 0: if prev object, fail
C
200	IF((AV.EQ.0).OR.(ROBJ.EQ.AV).OR.
	1	(IAND(OFLAG2(ROBJ), FINDBT).NE.0)) GO TO 300
	IF(OCAN(ROBJ).NE.AV) RETURN		! unreachable? use prev obj.
C
300	IF(GWIM.EQ.0) GO TO 400			! prev obj?
	GWIM=-GWIM				! yes, ambiguous.
	RETURN
C
400	IF(.NOT.TAKEIT(ROBJ,SFLAG)) RETURN	! if untakeable, return prev.
500	GWIM=ROBJ				! return room seach.
600	RETURN
C
	END

C NOADJS-	See if any adjectives for object
C
C Declarations
C
	LOGICAL FUNCTION NOADJS(OBJ)
	use dparam
	IMPLICIT INTEGER(A-Z)

C
	NOADJS=.FALSE.				! assume false.
	DO 100 I=1,AVMAX			! search adj.
	  IF(IABS(AVOC(I)).EQ.OBJ) RETURN	! found adjective?
	  IF(AVOC(I).EQ.0) GO TO 200		! end of list?
100	CONTINUE
200	NOADJS=.TRUE.				! true.
	RETURN
C
	END

C LCIFY-	"Lower case"-ify a string for printing
C
C Declarations
C
	CHARACTER*(*) FUNCTION LCIFY(STRING,START)
	IMPLICIT INTEGER(A-Z)
	CHARACTER*(*) STRING
C
	LCIFY=STRING				! assume input = output.
	K=LEN(STRING)				! get input length.
	IF(START.GT.K) RETURN			! anything to convert?
C
	ULCVT=ICHAR('a')-ICHAR('A')		! conversion factor
	DO 100 I=START,K			! loop on characters
	  IF((STRING(I:I).GE.'A').AND.(STRING(I:I).LE.'Z'))
	1	LCIFY(I:I)=CHAR(ICHAR(STRING(I:I))+ULCVT)
100	CONTINUE
	RETURN
C
	END

C FINDVB-	Find verb string corresponding to syntax.
C
C Declarations
C
	CHARACTER*(*) FUNCTION FINDVB(SYNTAX)
	use dparam
	IMPLICIT INTEGER(A-Z)

C
	J=1
	DO 100 K=1,VWMAX			! loop through verbs
	  NEWJ=J+VVOC(J)+1			! start of next syntax
	  IF((J.LE.SYNTAX).AND.(SYNTAX.LT.NEWJ)) GO TO 200
	  IF(VWORD(K)(1:1).NE.'*') J=NEWJ	! if last synonym, advance.
100	CONTINUE
	FINDVB=' '				! disaster
	RETURN
C
200	FINDVB=VWORD(K)				! return string
	IF(VWORD(K)(1:1).EQ.'*') FINDVB=VWORD(K)(2:WRDLNT)
	RETURN
C
	END

C FINDPR-	Find preposition string corresponding to index.
C
C Declarations
C
	CHARACTER*(*) FUNCTION FINDPR(PREPNO)
	use dparam
	IMPLICIT INTEGER(A-Z)

C
	DO 100 I=1,PWMAX			! loop through prepositions.
	  IF(PVOC(I).EQ.PREPNO) GO TO 200
100	CONTINUE
	FINDPR=' '
	RETURN
C
200	FINDPR=PWORD(I)
	RETURN
C
	END
