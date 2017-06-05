! Main program for DUNGEON

! COPYRIGHT 1980, 1990, INFOCOM COMPUTERS AND COMMUNICATIONS, CAMBRIDGE MA.
! ALL RIGHTS RESERVED, COMMERCIAL USAGE STRICTLY PROHIBITED
! WRITTEN BY R. M. SUPNIK

! 02-Dec-15     EMG     Compile using gfortran
! 16-Aug-94      RMS      Version 3.2.
! 30-Jun-92      RMS      Changed file names to lower case.
! 29-Jun-92      RMS      Changed OUTCH to 6 for VAX, UNIX compatibility.

PROGRAM DUNGEO
use state!,only: xnorth,xne,xeast,xse,xsouth,xsw,xwest,xnw,bkvw,
!     & bkve, bktwi, bkvau
use gamemod,only:game
implicit none
      
LOGICAL INITFL
integer i,j,x

integer,parameter :: skolwl(*)=[BKVW,271,XEAST,BKVE,272,XWEST, BKTWI, 270, XSOUTH, BKVAU, 269, XNORTH]

 cpdr(:) = [XNORTH,-8,XNE,-7,XEAST,1,XSE,9, XSOUTH,8,XSW,7,XWEST,-1,XNW,-9]
 CPWL(:)=[269,-8,270,8,271,1,272,-1]
 CPVEC(:) =[1,1,1,1,1,1,1,1,1,0,-1,0,0,-1,0,1,1,-1,0,1,0,-2,0,1,1,0,0,0,0,1,0,1,1,-3,0,0,-1,-1,0,1,1,0,0,-1,0,0,0,1,1,&
 1,1,0,0,0,1,1,1,1,1,1,1,1,1,1]

XELNT(:)=[1,2,3,3]

VMAJ=4
VMIN=0
VEDIT='A'

BATDRP(:)=[66,67,68,69,70,71,72,65,73]
      
      
! DUNGEON, PAGE 2

! 1) Initialize data structures
! 2) Initialize files
! 3) Play game

MLNT=0                              ! init array counters.
OLNT=0                              ! array limits are parameters
RLNT=0
VLNT=0
ALNT=0
CLNT=0
XLNT=1
R2LNT=0

LTSHFT=10                        ! set up state variables.
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
PRSA=0                              ! clear parse vector.
PRSI=0
PRSO=0
PRSCON=1
OFLAG=0                              ! clear orphans.
OACT=0
OPREP1=0
OOBJ1=0
OPREP=0
ONAME=' '
OPREP2=0
OOBJ2=0
BUNLNT=0                        ! clear bunch vector.
BUNSUB=0
BUNVEC(:)=0
THFFLG=.FALSE.                        ! thief not introduced but
THFACT=.TRUE.                        ! is active.
SWDACT=.FALSE.                        ! sword is inactive.
SWDSTA=0                        ! sword is off.

MBASE=0                              ! init melee base.
DBCH=2                  ! data base.

! DUNGEON, PAGE 3
! Init all arrays.
!
 CFLAG(:)=.FALSE.
 CCNCEL(:)=.FALSE.
 CTICK(:)=0
 CACTIO(:)=0

FLAGS(:)=.FALSE.
EGYPTF=.TRUE.                        ! some start as true.
CAGETF=.TRUE.
MR1F=.TRUE.
MR2F=.TRUE.
FOLLWF=.TRUE.
SWITCH(:)=0
ORMTCH=4                        ! number of matches.
LCELL=1
PNUMB=1
MDIR=270
MLOC=MRB
CPHERE=10

R2(:)=0
O2(:)=0
TRAVEL(:)=0

VOPPS(:)=0
VPROB(:)=0
VILLNS(:)=0
VBEST(:)=0
VMELEE(:)=0
      
! DUNGEON, PAGE 4

ODESC1(:)=0
ODESC2(:)=0
ODESCO(:)=0
OREAD(:)=0
OACTIO(:)=0
OFLAG1(:)=0
OFLAG2(:)=0
OFVAL(:)=0
OTVAL(:)=0
OSIZE(:)=0
OCAPAC(:)=0
OCAN(:)=0
OADV(:)=0
OROOM(:)=0

RDESC2=0                        ! clear desc base ptr.

RDESC1(:)=0
RACTIO(:)=0
RFLAG(:)=0
RVAL(:)=0
REXIT(:)=0
! clear message directory.
RTEXT(:)=0
! clear adventurer's arrays.
AROOM(:)=0
ASCORE(:)=0
AVEHIC(:)=0
AOBJ(:)=0
AACTIO(:)=0
ASTREN(:)=0
AFLAG(:)=0

DBGFLG=0
PRSFLG=0
GDTFLG=1
FROMDR=0                        ! init scol goodies.
SCOLRM=0
SCOLAC=0

IF(INITFL()) CALL GAME                  ! if init files, play game.
   
end program
