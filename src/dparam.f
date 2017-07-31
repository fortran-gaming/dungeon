C Declarations for DUNGEON
C
C COPYRIGHT 1980, 1990, INFOCOM COMPUTERS AND COMMUNICATIONS, CAMBRIDGE MA.
C ALL RIGHTS RESERVED, COMMERCIAL USAGE STRICTLY PROHIBITED
C WRITTEN BY R. M. SUPNIK
C
C This file should be included in *all* modules to supply consistent
C data structure definitions.
C
C 02-Dec-15     EMG     Update to compile using gfortran
C 29-Sep-94      RMS      Added COUNT, LDOOR, HEADS, COKES, GBROCH, SHOUS,
C                  FORE2, DIGBT.  Deleted SLEPBT.  Modified GLOBAL.
C 01-Feb-94      RMS      Aligned vocabulary COMMON.
C 25-Jan-94      RMS      Added sandy beach room.

! Array size parameters

      module dparam
      use, intrinsic:: iso_fortran_env, only: input_unit,output_unit
	    implicit none

      character(*), parameter :: savegameFN = 'dungeon_save.dat'

      ! hack for parser.f only
      INTEGER OBJVEC(2),PRPVEC(2)
      ! hack for gdt.f only
!      INTEGER :: EQR(RMAX,5),EQO(OMAX,14),EQC(CMAX,2),EQV(VMAX,5),
!     &           EQA(AMAX,7)
!---------------------------------------------------
      logical :: debug=.false.
      logical :: cheat=.false.
      integer :: ucheat  ! file handle for cheat file

      integer,PARAMETER :: MMAX=1500                  ! message
      integer,PARAMETER :: RMAX=200                  ! rooms
      integer,PARAMETER :: XXMAX=1000                  ! exits
      integer,PARAMETER :: OMAX=300                  ! objects
      integer,PARAMETER :: R2MAX=20                  ! multiroom objects
      integer,PARAMETER :: CMAX=30                  ! clock events
      integer,PARAMETER :: VMAX=4                  ! villains
      integer,PARAMETER :: AMAX=4                  ! actors
      integer,PARAMETER :: FMAX=56                  ! flags
      integer,parameter :: SMAX=24                  ! switches
      integer,parameter :: BWMAX=12                  ! buzzword vocabulary
      integer,parameter :: DWMAX=25                  ! direction vocabulary
      integer,parameter :: PWMAX=20                  ! preposition vocabulary
      integer,parameter :: AWMAX=160                  ! adjective vocabularly
      integer,parameter :: AVMAX=300                  ! string to obj index
      integer,parameter :: OWMAX=360                  ! object vocabularly
      integer,parameter :: OVMAX=550                  ! string to obj index
      integer,parameter :: VWMAX=240                  ! verb vocabularly
      integer,parameter :: VVMAX=750                  ! verb syntaxes

C Other parameters
C
      integer,parameter :: RECLNT=80                  ! DTEXT.DAT record size, bytes
      integer,parameter :: TEXLNT=76                  ! text buffer size, char
      integer,parameter :: WRDLNT=8                  ! word length size, char
      integer,parameter :: BUNMAX=10                  ! bunched objects
      integer,parameter :: LEXMAX=20                  ! lexical tokens
C
C Syntax definitions
C
      integer,parameter :: SDIR=16384                  ! direct object present
      integer,parameter :: SIND=8192                  ! indirect object present
      integer,parameter :: SSTD=4096                  ! direct object std format
      integer,parameter :: SFLIP=2048                  ! flip direct, indirect obj
      integer,parameter :: SDRIV=1024                  ! default (driver) syntax
      integer,parameter :: SVMASK=511                  ! mask for verb number
      integer,parameter :: VABIT=16384                  ! search adv for object
      integer,parameter :: VRBIT=8192                  ! search room for object
      integer,parameter :: VTBIT=4096                  ! take object if possible
      integer,parameter :: VCBIT=2048                  ! must have object
      integer,parameter :: VEBIT=1024                  ! qualifier flags = fwim flags
      integer,parameter :: VFBIT=512                  ! must reach object
      integer,parameter :: VPMASK=511                  ! mask for prep number
C
C Clock event indices
C
      integer,parameter :: CEVCUR=1                 ! wound cure timer
      integer,parameter :: CEVMNT=2                 ! maintenance room timer
      integer,parameter :: CEVLNT=3                 ! lantern timer
      integer,parameter :: CEVMAT=4                 ! match timer
      integer,parameter :: CEVCND=5                 ! candle timer
      integer,parameter :: CEVBAL=6                 ! balloon inflation
      integer,parameter :: CEVBRN=7                 ! balloon burnup
      integer,parameter :: CEVFUS=8                 ! fuse burn
      integer,parameter :: CEVLED=9                 ! ledge collapse timer
      integer,parameter :: CEVSAF=10                 ! safe timer
      integer,parameter :: CEVVLG=11                 ! volcano gnome appearance
      integer,parameter :: CEVGNO=12                 ! volcano gnome exit
      integer,parameter :: CEVBUC=13                 ! bucket timer
      integer,parameter :: CEVSPH=14                 ! sphere timer
      integer,parameter :: CEVEGH=15                 ! end game herald
      integer,parameter :: CEVFOR=16                 ! forest noises
      integer,parameter :: CEVSCL=17                 ! screen of light
      integer,parameter :: CEVZGI=18                 ! Zurich gnome appearance
      integer,parameter :: CEVZGO=19                 ! Zurich gnome exit
      integer,parameter :: CEVSTE=20                 ! end game start
      integer,parameter :: CEVMRS=21                 ! mirror timer
      integer,parameter :: CEVPIN=22                 ! panel timer
      integer,parameter :: CEVINQ=23                 ! inquisition timer
      integer,parameter :: CEVFOL=24                 ! master follows
      integer,parameter :: CEVBRO=25                 ! brochure timer
      integer,parameter :: CEVCYC=26                 ! cyclops demon
      integer,parameter :: CEVSLI=27                 ! slide timer
      integer,parameter :: CEVXB=28                 ! exorcism bell timer
      integer,parameter :: CEVXC=29                 ! exorcism candle timer
      integer,parameter :: CEVXBH=30                 ! exorcism bell cooling
C
C Exit definitions
C
      integer,parameter :: XLFLAG=32768           ! last entry flag
      integer,parameter :: XDMASK=31744           ! direction mask
      integer,parameter :: XRMASK=255                 ! room mask
      integer,parameter :: XFMASK=3                 ! type mask
      integer,parameter :: XFSHFT=256
      integer,parameter :: XASHFT=256
      integer,parameter :: XNORM=1                 ! normal exit
      integer,parameter :: XNO=2                 ! no exit
      integer,parameter :: XCOND=3                 ! conditional exit
      integer,parameter :: XDOOR=4                 ! door
      integer,parameter :: XMIN=1024                 ! minimum direction
      integer,parameter :: XMAX=16384                 ! maximum direction
      integer,parameter :: XNORTH=1024
      integer,parameter :: XNE=2048
      integer,parameter :: XEAST=3072
      integer,parameter :: XSE=4096
      integer,parameter :: XSOUTH=5120
      integer,parameter :: XSW=6144
      integer,parameter :: XWEST=7168
      integer,parameter :: XNW=8192
      integer,parameter :: XUP=9216
      integer,parameter :: XDOWN=10240
      integer,parameter :: XLAUN=11264
      integer,parameter :: XLAND=12288
      integer,parameter :: XENTER=13312
      integer,parameter :: XEXIT=14336
      integer,parameter :: XCROSS=15360
C
C Actor indices
C
      integer,parameter :: PLAYER=1                 ! player
      integer,parameter :: AROBOT=2                 ! robot
      integer,parameter :: AMASTR=3                 ! dungeon master
C
C Actor flags
C
      integer,parameter :: ASTAG=32768                 ! staggered
C
C Room flags
C
      integer,parameter :: RSEEN=32768                 ! seen
      integer,parameter :: RLIGHT=16384           ! lighted
      integer,parameter :: RLAND=8192                 ! land
      integer,parameter :: RWATER=4096                 ! water
      integer,parameter :: RAIR=2048                 ! air
      integer,parameter :: RSACRD=1024                 ! sacred thief cant visit)
      integer,parameter :: RFILL=512                 ! contains water
      integer,parameter :: RMUNG=256                 ! destroyed
      integer,parameter :: RBUCK=128                 ! part of bucket
      integer,parameter :: RHOUSE=64                 ! part of house
      integer,parameter :: RNWALL=32                 ! no walls
      integer,parameter :: REND=16                 ! part of endgame
C
C Room indices
C
      integer,parameter :: WHOUS=2                 ! west of house
      integer,parameter :: SHOUS=4                 ! south of house
      integer,parameter :: EHOUS=5                 ! east of house
      integer,parameter :: KITCH=6                 ! kitchen
      integer,parameter :: LROOM=8                 ! living room
      integer,parameter :: CELLA=9                 ! cellar
      integer,parameter :: MTROL=10                 ! troll room
      integer,parameter :: MAZE1=11                 ! start of maze
      integer,parameter :: MGRAT=25                 ! grating room
      integer,parameter :: MAZ15=30                 ! end of maze
      integer,parameter :: FORE1=31                 ! forest 1
      integer,parameter :: FORE2=32                 ! forest 2
      integer,parameter :: FORE3=33                 ! forest 3
      integer,parameter :: CLEAR=36                 ! clearing
      integer,parameter :: RESER=40                 ! reservoir
      integer,parameter :: STREA=42                 ! stream
      integer,parameter :: EGYPT=44                 ! egypt room
      integer,parameter :: ECHOR=49                 ! echo room
      integer,parameter :: SLIDE=58                 ! slide room
      integer,parameter :: TSHAF=61                 ! top of shaft
      integer,parameter :: BSHAF=76                 ! bottom of shaft
      integer,parameter :: MMACH=77                 ! machine room
      integer,parameter :: DOME=79                 ! dome room
      integer,parameter :: MTORC=80                 ! torch room
      integer,parameter :: CAROU=83                 ! carousel room
      integer,parameter :: RIDDL=91                 ! riddle room
      integer,parameter :: LLD1=93                 ! entrance to hades
      integer,parameter :: LLD2=94                 ! land of the living dead
      integer,parameter :: TEMP1=96                 ! temple
      integer,parameter :: TEMP2=97                 ! altar
      integer,parameter :: MAINT=100                 ! maintenance room
      integer,parameter :: MCYCL=101                 ! cyclops room
      integer,parameter :: BLROO=102                 ! back of living room
      integer,parameter :: TREAS=103                 ! thief's treasury
      integer,parameter :: RIVR1=107                 ! river 1
      integer,parameter :: RIVR2=108                 ! river 2
      integer,parameter :: RIVR3=109                 ! river 3
      integer,parameter :: RIVR4=112                 ! river 4
      integer,parameter :: RIVR5=113                 ! river 5
      integer,parameter :: FCHMP=114                 ! over falls
      integer,parameter :: SBEACH=116                 ! sandy beach
      integer,parameter :: FALLS=120                 ! top of falls
      integer,parameter :: MRAIN=121                 ! rainbow room
      integer,parameter :: POG=122                 ! pot of gold room
      integer,parameter :: VLBOT=126                 ! volcano bottom
      integer,parameter :: VAIR1=127                 ! mid air 1
      integer,parameter :: VAIR2=128                 ! mid air 2
      integer,parameter :: VAIR3=129                 ! mid air 3
      integer,parameter :: VAIR4=130                 ! mid air 4
      integer,parameter :: LEDG2=131                 ! ledge 2
      integer,parameter :: LEDG3=132                 ! ledge 3
      integer,parameter :: LEDG4=133                 ! ledge 4
      integer,parameter :: MSAFE=135                 ! safe room
      integer,parameter :: CAGER=140                 ! cage room
      integer,parameter :: CAGED=141                 ! in cage
      integer,parameter :: TWELL=142                 ! top of well
      integer,parameter :: BWELL=143                 ! bottom of well
      integer,parameter :: ALICE=144                 ! alice room
      integer,parameter :: ALISM=145                 ! alice small room
      integer,parameter :: ALITR=146                 ! alice trap room
      integer,parameter :: MTREE=147                 ! up a tree
      integer,parameter :: BKENT=148                 ! bank entrance
      integer,parameter :: BKVW=151                 ! bank view west
      integer,parameter :: BKVE=152                 ! bank view east
      integer,parameter :: BKTWI=153                 ! bank
      integer,parameter :: BKVAU=154                 ! bank vault
      integer,parameter :: BKBOX=155                 ! bank box
      integer,parameter :: CRYPT=157                 ! crypt
      integer,parameter :: TSTRS=158                 ! top of stairs
      integer,parameter :: MRANT=159                 ! mirror anteroom
      integer,parameter :: MREYE=160                 ! mirror eye
      integer,parameter :: MRA=161                 ! mirror A
      integer,parameter :: MRB=162                 ! mirror B
      integer,parameter :: MRC=163                 ! mirror C
      integer,parameter :: MRG=164                 ! mirror G
      integer,parameter :: MRD=165                 ! mirror D
      integer,parameter :: FDOOR=166                 ! front door
      integer,parameter :: MRAE=167                 ! mirror A east
      integer,parameter :: MRCE=171                 ! mirror C east
      integer,parameter :: MRCW=172                 ! mirror C west
      integer,parameter :: MRGE=173                 ! mirror G east
      integer,parameter :: MRGW=174                 ! mirror G west
      integer,parameter :: MRDW=176                 ! mirror D west
      integer,parameter :: INMIR=177                 ! in mirror
      integer,parameter :: SCORR=179                 ! south corridor
      integer,parameter :: NCORR=182                 ! north corridor
      integer,parameter :: PARAP=183                 ! parapet
      integer,parameter :: CELL=184                 ! cell
      integer,parameter :: PCELL=185                 ! prison cell
      integer,parameter :: NCELL=186                 ! nirvana cell
      integer,parameter :: CPANT=188                 ! puzzle anteroom
      integer,parameter :: CPOUT=189                 ! puzzle exit
      integer,parameter :: CPUZZ=190                 ! puzzle room
      integer,parameter :: PRM=192                 ! palantir anteroom
      integer,parameter :: PALRM=193                 ! palantir room
      integer,parameter :: SLID1=194                 ! slide 1
      integer,parameter :: SLEDG=197                 ! slide ledge
C
C Verb indices
C
      integer,parameter :: CINTW=1                 ! clock interrupt
      integer,parameter :: DEADXW=2                 ! villain dead
      integer,parameter :: FRSTQW=3                 ! villain first encounter
      integer,parameter :: INXW=4                 ! villain wakes up
      integer,parameter :: OUTXW=5                 ! villain out cold
      integer,parameter :: WALKIW=6                 ! player walks in
      integer,parameter :: FIGHTW=7                 ! fighting starts
      integer,parameter :: FOOW=8                 ! nop
      integer,parameter :: SQUEEW=68                 ! squeeze
      integer,parameter :: STAYW=73                 ! stay
      integer,parameter :: PRAYW=79                 ! pray
      integer,parameter :: BLASTW=82                 ! blast
      integer,parameter :: SCOREW=83                 ! score
      integer,parameter :: QUITW=84                 ! quit
      integer,parameter :: FOLLOW=85                 ! follow
      integer,parameter :: GTHROW=86                 ! go through
      integer,parameter :: RINGW=87                 ! ring
      integer,parameter :: DIGW=89                 ! dig
      integer,parameter :: LEAPW=91                 ! leap
      integer,parameter :: LOCKW=92                 ! lock
      integer,parameter :: UNLOKW=93                 ! unlock
      integer,parameter :: DIAGNW=94                 ! diagnose
      integer,parameter :: COUNTW=97                 ! count
      integer,parameter :: READW=100                 ! read
      integer,parameter :: MELTW=101                 ! melt
      integer,parameter :: INFLAW=102                 ! inflate
      integer,parameter :: DEFLAW=103                 ! deflate
      integer,parameter :: ALARMW=104                 ! alarm
      integer,parameter :: EXORCW=105                 ! exorcise
      integer,parameter :: PLUGW=106                 ! plug
      integer,parameter :: KICKW=107                 ! kick
      integer,parameter :: WAVEW=108                 ! wave
      integer,parameter :: RAISEW=109                 ! raise
      integer,parameter :: LOWERW=110                 ! lower
      integer,parameter :: RUBW=111                 ! rub
      integer,parameter :: PUSHW=112                 ! push
      integer,parameter :: UNTIEW=113                 ! untie
      integer,parameter :: TIEW=114                 ! tie
      integer,parameter :: TIEUPW=115                 ! tie up
      integer,parameter :: TURNW=116                 ! turn
      integer,parameter :: BREATW=117                 ! breathe
      integer,parameter :: KNOCKW=118                 ! knock
      integer,parameter :: LOOKW=119                 ! look
      integer,parameter :: EXAMIW=120                 ! examine
      integer,parameter :: SHAKEW=121                 ! shake
      integer,parameter :: MOVEW=122                 ! move
      integer,parameter :: TRNONW=123                 ! turn on
      integer,parameter :: TRNOFW=124                 ! turn off
      integer,parameter :: OPENW=125                 ! open
      integer,parameter :: CLOSEW=126                 ! close
      integer,parameter :: FINDW=127                 ! find
      integer,parameter :: WAITW=128                 ! wait
      integer,parameter :: SPINW=129                 ! spin
      integer,parameter :: BOARDW=130                 ! board
      integer,parameter :: UNBOAW=131                 ! disembark
      integer,parameter :: TAKEW=132                 ! take
      integer,parameter :: INVENW=133                 ! inventory
      integer,parameter :: EATW=135                 ! eat
      integer,parameter :: DRINKW=136                 ! drink
      integer,parameter :: BURNW=137                 ! burn
      integer,parameter :: MUNGW=138                 ! destroy
      integer,parameter :: KILLW=139                 ! kill
      integer,parameter :: SWINGW=140                 ! swing
      integer,parameter :: ATTACW=141                 ! attack
      integer,parameter :: WALKW=142                 ! walk
      integer,parameter :: TELLW=143                 ! tell
      integer,parameter :: PUTW=144                 ! put
      integer,parameter :: DROPW=145                 ! drop
      integer,parameter :: GIVEW=146                 ! give
      integer,parameter :: POURW=147                 ! pour
      integer,parameter :: THROWW=148                 ! throw
      integer,parameter :: HELLOW=151                 ! hello
      integer,parameter :: LOOKIW=152                 ! look in
      integer,parameter :: LOOKUW=153                 ! look under
      integer,parameter :: PUMPW=154                 ! pump
      integer,parameter :: WINDW=155                 ! wind
      integer,parameter :: CLMBW=156                 ! climb
      integer,parameter :: CLMBUW=157                 ! climb up
      integer,parameter :: CLMBDW=158                 ! climb down
      integer,parameter :: TRNTOW=159                 ! turn to
      integer,parameter :: PORONW=160                 ! pour on
      integer,parameter :: PUTUNW=161                 ! put under
      integer,parameter :: UTFRMW=162                 ! untie from
      integer,parameter :: MAKEW=163                 ! make
      integer,parameter :: OILW=164                 ! oil
      integer,parameter :: PLAYW=165                 ! play
      integer,parameter :: SENDW=166                 ! send
C
C Object flags
C
      integer,parameter :: VISIBT=32768           ! visible
      integer,parameter :: READBT=16384           ! readable
      integer,parameter :: TAKEBT=8192                 ! takeable
      integer,parameter :: DOORBT=4096                 ! door
      integer,parameter :: TRANBT=2048                 ! transparent
      integer,parameter :: FOODBT=1024                 ! edible
      integer,parameter :: NDSCBT=512                 ! don't describe
      integer,parameter :: DRNKBT=256                 ! drinkable
      integer,parameter :: CONTBT=128                 ! container
      integer,parameter :: LITEBT=64                 ! provides light
      integer,parameter :: VICTBT=32                 ! victim
      integer,parameter :: BURNBT=16                 ! burnable
      integer,parameter :: FLAMBT=8                 ! flaming
      integer,parameter :: TOOLBT=4                 ! tool
      integer,parameter :: TURNBT=2                 ! turnable
      integer,parameter :: ONBT=1                 ! turned on
      integer,parameter :: FINDBT=32768           ! find
      integer,parameter :: DIGBT=16384                 ! digable
      integer,parameter :: SCRDBT=8192                 ! sacred (thief wont steal)
      integer,parameter :: TIEBT=4096                 ! tieable
      integer,parameter :: CLMBBT=2048                 ! climbable
      integer,parameter :: ACTRBT=1024                 ! actor
      integer,parameter :: WEAPBT=512                 ! weapon
      integer,parameter :: FITEBT=256                 ! fighting
      integer,parameter :: VILLBT=128                 ! villain
      integer,parameter :: STAGBT=64                 ! staggered
      integer,parameter :: TRYBT=32                 ! try to take
      integer,parameter :: NOCHBT=16                 ! don't check
      integer,parameter :: OPENBT=8                 ! open
      integer,parameter :: TCHBT=4                 ! touched
      integer,parameter :: VEHBT=2                 ! vehicle
      integer,parameter :: SCHBT=1                 ! searchable
C
C Object indices
C
      integer,parameter :: GARLI=2                 ! garlic
      integer,parameter :: FOOD=3                 ! hot peppers
      integer,parameter :: GUNK=4                 ! gunk
      integer,parameter :: COAL=5                 ! piece of coal
      integer,parameter :: MACHI=7                 ! machine
      integer,parameter :: DIAMO=8                 ! diamond
      integer,parameter :: TCASE=9                 ! trophy case
      integer,parameter :: BOTTL=10                 ! bottle
      integer,parameter :: WATER=11                 ! water
      integer,parameter :: ROPE=12                 ! rope
      integer,parameter :: KNIFE=13                 ! knife
      integer,parameter :: SWORD=14                 ! sword
      integer,parameter :: LAMP=15                 ! lamp
      integer,parameter :: BLAMP=16                 ! broken lamp
      integer,parameter :: RUG=17                 ! rug
      integer,parameter :: LEAVE=18                 ! pile of leaves
      integer,parameter :: TROLL=19                 ! troll
      integer,parameter :: AXE=20                 ! axe
      integer,parameter :: KEYS=23                 ! keys
      integer,parameter :: RKNIF=24                 ! rusty knife
      integer,parameter :: BAGCO=25                 ! bag of coins
      integer,parameter :: BAR=26                 ! platinum bar
      integer,parameter :: ICE=30                 ! glacier
      integer,parameter :: COFFI=33                 ! coffin
      integer,parameter :: TORCH=34                 ! torch
      integer,parameter :: TBASK=35                 ! true basket
      integer,parameter :: FBASK=36                 ! false basket
      integer,parameter :: TIMBE=38                 ! timber
      integer,parameter :: IRBOX=39                 ! iron box
      integer,parameter :: STRAD=40                 ! violin
      integer,parameter :: GHOST=42                 ! spirits
      integer,parameter :: TRUNK=45                 ! trunk
      integer,parameter :: BELL=46                 ! bell
      integer,parameter :: BOOK=47                 ! book
      integer,parameter :: CANDL=48                 ! candles
      integer,parameter :: GUIDE=49                 ! guidebook
      integer,parameter :: MATCH=51                 ! matches
      integer,parameter :: MAILB=53                 ! mailbox
      integer,parameter :: TUBE=54                 ! tube of putty
      integer,parameter :: PUTTY=55                 ! putty
      integer,parameter :: WRENC=56                 ! wrench
      integer,parameter :: SCREW=57                 ! screwdriver
      integer,parameter :: CYCLO=58                 ! cyclops
      integer,parameter :: CHALI=59                 ! chalice
      integer,parameter :: THIEF=61                 ! thief
      integer,parameter :: STILL=62                 ! stiletto
      integer,parameter :: WINDO=63                 ! window
      integer,parameter :: GRATE=65                 ! grating
      integer,parameter :: DOOR=66                 ! door
      integer,parameter :: HPOLE=71                 ! head on pole
      integer,parameter :: RBUTT=79                 ! red button
      integer,parameter :: LEAK=78                 ! leak
      integer,parameter :: RAILI=75                 ! railing
      integer,parameter :: POT=85                 ! pot of gold
      integer,parameter :: STATU=86                 ! statue
      integer,parameter :: IBOAT=87                 ! inflatable boat
      integer,parameter :: DBOAT=88                 ! dead boat
      integer,parameter :: PUMP=89                 ! pump
      integer,parameter :: RBOAT=90                 ! inflated boat
      integer,parameter :: LABEL=91                 ! boat label
      integer,parameter :: STICK=92                 ! stick
      integer,parameter :: BARRE=93                 ! barrel
      integer,parameter :: BUOY=94                 ! buoy
      integer,parameter :: SHOVE=96                 ! shovel
      integer,parameter :: GUANO=97                 ! pile of guano
      integer,parameter :: BALLO=98                 ! balloon
      integer,parameter :: RECEP=99                 ! receptacle
      integer,parameter :: BROPE=101                 ! braided rope
      integer,parameter :: HOOK1=102                 ! hook 1
      integer,parameter :: HOOK2=103                 ! hook 2
      integer,parameter :: ZORKM=104                 ! zorkmid coin
      integer,parameter :: SAFE=105                 ! safe
      integer,parameter :: CARD=106                 ! card
      integer,parameter :: SSLOT=107                 ! safe slot
      integer,parameter :: BRICK=109                 ! brick (bomb)
      integer,parameter :: FUSE=110                 ! fuse
      integer,parameter :: GNOME=111                 ! volcano gnome
      integer,parameter :: BLABE=112                 ! balloon label
      integer,parameter :: DBALL=113                 ! dead balloon
      integer,parameter :: TOMB=119                 ! tomb
      integer,parameter :: HEADS=120                 ! heads
      integer,parameter :: COKES=121                 ! coke bottles
      integer,parameter :: LCASE=123                 ! large case
      integer,parameter :: CAGE=124                 ! cage
      integer,parameter :: RCAGE=125                 ! real cage
      integer,parameter :: SPHER=126                 ! white crystal sphere
      integer,parameter :: SQBUT=127                 ! square button
      integer,parameter :: FLASK=132                 ! flask
      integer,parameter :: POOL=133                 ! pool of sewage
      integer,parameter :: SAFFR=134                 ! spices
      integer,parameter :: BUCKE=137                 ! bucket
      integer,parameter :: ECAKE=138                 ! eatme cake
      integer,parameter :: ORICE=139                 ! orange icing
      integer,parameter :: RDICE=140                 ! red icing
      integer,parameter :: BLICE=141                 ! blue icing
      integer,parameter :: ROBOT=142                 ! robot
      integer,parameter :: RBTLB=143                 ! robot label
      integer,parameter :: TTREE=144                 ! foot of tree
      integer,parameter :: FTREE=145                 ! foot of tree
      integer,parameter :: BILLS=148                 ! pile of bills
      integer,parameter :: PORTR=149                 ! portrait
      integer,parameter :: SCOL=151                 ! screen of light
      integer,parameter :: ZGNOM=152                 ! gnome of Zurich
      integer,parameter :: NEST=153                 ! nest
      integer,parameter :: EGG=154                 ! egg
      integer,parameter :: BEGG=155                 ! broken egg
      integer,parameter :: BAUBL=156                 ! bauble
      integer,parameter :: CANAR=157                 ! canary
      integer,parameter :: BCANA=158                 ! broken canary
      integer,parameter :: YLWAL=159                 ! yellow wall
      integer,parameter :: RDWAL=161                 ! red wall
      integer,parameter :: PINDR=164                 ! pine door
      integer,parameter :: RBEAM=171                 ! red beam
      integer,parameter :: ODOOR=172                 ! endgame door
      integer,parameter :: QDOOR=173                 ! endgame door
      integer,parameter :: LDOOR=174                 ! endgame door
      integer,parameter :: CDOOR=175                 ! endgame door
      integer,parameter :: NUM1=178                 ! numeral 1
      integer,parameter :: NUM8=185                 ! numeral 8
      integer,parameter :: WARNI=186                 ! warning
      integer,parameter :: CSLIT=187                 ! card slit
      integer,parameter :: GCARD=188                 ! gold card
      integer,parameter :: STLDR=189                 ! steel door
      integer,parameter :: HBELL=190                 ! hot bell
      integer,parameter :: PLEAK=191                 ! Alice room leak
      integer,parameter :: BROCH=195                 ! brochure
      integer,parameter :: STAMP=196                 ! stamp on brochure
      integer,parameter :: PDOOR=197                 ! palantir door
      integer,parameter :: PLID1=200                 ! lid 1
      integer,parameter :: PLID2=201                 ! lid 2
      integer,parameter :: PKH1=202                 ! keyhole 1
      integer,parameter :: PKH2=203                 ! keyhole 2
      integer,parameter :: PKEY=205                 ! rusty key
      integer,parameter :: PALAN=206                 ! blue crystal sphere
      integer,parameter :: MAT=207                 ! welcome mat
      integer,parameter :: PAL3=209                 ! red crystal sphere
C
      integer,parameter :: ITOBJ=250                 ! global it
      integer,parameter :: OPLAY=251                 ! global me
      integer,parameter :: EVERY=252                 ! global everything
      integer,parameter :: VALUA=253                 ! global valuables
      integer,parameter :: POSSE=254                 ! global possessions
      integer,parameter :: SAILO=255                 ! global sailor
      integer,parameter :: TEETH=256                 ! global teeth
      integer,parameter :: WALL=257                 ! global wall
      integer,parameter :: HANDS=259                 ! global hands
      integer,parameter :: LUNGS=260                 ! global lungs
      integer,parameter :: AVIAT=261                 ! global flyer
      integer,parameter :: GBROCH=262                 ! global brochure
      integer,parameter :: GWISH=263                 ! global wish (blessing)
      integer,parameter :: GLOBAL=264                 ! end of universals
      integer,parameter :: GRWAL=265                 ! global granite wall
      integer,parameter :: WNORT=269                 ! global north wall
      integer,parameter :: GWATE=273                 ! global water
      integer,parameter :: MASTER=279                 ! global dungeon master
      integer,parameter :: BUNOBJ=284                 ! bunch pseudo object
C
C Misc definitions
C
      integer,parameter :: HFACTR=500

C
C Parser output
C
      LOGICAL PRSWON
      integer  PRSA,PRSI,PRSO,PRSCON
      COMMON /PRSVEC/ PRSA,PRSI,PRSO,PRSWON,PRSCON
C
C Parser state
C
      CHARACTER(WRDLNT) ONAME
      integer
     &      OFLAG,OACT,OPREP1,OOBJ1,OPREP,OPREP2,OOBJ2,
     &      LASTIT,ACT,OBJ1,OBJ2,PREP1,PREP2,
     &      VFLAG,DOBJ,DFL1,DFL2,DFW1,DFW2,
     &      IOBJ,IFL1,IFL2,IFW1,IFW2,
     &      BUNLNT,BUNSUB,BUNVEC(BUNMAX)

      INTEGER SYN(11)
      EQUIVALENCE (SYN(1),VFLAG)
C
C Parser vocabularies
C
      CHARACTER*(WRDLNT) BWORD(BWMAX),PWORD(PWMAX),DWORD(DWMAX),
     & AWORD(AWMAX),OWORD(OWMAX),VWORD(VWMAX)
      integer PVOC(PWMAX),
     &      DVOC(DWMAX),AVOC(AVMAX),OVOC(OVMAX),VVOC(VVMAX)

C
C Game state
C
      LOGICAL TELFLG
      integer WINNER,HERE,
     &      MOVES,DEATHS,RWSCOR,MXSCOR,MXLOAD,
     &      LTSHFT,BLOC,MUNGRM,HS,EGSCOR,EGMXSC

C
C Screen of light state

      integer FROMDR,SCOLRM,SCOLAC,SCOLDR(8),SCOLWL(12)

C
C Puzzle room state
      integer CPDR(16),CPWL(8),CPVEC(64)

C
C Message index
      integer MLNT,RTEXT(MMAX)
      COMMON /RMSG/ MLNT,RTEXT
C
C Miscellaneous variables
C
      integer INLNT,SUBLNT
      CHARACTER(TEXLNT) INBUF,SUBBUF
      CHARACTER(1) VEDIT

      integer :: MBASE,STRBIT,
     &      PLTIME,DARRAY(3),SHOUR,SMIN,
     &      BATDRP(9),TMARRAY(3),
     &      INPCH,OUTCH,DBCH,
     &      DBGFLG,PRSFLG,GDTFLG,
     &      VMAJ,VMIN

C
C Rooms

      integer  RLNT,RDESC2,RDESC1(RMAX),REXIT(RMAX),
     &      RACTIO(RMAX),RVAL(RMAX),RFLAG(RMAX)
C
C Exits

      integer XLNT,TRAVEL(XXMAX)
      COMMON /EXITS/ XLNT,TRAVEL

      integer XELNT(4),XTYPE,XROOM1,XSTRNG,XACTIO,XOBJ,xflag
      COMMON /CURXT/ XELNT,XTYPE,XROOM1,XSTRNG,XACTIO,XOBJ
      EQUIVALENCE (XFLAG,XOBJ)
C
C Objects

      integer :: OLNT,ODESC1(OMAX),ODESC2(OMAX),ODESCO(OMAX),
     &      OACTIO(OMAX),OFLAG1(OMAX),OFLAG2(OMAX),OFVAL(OMAX),
     &      OTVAL(OMAX),OSIZE(OMAX),OCAPAC(OMAX),OROOM(OMAX),
     &      OADV(OMAX),OCAN(OMAX),OREAD(OMAX)

      integer  R2LNT,O2(R2MAX),R2(R2MAX)
C
C Clock interrupts
C
      LOGICAL CFLAG(CMAX),CCNCEL(CMAX)
      integer CLNT,CTICK(CMAX),CACTIO(CMAX)
      COMMON /CEVENT/ CLNT,CTICK,CACTIO,CFLAG,CCNCEL
C
C Villains and demons
C
      LOGICAL THFFLG,SWDACT,THFACT
      integer THFPOS,SWDSTA
      COMMON /HACK/ THFPOS,THFFLG,THFACT,SWDACT,SWDSTA

      integer VLNT,VILLNS(VMAX),VPROB(VMAX),VOPPS(VMAX),
     &      VBEST(VMAX),VMELEE(VMAX)

C Adventurers
C

      integer ALNT,AROOM(AMAX),ASCORE(AMAX),AVEHIC(AMAX),
     &      AOBJ(AMAX),AACTIO(AMAX),ASTREN(AMAX),AFLAG(AMAX)

C
C Flags
C
      LOGICAL FLAGS(FMAX)
      EQUIVALENCE (FLAGS(1),TROLLF)
      INTEGER SWITCH(SMAX)
      EQUIVALENCE (SWITCH(1),BTIEF)
      LOGICAL TROLLF,CAGESF,BUCKTF,CAROFF,CAROZF,LWTIDF
      LOGICAL DOMEF,GLACRF,ECHOF,RIDDLF,LLDF,CYCLOF
      LOGICAL MAGICF,LITLDF,SAFEF,GNOMEF,GNODRF,MIRRMF
      LOGICAL EGYPTF,ONPOLF,BLABF,BRIEFF,SUPERF,BUOYF
      LOGICAL GRUNLF,GATEF,RAINBF,CAGETF,EMPTHF,DEFLAF
      LOGICAL GLACMF,FROBZF,ENDGMF,BADLKF,THFENF,SINGSF
      LOGICAL MRPSHF,MROPNF,WDOPNF,MR1F,MR2F,INQSTF
      LOGICAL FOLLWF,SPELLF,CPOUTF,CPUSHF
      LOGICAL DEADF,ZGNOMF,MATF,PLOOKF,PTOUCF
      LOGICAL BROC1F,BROC2F,EXORBF,EXORCF,PUNLKF


      integer  BTIEF,BINFF,RVMNT,RVCLR,RVCYC,RVSND,RVGUA,
     & ORRUG,ORCAND,ORMTCH,ORLAMP, MDIR,MLOC,POLEUF,
     & QUESNO,NQATT,CORRCT, lcell,PNUMB,ACELL,DCELL,CPHERE,TTIE,MATOBJ


! moved from parser.f
C SPARSE, PAGE 2
C
C Vocabularies
C
C Buzz words--	ignored in syntactic processing
C
      integer idontuse
	DATA BWORD/'BY','IS','A','AN','THE','AM','ARE',
	1	'TODAY','MY','YOUR','OUR','HIS'/
C
C Prepositions--	maps prepositions to indices
C
	DATA PWORD/'OVER','WITH','USING','AT','TO',
	1	'IN','INSIDE','INTO','DOWN','UP',
	2	'UNDER','OF','ON','OFF','FOR',
	3	'FROM','OUT','THROUGH',' ',' '/
C
	DATA PVOC/1,2,2,3,4,
	1	5,5,5,6,7,
	2	8,9,10,11,12,
	3	13,13,14,0,0/
C
C Directions--	maps directions to indices
C
	DATA DWORD/'N','NORTH','S','SOUTH',
	1 'E','EAST','W','WEST',
	2 'SE','SW','NE','NW',
	4 'U','UP','D','DOWN',
	5 'LAUNCH','LAND','EXIT','OUT',
	6 'TRAVEL','IN','CROSS',' ',' '/
C
	DATA DVOC/XNORTH,XNORTH,XSOUTH,XSOUTH,
	1 XEAST,XEAST,XWEST,XWEST,
	2 XSE,XSW,XNE,XNW,
	4 XUP,XUP,XDOWN,XDOWN,
	5 XLAUN,XLAND,XEXIT,XEXIT,
	6 XCROSS,XENTER,XCROSS,0,0/

C SPARSE, PAGE 3
C
C Adjectives--	maps adjectives to object numbers
C
C Each string entry in aword corresponds to a list of one or more
C object numbers in AVOC.  Object entries are delimited by the first
C object being positive, and all subsequent objects in the same entry
C being negative.
C
	DATA (AWORD(Idontuse),Idontuse=1,40) /
	1 'BROWN','ELONGATE','HOT','PEPPER',
	1 'VITREOUS','JADE','HUGE','ENORMOUS',
	2 'TROPHY','CLEAR','LARGE','NASTY',
	3 'ELVISH','BRASS','BROKEN','ORIENTAL',
	4 'BLOODY','RUSTY','BURNED-O','DEAD',
	5 'OLD','LEATHER','PLATINUM','PEARL',
	6 'MOBY','CRYSTAL','GOLD','IVORY',
	7 'SAPPHIRE','WOODEN','WOOD','STEEL',
	8 'DENTED','FANCY','ANCIENT','SMALL',
	9 'BLACK','TOUR','VISCOUS','VICIOUS'/
C
	DATA (AVOC(Idontuse),Idontuse=1,112) /
	1 1,-81,-133,1,3,-190,3,
	1 4,6,8,8,-122,
	2 9,10,12,-26,-47,-95,-96,-123,-133,-135,-144,-145,
	2	-150,-176,-191,13,-19,
	3 14,15,-16,-46,-156,-190,16,-22,-38,-92,-113,-155,-158,17,
	4 20,24,-205,22,22,
	5 25,-41,-44,-45,-208,25,26,27,
	6 31,32,-126,-206,-209,33,-85,-104,-157,-158,-188,34,
	7 37,38,-67,-75,-93,-136,-137,-165,-173,-174,-175,-197,-204,
	7	38,-67,-136,-137,-165,-173,-174,-175,
	7	39,-105,-124,-125,-189,
	8 39,40,41,-44,5,-46,-52,-53,-89,-102,-103,-153,-187,
	9 47,-162,49,55,62/
C
	DATA (AWORD(Idontuse),Idontuse=41,80) /
	1 'GLASS','TRAP','FRONT','STONE',
	1 'MANGLED','RED','YELLOW','BLUE',
	2 'VAMPIRE','MAGIC','SEAWORTH','TAN',
	3 'SHARP','WICKER','CLOTH','BRAIDED',
	4 'GAUDY','SQUARE','CLAY','SHINY',
	5 'THIN','GREEN','PURPLE','WHITE',
	6 'MARBLE','COKE','EMPTY','ROUND',
	7 'TRIANGUL','RARE','OBLONG','EAT-ME',
	8 'EATME','ORANGE','ECCH','ROCKY',
	9 'SHEER','200','NEAT','SHIMMERI'/
C
	DATA (AVOC(Idontuse),Idontuse=113,179) /
	1 10,-126,-132,-206,-209,66,68,69,-150,-278,
	1 	72,-124,79,-94,-140,-161,-170,-171,-190,-209,
	1	80,-159,82,-112,-114,-141,-206,
	2 83,90,-281,90,91,
	3 92,98,100,101,
	4 108,109,-127,109,110,
	5 110,77,-115,-143,116,117,-126,-147,-160,-266,
	6 119,121,121,128,
	7 129,134,135,138,
	8 138,139,141,146,
	9 146,148,148,151/
C
	DATA (AWORD(Idontuse),Idontuse=81,120) /
	1 'ZURICH','BIRDS','ENCRUSTE','BEAUTIFU',
	1 'CLOCKWOR','MECHANIC','MAHOGANY','PINE',
	2 'LONG','CENTER','SHORT','T',
	3 'COMPASS','BRONZE','CELL','LOCKED',
	4 'SUN','BARE','SONG','NORTH',
	5 'NORTHERN','SOUTH','SOUTHERN','EAST',
	6 'EASTERN','WEST','WESTERN','DUNGEON',
	7 'FREE','GRANITE','LOWERED','VOLCANO',
	8 'MAN-SIZE','METAL','PLASTIC','SILVER',
	9 'USED','USELESS','SEEING','ONE-EYED'/
C
	DATA (AVOC(Idontuse),Idontuse=180,238) /
	1 152,153,-154,-155,154,-155,86,-156,
	1 157,-158,157,-158,163,164,
	2 166,166,167,168,
	3 169,-275,172,174,-175,174,
	4 177,259,267,269,
	5 269,270,270,271,
	6 271,67,-272,67,-272,279,
	7 195,-262,265,36,111,
	8 93,64,-99,-200,-201,77,-87,-88,-90,59,
	9 22,22,126,-206,-209,58/
C
	DATA (AWORD(Idontuse),Idontuse=121,160) /
	1 'HOLY','HAND-HEL','UNRUSTY','PLAIN',
	1 'PRICELES','SANDY','GIGANTIC','LINE-PRI',
	2 'FLATHEAD','FINE','SHADY','SUSPICIO',
	3 'CROSS','TOOL','CONTROL','DON',
	4 'WOODS','GOLDEN','OAK','BARRED',
	5 'DUSTY','NARROW','IRON','WELCOME',
	6 'RUBBER','SKELETON','ALL','ZORKMID',
	7 12*' '/
C
	DATA (AVOC(Idontuse),Idontuse=239,282) /
	1 43,89,13,13,
	1 104,192,122,122,
	2 118,91,61,61,
	3 165,193,194,196,
	4 196,157,-158,197,198,-210,
	5 204,199,205,207,
	6 207,23,253,-254,104,-148,
	7 12*0/

C SPARSE, PAGE 4
C
C OBJECTS--	Maps objects to object indices,
C 		same format as AVOC.
C
	DATA (OWORD(Idontuse),Idontuse=1,40) /
	1 'BAG','SACK','GARLIC','CLOVE',
	1 'FOOD','SANDWICH','LUNCH','DINNER',
	2 'GUNK','PIECE','SLAG','COAL',
	3 'PILE','HEAP','FIGURINE','MACHINE',
	4 'PDP10','VAX','DRYER','LID',
	5 'DIAMOND','CASE','BOTTLE','CONTAINE',
	6 'WATER','QUANTITY','LIQUID','H2O',
	7 'ROPE','HEMP','COIL','KNIFE',
	8 'BLADE','SWORD','ORCHRIST','GLAMDRIN',
	9 'LAMP','LANTERN','RUG','CARPET'/
C
	DATA (OVOC(Idontuse),Idontuse=1,71) /
	1 1,-25,-100,1,2,2,
	1 3,3,3,3,
	2 4,-55,4,-143,-186,-282,4,5,
	3 5,-18,-38,-72,-73,-87,-88,-122,-148,5,6,7,
	4 7,7,7,7,-200,-201,
	5 8,9,-123,10,-121,10,
	6 11,-273,11,-273,11,-273,11,-273,
	7 12,-101,-282,12,12,-110,13,-24,
	8 13,-14,14,14,14,
	9 15,-16,-22,15,-16,-22,17,17/
C
	DATA (OWORD(Idontuse),Idontuse=41,80) /
	1 'LEAVES','LEAF','TROLL','AXE',
	1 'PRAYER','KEYS','KEY','SET',
	2 'BONES','SKELETON','BODY','COINS',
	3 'BAR','NECKLACE','PEARLS','MIRROR',
	4 'ICE','MASS','GLACIER','RUBY',
	5 'TRIDENT','FORK','COFFIN','CASKET',
	6 'TORCH','CAGE','DUMBWAIT','BASKET',
	7 'BRACELET','JEWEL','TIMBER','BOX',
	8 'STRADIVA','VIOLIN','ENGRAVIN','INSCRIPT',
	9 'GHOST','SPIRIT','FIEND','GRAIL'/
C
	DATA (OVOC(Idontuse),Idontuse=72,130) /
	1 18,18,19,-111,20,
	1 44,-47,23,23,-205,23,
	2 21,21,21,-72,-73,25,
	3 26,-165,-168,27,27,28,-29,-276,
	4 30,30,30,31,
	5 32,32,33,33,
	6 34,35,-36,-124,-125,35,-36,35,-36,-98,-113,
	7 37,37,38,39,-53,-105,
	8 40,40,41,41,-44,
	9 42,42,42,43/
C
	DATA (OWORD(Idontuse),Idontuse=81,120) /
	1 'TRUNK','CHEST','BELL','BOOK',
	1 'BIBLE','GOODBOOK','CANDLES','PAIR',
	2 'GUIDEBOO','GUIDE','PAPER','NEWSPAPE',
	3 'ISSUE','REPORT','MAGAZINE','NEWS',
	4 'MATCHBOO','MATCH','MATCHES','ADVERTIS',
	5 'PAMPHLET','LEAFLET','BOOKLET','MAILBOX',
	6 'TUBE','TOOTHPAS','PUTTY','MATERIAL',
	7 'GLUE','WRENCH','SCREWDRI','CYCLOPS',
	8 'MONSTER','CHALICE','CUP','GOBLET',
	9 'PAINTING','ART','CANVAS','PICTURE'/
C
	DATA (OVOC(Idontuse),Idontuse=131,182) /
	1 45,45,-193,46,-190,47,-49,-114,-115,-116,-117,
	1 47,47,48,48,
	2 49,49,50,-122,-143,-186,50,
	3 50,50,50,50,
	4 51,51,51,52,
	5 52,52,52,53,
	6 54,54,55,55,
	7 55,56,57,58,
	8 58,59,59,59,
	9 60,-149,60,-149,60,60/
C
	DATA (OWORD(Idontuse),Idontuse=121,160) /
	1 'WORK','MASTERPI','THIEF','ROBBER',
	1 'CRIMINAL','BANDIT','CROOK','GENT',
	2 'GENTLEMA','MAN','INDIVIDU','BAGMAN',
	3 'STILETTO','WINDOW','BOLT','NUT',
	4 'GRATE','GRATING','DOOR','TRAP-DOO',
	5 'SWITCH','HEAD','CORPSE','BODIES',
	6 'DAM','GATES','GATE','FCD',
	7 'RAIL','RAILING','BUTTON','BUBBLE',
	8 'LEAK','DRIP','HOLE','BAT',
	9 'RAINBOW','POT','STATUE','SCULPTUR'/
C
	DATA (OVOC(Idontuse),Idontuse=183,258) /
	1 60,60,61,61,
	1 61,61,61,61,
	2 61,61,61,61,
	3 62,63,-198,-210,64,64,
	4 65,65,66,-67,-68,-69,-119,-164,
	4	-172,-173,-174,-175,-189,-197,66,
	5 70,-79,-80,-81,-82,-170,71,-120,72,-73,72,-73,
	6 74,74,-76,74,-76,74,
	7 75,75,76,-79,-80,-81,-82,-127,-128,-129,-170,-176,77,
	8 78,-191,78,78,-107,-202,-203,83,
	9 84,85,86,86/
C
	DATA (OWORD(Idontuse),Idontuse=161,200) /
	1 'ROCK','BOAT','PLASTIC','PUMP',
	1 'AIRPUMP','AIR-PUMP','LABEL','FINEPRIN',
	2 'STICK','BARREL','BUOY','EMERALD',
	3 'SHOVEL','GUANO','CRAP','SHIT',
	4 'HUNK','BALLOON','RECEPTAC','WIRE',
	5 'HOOK','ZORKMID','COIN','SAFE',
	6 'CARD','NOTE','SLOT','CROWN',
	7 'BRICK','FUSE','GNOME','STAMP',
	8 'TOMB','CRYPT','GRAVE','HEADS',
	9 'POLES','IMPLEMEN','LOSERS','COKES'/
C
	DATA (OVOC(Idontuse),Idontuse=259,312) /
	1 86,87,-88,-90,87,-88,-90,89,
	1 89,89,91,-112,91,
	2 92,93,94,95,
	3 96,97,97,97,
	4 97,98,-113,99,101,-110,
	5 102,-103,104,-148,104,105,
	6 106,-188,106,-186,107,-187,108,
	7 109,110,111,-152,118,-196,
	8 119,119,119,120,
	9 120,120,120,121/
C
	DATA (OWORD(Idontuse),Idontuse=201,240) /
	1 'LISTINGS','OUTPUT','PRINTOUT','SPHERE',
	1 'BALL','ETCHING','WALLS','WALL',
	2 'FLASK','POOL','SEWAGE','TIN',
	3 'SAFFRON','SPICES','TABLE','POST',
	4 'POSTS','BUCKET','CAKE','ICING',
	5 'ROBOT','ROBBY','C3PO','R2D2',
	6 'PANEL','POLE','TBAR','T-BAR',
	7 'ARROW','POINT','BEAM','DIAL',
	8 'SUNDIAL','1','ONE','2',
	9 'TWO','3','THREE','4'/
C
	DATA (OVOC(Idontuse),Idontuse=313,387) /
	1 122,122,122,126,-206,-209,
	1 126,130,-131,130,-131,-257,130,-131,-159,
	1	-160,-161,-162,-163,-164,-257,-265,-269,-270,-271,-272,
	2 132,133,133,134,
	3 134,134,135,-204,136,-166,-167,
	4 136,137,138,-139,-140,-141,139,-140,-141,
	5 142,142,142,142,
	6 159,-160,-161,-162,-163,-164,-194,-277,120,-166,-167,168,168,
	7 169,169,171,177,
	8 177,178,178,179,
	9 179,180,180,181/
C
	DATA (OWORD(Idontuse),Idontuse=241,280) /
	1 'FOUR','5','FIVE','6',
	1 'SIX','7','SEVEN','8',
	2 'EIGHT','WARNING','SLIT','IT',
	3 'THAT','THIS','ME','MYSELF',
	4 'CRETIN','ALL','EVERYTHI','TREASURE',
	5 'VALUABLE','SAILOR','TEETH','GRUE',
	6 'HAND','HANDS','LUNGS','AIR',
	7 'AVIATOR','FLYER','TREE','CLIFF',
	8 'LEDGE','PORTRAIT','STACK','BILLS',
	9 'VAULT','CUBE','LETTERIN','CURTAIN'/
C
	DATA (OVOC(Idontuse),Idontuse=388,432) /
	1 181,182,182,183,
	1 183,184,184,185,
	2 185,186,187,250,
	3 250,250,251,251,
	4 251,252,252,253,
	5 253,255,256,258,
	6 259,259,260,260,
	7 261,261,144,-145,-268,146,-147,
	8 146,149,122,-148,148,
	9 150,150,67,-150,151/
C
	DATA (OWORD(Idontuse),Idontuse=281,320) /
	1 'LIGHT','NEST','EGG','BAUBLE',
	1 'CANARY','BIRD','SONGBIRD','GUARD',
	2 'GUARDIAN','ROSE','STRUCTUR','CHANNEL',
	3 'KEEPER','LADDER','BROCHURE','WISH',
	4 'GROUND','EARTH','SAND','WELL',
	5 'SLIDE','CHUTE','HOUSE','BOTTLES',
	6 'BUNCH','PALANTIR','STONE','FLINT',
	7 'POSSESSI','GOOP','BEACH','GRIP',
	8 'HANDGRIP','PRINT','ETCHINGS','CRACK',
	9 'KEYHOLE','MAT','STOVE','PLATINUM'/
C
	DATA (OVOC(Idontuse),Idontuse=433,485) /
	1 15,-151,-171,153,154,-155,156,
	1 157,-158,267,267,274,
	2 274,275,276,278,
	3 279,280,195,-262,263,
	4 264,264,192,-264,281,
	5 283,283,266,121,
	6 121,126,-206,-209,126,-206,-209,51,
	7 254,133,192,167,
	8 167,91,-122,130,-131,199,
	9 202,-203,207,208,26/
C
	DATA (OWORD(Idontuse),Idontuse=321,360) /
	1 'HIM','SELF','GOLD','SAPPHIRE',
	1 'IVORY','MASTER','CANDLE','JADE',
	2 'SCREEN','BLESSING','GHOSTS','SPIRITS',
	3 'CORPSES','JEWELS','CLIFFS','CHIMNEY',
	4 24*' '/
C
	DATA (OVOC(Idontuse),Idontuse=486,529) /
	1 250,251,85,-104,37,
	1 34,279,48,6,
	2 151,263,42,42,
	3 72,-73,37,-45,146,-147,211,
	4 24*0/

C SPARSE, PAGE 5
C
C VERBS--	Maps verbs to syntax slots
C
C Vocabulary entries are variable length and consist of one
C or more words.  If an entry contains more than one word,
C all but the last are prefaced with an '*'.  The preferred
C string for error messages should be first.
C
C Syntax entries consist of a flag word followed by 0, 1, or 2
C Object descriptions.  The flag word has the following format--
C
C bit <14>	if 1, syntax includes direct object
C bit <13>	if 1, syntax includes indirect object
C bit <12>	if 1, direct object is implicit (standard form)
C bit <11>	if 1, direct and indirect object must be swapped
C			after syntax processing
C bit <10>	if 1, this is default syntax for orphanery
C bits <8:0>	verb number for VAPPLI
C
C Object descriptions consist of a flag word and two FWIM words.
C The flag word has the following format--
C
C bit <14>	if 1, search adventurer for object
C bit <13>	if 1, search room for object
C bit <12>	if 1, parser will try to take object
C bit <11>	if 1, adventurer must have object
C bit <10>	if 1, qualifying bits (normally -1,-1) are same
C			as FWIM bits
C bit <9>	if 1, object must be reachable
C bits <8:0>	preposition number for SYNMCH
C
C The FWIM words have the same format as the two object flag words.
C
C Note that bits 12 and 11 of object descriptions actually have
C four distinct states--
C
C	bit 12	bit 11	mdldesc		interpretation
C	------	------	-------		---------------
C
C	  0	  0	 --		no parser action
C	  0	  1	 HAVE		adventurer must have object
C	  1	  0	 TRY		try to take, dont care if fail
C	  1	  1	 TAKE		try to take, care if fail
C

C SPARSE, PAGE 6
C
	DATA (VWORD(Idontuse),Idontuse=1,43) /
	1 'BRIEF','VERBOSE','SUPERBRI','STAY',
	1 'VERSION','*SWIM','*BATHE','WADE',
	2 'GERONIMO','*ULYSSES','ODYSSEUS','*PLUGH','XYZZY',
	3 'PRAY','TREASURE','TEMPLE','BLAST',
	4 'SCORE','*QUIT','*GOODBYE','*Q','BYE','HELP',
	5 'INFO','*HISTORY','UPDATE','BACK',
	6 '*MUMBLE','SIGH','*CHOMP','*LOSE',
	7 'BARF','DUNGEON','FROBOZZ','*FOO',
	8 '*BLETCH','BAR','REPENT','*HOURS',
	9 'SCHEDULE','WIN','*YELL','*SCREAM'/
C
	DATA (VVOC(Idontuse),Idontuse=1,54) /
	1 1,70,1,71,1,72,1,73,
	1 1,74,1,75,
	2 1,76,1,77,1,56,
	3 1,79,1,80,1,81,1,82,
	4 1,83,1,84,1,40,
	5 1,41,1,42,1,43,
	6 1,44,
	7 1,45,1,46,1,47,
	8 1,48,1,49,
	9 1,50,1,51/
C
	DATA (VWORD(Idontuse),Idontuse=44,86) /
	1 'SHOUT','*HOP','SKIP','*CURSE',
	1 '*SHIT','*DAMN','FUCK','ZORK',
	2 'WISH','SAVE','RESTORE','TIME',
	3 'DIAGNOSE','EXORCISE','*LIST','*I','INVENTOR',
	4 'WAIT','INCANT','*ANSWER','RESPOND','AGAIN',
	5 'NOOBJ','*BUG','*GRIPE','COMPLAIN',
	6 '*FEATURE','*COMMENT','*IDEA','SUGGESTI',
	7 'ROOM','*OBJECTS','OBJ','RNAME','DEFLATE',
	8 '*EXAMINE','*WHAT','DESCRIBE','FILL',
	9 '*FIND','*SEEK','*WHERE','SEE'/
C
	DATA (VVOC(Idontuse),Idontuse=55,120) /
	1 1,52,1,53,
	1 1,54,1,55,
	2 1,169,1,149,1,150,1,90,
	3 1,94,1,105,1,133,
	4 1,128,1,95,1,96,1,57,
	5 1,58,1,59,
	6 1,60,
	7 1,65,1,66,1,67,1,'50147'O,
	8 4,'40170'O,'60000'O,-1,-1,
	8 11,'60206'O,'61000'O,'200'O,0,'61002'O,-1,-1,
	8	'40206'O,'61000'O,'200'O,0,
	9 4,'40177'O,'60000'O,-1,-1/
C
	DATA (VWORD(Idontuse),Idontuse=87,131) /
	1 'FOLLOW','*KICK','*BITE','TAUNT',
	1 'LOWER','*PUSH','PRESS','*RING',
	2 'PEAL','*RUB','*FEEL','*CARESS','*TOUCH',
	3 'FONDLE','SHAKE','SPIN','*UNTIE',
	4 'FREE','*WALK','*RUN','*PROCEED','GO','*ATTACK','*FIGHT',
	5 '*INJURE','*HIT','HURT','BOARD',
	6 '*BRUSH','CLEAN','*BURN','*IGNITE',
	7 'INCINERA','CLIMB','CLOSE','DIG',
	8 'DISEMBAR','*DRINK','*IMBIBE','SWALLOW',
	9 '*DROP','RELEASE','*EAT','*GOBBLE','*CONSUME'/
C
	DATA (VVOC(Idontuse),Idontuse=121,278) /
	1 2,'125'O,'50125'O,1,'50153'O,
	1 1,'50156'O,9,'50160'O,'40160'O,'61012'O,-1,-1,
	1	'40241'O,'61010'O,-1,-1,
	2 5,'52127'O,'70127'O,'61002'O,-1,-1,
	3 1,'50157'O,1,'50171'O,1,'50201'O,
	4 11,'42161'O,'61000'O,0,'10000'O,
	4	'60242'O,'61000'O,0,'10000'O,'61015'O,-1,-1,
	4 9,'50216'O,'40126'O,'61016'O,-1,-1,'40126'O,'61005'O,-1,-1,
	5 7,'60215'O,'21000'O,0,'200'O,'44002'O,0,'1000'O,
	5 4,'40202'O,'21000'O,0,2,
	6 5,'52130'O,'70130'O,'61002'O,-1,-1,
	7 7,'60211'O,'61000'O,'20'O,0,'64002'O,'10'O,0,
	7 12,'40235'O,'20007'O,0,'4000'O,'40236'O,'20006'O,0,'4000'O,
	7	'40234'O,'20000'O,0,'4000'O,
	7 4,'40176'O,'61000'O,'10200'O,0,
	7 21,'60131'O,'20005'O,0,'40000'O,'44002'O,4,0,
	7 	'60131'O,'20016'O,0,'40000'O,'44002'O,4,0,
	7 	'60131'O,'20000'O,0,'40000'O,'44002'O,4,0,
	8 8,'40203'O,'20000'O,0,2,'40203'O,'20015'O,0,2,
	8 4,'40210'O,'61000'O,'400'O,0,
	9 25,'42221'O,'41000'O,-1,-1,
	9	'60220'O,'41000'O,-1,-1,'61005'O,-1,-1,
	9	'60220'O,'41000'O,-1,-1,'61006'O,-1,-1,
	9	'60220'O,'41000'O,-1,-1,'61016'O,-1,-1/
C
	DATA (VWORD(Idontuse),Idontuse=132,172) /
	1 '*MUNCH','TASTE','*DOUSE','EXTINGUI',
	1 '*GIVE','*HAND','DONATE','*HELLO',
	2 'HI','BLOW','INFLATE','*JUMP',
	3 'LEAP','*KILL','*MURDER','*SLAY',
	4 '*STAB','DISPATCH','*KNOCK','RAP',
	5 'LIGHT','LOCK','*LOOK','*L','*STARE',
	6 'GAZE','*MELT','LIQUIFY','MOVE',
	7 '*PULL','TUG','*DESTROY','*MUNG',
	8 '*BREAK','DAMAGE','OPEN','PICK',
	9 '*PLUG','*GLUE','PATCH','*POKE'/
C
	DATA (VVOC(Idontuse),Idontuse=279,450) /
	1 4,'40207'O,'75000'O,'2000'O,0,
	1 4,'40174'O,'75000'O,'100'O,0,
	1 11,'72222'O,'21004'O,'40'O,0,'64222'O,'21000'O,'40'O,0,
	1	'61000'O,-1,-1,
	2 2,'2227'O,'50227'O,
	2 15,'62146'O,'61007'O,-1,-1,'61002'O,4,0,
	2	'40122'O,'61007'O,-1,-1,'40165'O,'61005'O,-1,-1,
	2 4,'70146'O,'61002'O,4,0,
	3 5,'133'O,'40133'O,'61001'O,-1,-1,
	4 7,'60213'O,'21000'O,0,'200'O,'44002'O,0,'1000'O,
	4 12,'42166'O,'61003'O,-1,-1,'40166'O,'61012'O,-1,-1,
	4	'40215'O,'23006'O,'40'O,0,
	5 11,'42173'O,'75000'O,'100'O,0,'60211'O,'61000'O,'100'O,0,
	5	'54002'O,'10'O,0,
	5 7,'60134'O,'20000'O,-1,-1,'74002'O,4,0,
	6 31,'167'O,'40170'O,'60003'O,-1,-1,'40231'O,'61010'O,-1,-1,
	6	'40230'O,'60005'O,-1,-1,'40230'O,'60016'O,-1,-1,
	6	'60144'O,'60003'O,-1,-1,'61002'O,-1,-1,
	6	'60144'O,'60003'O,-1,-1,'61016'O,-1,-1,
	6 4,'70145'O,'61002'O,'10'O,0,
	6 4,'40172'O,'20000'O,-1,-1,
	7 8,'42172'O,'21000'O,-1,-1,'40172'O,'21012'O,-1,-1,
	8 5,'52212'O,'70212'O,'44002'O,-1,-1,
	8 11,'42175'O,'61000'O,'10200'O,0,'60175'O,'61000'O,'10200'O,0,
	8	'54002'O,4,'1000'O,
	8 4,'40204'O,'61007'O,'20000'O,'40'O,
	9 4,'70152'O,'61002'O,-1,-1/
C
	DATA (VWORD(Idontuse),Idontuse=173,212) /
	1 '*BLIND','JAB','*POUR','SPILL',
	1 'PUMP','*PUT','*INSERT','*STUFF',
	2 'PLACE','*RAISE','LIFT','*READ',
	3 '*PERUSE','SKIM','STRIKE','*SWING',
	4 'THRUST','*TAKE','*HOLD','*CARRY',
	5 'REMOVE','*TELL','*COMMAND','REQUEST',
	6 '*THROW','*HURL','CHUCK','*TIE',
	7 'FASTEN','*TURN','SET','UNLOCK',
	8 '*WAKE','*ALARM','*STARTLE','SURPRISE',
	9 '*WAVE','*FLAUNT','BRANDISH','WIND'/
C
	DATA (VVOC(Idontuse),Idontuse=451,654) /
	1 7,'60212'O,'21000'O,0,'200'O,'44002'O,0,'1000'O,
	1 25,'42223'O,'41000'O,'400'O,0,
	1	'60223'O,'41000'O,'400'O,0,'61005'O,-1,-1,
	1	'60223'O,'41000'O,'400'O,0,'61016'O,-1,-1,
	1	'60240'O,'41000'O,'400'O,0,'61012'O,-1,-1,
	1 4,'40232'O,'60007'O,-1,-1,
	2 16,'72220'O,'61005'O,-1,-1,'70220'O,'61016'O,-1,-1,
	2	'40221'O,'61006'O,-1,-1,'70241'O,'61010'O,-1,-1,
	2 5,'52155'O,'40155'O,'61007'O,-1,-1,
	3 18,'42144'O,'71000'O,'40000'O,0,
	3	'60144'O,'71000'O,'40000'O,0,'61002'O,-1,-1,
	3	'60144'O,'71000'O,'40000'O,0,'61016'O,-1,-1,
	3 12,'60215'O,'23000'O,'40'O,0,'44002'O,0,'1000'O,
	3	'42215'O,'23000'O,'40'O,0,'50173'O,
	4 7,'60214'O,'44000'O,0,'1000'O,'21003'O,0,'200'O,
	5 11,'42204'O,'61000'O,'20000'O,'40'O,
	5	'60204'O,'61000'O,'20000'O,0,'61015'O,-1,-1,
	5 4,'40217'O,'20000'O,0,'2000'O,
	6 21,'62224'O,'44000'O,-1,-1,'21003'O,'40'O,0,
	6	'60224'O,'44000'O,-1,-1,'21016'O,'40'O,0,
	6	'60220'O,'44000'O,-1,-1,'61005'O,-1,-1,
	7 11,'70162'O,'61004'O,-1,-1,'60163'O,'21007'O,'40'O,0,
	7	'65002'O,4,0,
	7 22,'62164'O,'61000'O,2,0,'64002'O,4,0,
	7	'40173'O,'75012'O,'100'O,0,'40174'O,'75013'O,'100'O,0,
	7	'60237'O,'61000'O,2,0,'20004'O,-1,-1,
	7 7,'60135'O,'21000'O,-1,-1,'74002'O,4,0,
	8 8,'42150'O,'20000'O,'40'O,0,'40150'O,'20007'O,'40'O,0,
	9 4,'40154'O,'40000'O,-1,-1,
	9 5,'50233'O,'40233'O,'61007'O,-1,-1/
C
	DATA (VWORD(Idontuse),Idontuse=213,240)/
	1 'ENTER','LEAVE','*MAKE','BUILD',
	1 '*OIL','*GREASE','LUBRICAT','PLAY',
	2 'SEND','SLIDE','*SMELL','SNIFF',
	3 'SQUEEZE','GET','COUNT',13*' '/
C
	DATA (VVOC(Idontuse),Idontuse=655,722) /
	1 2,167,'50126'O,2,168,'50220'O,1,'50243'O,
	1 4,'70244'O,'41002'O,-1,-1,
	1 5,'50245'O,'70245'O,'75002'O,4,0,
	2 4,'40246'O,'61014'O,-1,-1,
	2 4,'70241'O,'61010'O,-1,-1,1,'50105'O,
	3 1,'50104'O,19,'42204'O,'61000'O,'20000'O,'40'O,
	3	'40202'O,'21005'O,0,2,'40203'O,'21015'O,0,2,
	3	'60204'O,'61000'O,'20000'O,'40'O,'61015'O,-1,-1,
	3 1,'50141'O,13*0/

! hack to avoid refactoring code: moved from parser.f--variables not used anywhere else.
      	EQUIVALENCE (OBJVEC(1),OBJ1),(PRPVEC(1),PREP1)
! hack for gdt.f
 !       EQUIVALENCE (EQR(1,1),RDESC1(1))
 !       EQUIVALENCE (EQO(1,1),ODESC1(1))
  !      EQUIVALENCE (EQC(1,1),CTICK(1))
   !     EQUIVALENCE (EQV(1,1),VILLNS(1))
  !      EQUIVALENCE (EQA(1,1),AROOM(1))

      end module
