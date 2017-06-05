module state
    implicit none


! Syntax definitions

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

! Clock event indices

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


! Actor indices

integer,parameter :: PLAYER=1                 ! player
integer,parameter :: AROBOT=2                 ! robot
integer,parameter :: AMASTR=3                 ! dungeon master

! Actor flags

integer,parameter :: ASTAG=32768                 ! staggered

! Room flags

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

! Room indices

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

! Verb indices

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

! Object flags

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

! Object indices

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

! Misc definitions

integer,parameter :: HFACTR=500

! Exit definitions

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

! Array size parameters

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

! Other parameters

integer,parameter :: RECLNT=80                  ! DTEXT.DAT record size, bytes
integer,parameter :: TEXLNT=76                  ! text buffer size, char
integer,parameter :: WRDLNT=8                  ! word length size, char
integer,parameter :: BUNMAX=10                  ! bunched objects
integer,parameter :: LEXMAX=20                  ! lexical tokens

! Game state

LOGICAL TELFLG
integer WINNER,HERE, MOVES,DEATHS,RWSCOR,MXSCOR,MXLOAD, LTSHFT,BLOC,MUNGRM,HS,EGSCOR,EGMXSC

LOGICAL TROLLF,CAGESF,BUCKTF,CAROFF,CAROZF,LWTIDF, & 
DOMEF,GLACRF,ECHOF,RIDDLF,LLDF,CYCLOF, MAGICF,LITLDF,SAFEF,GNOMEF,GNODRF,MIRRMF,&
EGYPTF,ONPOLF,BLABF,BRIEFF,SUPERF,BUOYF, GRUNLF,GATEF,RAINBF,CAGETF,EMPTHF,DEFLAF,& 
GLACMF,FROBZF,ENDGMF,BADLKF,THFENF,SINGSF, MRPSHF,MROPNF,WDOPNF,MR1F,MR2F,INQSTF,& 
FOLLWF,SPELLF,CPOUTF,CPUSHF, DEADF,ZGNOMF,MATF,PLOOKF,PTOUCF, BROC1F,BROC2F,EXORBF,EXORCF,PUNLKF

! Parser state
CHARACTER(WRDLNT) ONAME
integer OFLAG,OACT,OPREP1,OOBJ1,OPREP,OPREP2,OOBJ2, LASTIT,ACT,OBJ1,OBJ2,PREP1,PREP2, &
VFLAG,DOBJ,DFL1,DFL2,DFW1,DFW2, IOBJ,IFL1,IFL2,IFW1,IFW2, BUNLNT,BUNSUB,BUNVEC(BUNMAX)


! Parser output
LOGICAL PRSWON
integer  PRSA,PRSI,PRSO,PRSCON

! Miscellaneous variables

integer INLNT,SUBLNT
CHARACTER(TEXLNT) INBUF,SUBBUF
CHARACTER(1) VEDIT

integer MBASE,STRBIT, PLTIME,DARRAY(3),SHOUR,SMIN, BATDRP(9),TMARRAY(3), INPCH,OUTCH,DBCH, DBGFLG,PRSFLG,GDTFLG,VMAJ,VMIN

! Rooms

integer  RLNT,RDESC2,RDESC1(RMAX),REXIT(RMAX), RACTIO(RMAX),RVAL(RMAX),RFLAG(RMAX)

! Adventurers
integer ALNT,AROOM(AMAX),ASCORE(AMAX),AVEHIC(AMAX), AOBJ(AMAX),AACTIO(AMAX),ASTREN(AMAX),AFLAG(AMAX)

! Clock interrupts
LOGICAL CFLAG(CMAX),CCNCEL(CMAX)
integer CLNT,CTICK(CMAX),CACTIO(CMAX)

! Objects

integer OLNT,ODESC1(OMAX),ODESC2(OMAX),ODESCO(OMAX), OACTIO(OMAX),OFLAG1(OMAX),OFLAG2(OMAX),OFVAL(OMAX), &
 OTVAL(OMAX),OSIZE(OMAX),OCAPAC(OMAX),OROOM(OMAX), OADV(OMAX),OCAN(OMAX),OREAD(OMAX)

integer  R2LNT,O2(R2MAX),R2(R2MAX)

! Prepositions--      maps prepositions to indices

character,parameter :: PWORD(*)=[character(WRDLNT):: 'OVER','WITH', & 
'USING', 'AT','TO', 'IN','INSIDE','INTO','DOWN','UP', 'UNDER', 'OF','ON','OFF', 'FOR', 'FROM','OUT','THROUGH',' ',' ']

integer, parameter :: PVOC(*) = [1,2,2,3,4,5,5,5,6,7,8,9,10,11,12,13,13,14,0,0]


character,parameter :: VWORD(*)=[character(wrdlnt)::& 
'BRIEF','VERBOSE','SUPERBRI','STAY','VERSION','*SWIM','*BATHE','WADE', & 
'GERONIMO','*ULYSSES','ODYSSEUS','*PLUGH','XYZZY', 'PRAY','TREASURE','TEMPLE','BLAST', &
'SCORE','*QUIT','*GOODBYE','*Q','BYE','HELP', 'INFO','*HISTORY','UPDATE','BACK', &
'*MUMBLE','SIGH','*CHOMP','*LOSE', 'BARF','DUNGEON','FROBOZZ','*FOO', '*BLETCH','BAR','REPENT','*HOURS', &
'SCHEDULE','WIN','*YELL','*SCREAM', 'SHOUT','*HOP','SKIP','*CURSE', '*SHIT','*DAMN','FUCK','ZORK', &
'WISH','SAVE','RESTORE','TIME', 'DIAGNOSE','EXORCISE','*LIST','*I','INVENTOR', 'WAIT','INCANT','*ANSWER','RESPOND','AGAIN', &
'NOOBJ','*BUG','*GRIPE','COMPLAIN','*FEATURE','*COMMENT','*IDEA','SUGGESTI', 'ROOM','*OBJECTS','OBJ','RNAME','DEFLATE', &
'*EXAMINE','*WHAT','DESCRIBE','FILL','*FIND','*SEEK','*WHERE','SEE', 'FOLLOW','*KICK','*BITE','TAUNT', &
'LOWER','*PUSH','PRESS','*RING', 'PEAL','*RUB','*FEEL','*CARESS','*TOUCH', 'FONDLE','SHAKE','SPIN','*UNTIE', &
'FREE','*WALK','*RUN','*PROCEED','GO','*ATTACK','*FIGHT', '*INJURE','*HIT','HURT','BOARD', &
'*BRUSH','CLEAN','*BURN','*IGNITE','INCINERA','CLIMB','CLOSE','DIG', 'DISEMBAR','*DRINK','*IMBIBE','SWALLOW', &
'*DROP','RELEASE','*EAT','*GOBBLE','*CONSUME', '*MUNCH','TASTE','*DOUSE','EXTINGUI', '*GIVE','*HAND','DONATE','*HELLO', &
'HI','BLOW','INFLATE','*JUMP','LEAP','*KILL','*MURDER','*SLAY', '*STAB','DISPATCH','*KNOCK','RAP', &
'LIGHT','LOCK','*LOOK','*L','*STARE','GAZE','*MELT','LIQUIFY','MOVE', '*PULL','TUG','*DESTROY','*MUNG',&
'*BREAK','DAMAGE','OPEN','PICK','*PLUG','*GLUE','PATCH','*POKE','*BLIND','JAB','*POUR','SPILL', 'PUMP','*PUT','*INSERT','*STUFF',&
'PLACE','*RAISE','LIFT','*READ','*PERUSE','SKIM','STRIKE','*SWING', 'THRUST','*TAKE','*HOLD','*CARRY',&
'REMOVE','*TELL','*COMMAND','REQUEST', '*THROW','*HURL','CHUCK','*TIE', 'FASTEN','*TURN','SET','UNLOCK',&
'*WAKE','*ALARM','*STARTLE','SURPRISE','*WAVE','*FLAUNT','BRANDISH','WIND', 'ENTER','LEAVE','*MAKE','BUILD', &
'*OIL','*GREASE','LUBRICAT','PLAY','SEND','SLIDE','*SMELL','SNIFF','SQUEEZE','GET','COUNT',&
' ',' ',' ',' ',' ',' ',' ',' ',' ',' ', ' ',' ',' ']

contains

end module
