LRHYBC1 ;DALOI/HOAK - LAB PHLEB AND COLLECTION TIME UPDATER ;11/8/10 1:50pm
 ;;5.2;LAB SERVICE;**405**;Sep 27, 1994;Build 93
 ;
 ; This routine will be used to capture the phlebotomist and the
 ; specimen collection time.
 ;
 ; The barcoded specimen tubes will be waunded.
 ; The phlebotomist ID will then be waunded.
 ;
CONTROL ;
 K LRPTARIV
 K ^TMP("LRHY ASH",$J)
 K LRSCAN
 K DIC,LRHYTECH,LRHYDUZ,LRPHLEB,LRRECVR
 K DIR,DIC,DIE,LRARIVE,LRDRAW
 K DIC,LRHYTECH,LRHYDUZ,LRPHLEB
 K DIR,DIC,DIE,LRARIVE,LRDRAW
 S LRPL=1
 S LREND=0
 D TECH
 I U[X D END QUIT
 Q:X=""  D SINGLE
 D PTSCAN
 Q:X[U
 K LRPL
 K LRPTARIV
 QUIT
FINDER ;
 S DIC="^VA(200,"
 S DIC(0)="AEMQZ"
 S DIC("A")="Please enter employee number: "
 D ^DIC
 QUIT
TECH ;
 K DIC,LRHYTECH,LRHYDUZ,LRPHLEB,LRRECVR
 K DIR,DIC,DIE,LRARIVE,LRDRAW
 ;
 S X=""
 ;
 I X="" S X=DUZ G PST
 I U[X QUIT
 I $L(X)'=9 K DIC,LRHYTECH,LRHYDUZ,LRPHLEB,LRRECV G TECH
 ;
 ;
 ;
 ;
 K DIC
 K DIC,LRHYTECH,LRHYDUZ
 K Y
 S DIC=200
 S DIC(0)="MQZ"
 D ^DIC
 ;
 W Y
 ;
 I X[U QUIT
PST S Y=DUZ
 S (LRHYDUZ,LRHYTECH,LRPHLEB,LRRECVR)=+Y
 S LRHYDUZ=$P($G(^VA(200,LRHYDUZ,0)),U)
 QUIT
 ;
TIME ;
 ;
 ;
 ;
 S LREND=0
 S DIC="^DPT("
 S DIC(0)="AEMQZ"
 D ^DIC
 S DFN=+Y
 S LRDFN=$G(^DPT(DFN,"LR"))
 D ^VADPT,INP^VADPT
 ;
 QUIT
 ;
SINGLE ;
 ; This block calls up the testing demographics.
 ;  LRHYD123 IS LRUID
 W !!
 S LRACC=""
 I $G(LRHYD123)'="" D EN^LA7ADL(LRHYD123)
 ;
 K LRHYD123
 ;
 K LRHN0,LRHNODE,LRN0,LR0NODE,LRPHLEB
 S X="D"
 D ^LRHYU5
 I LRAN<1 QUIT
SETFILE ;
 I '$D(^LRO(68,LRAA,1,LRAD,1,LRAN,0)) W !,"Doesn't exist." G SINGLE
 S LRUNC=1
 S LRORDT1=$P(^LRO(68,LRAA,1,LRAD,1,LRAN,0),U,4)
 S LRHYD123=$G(^LRO(68,LRAA,1,LRAD,1,LRAN,.3))
 ;  mdofied by Hoak per Joe for prior to free t-4
 I '$O(^LRHY(69.87,"B",LRHYD123,0)) D
 .  S DA=$P(^LRHY(69.87,0),U,3)
 .  S DA=DA+1
 .  S X=LRHYD123
 .  S DIE="^LRHY(69.87,"
 .  S DR=".01///"_X
 .  S DIK=DIE
 .  D ^DIE
 I '$G(DA) S DA=$P(^LRHY(69.87,0),U,3)+1
 S LRUID=LRHYD123
 S $P(^LRHY(69.87,0),U,3)=DA,$P(^LRHY(69.87,0),U,4)=DA
 S LRSPIEN=$O(^LRHY(69.87,"B",LRUID,0)) I $G(LRSPIEN) D
 .  D NOW^%DTC
 .  S LRDT=%
 .  I '$G(LRLABTIM) S LRLABTIM=+$G(^LRO(68,LRAA,1,LRAD,1,LRAN,3))
 .  S LRSCAN=LRLABTIM
 .  K DIE S DIE="^LRHY(69.87," S DR="2///"_LRSCAN S DA=LRSPIEN D ^DIE
 .  K DIE S DIE="^LRHY(69.87," S DR="4///"_LRDT S DA=LRSPIEN D ^DIE
 .  K DIE S DIE="^LRHY(69.87," S DR="6////"_LRHYTECH S DA=LRSPIEN D ^DIE
 .  K DIE S DIE="^LRHY(69.87," S DR="8///"_LRDT S DA=LRSPIEN D ^DIE
 S DIK=DIE D IX1^DIK
ZZ1 ;
 QUIT:$G(XQY0)'["PPOC"
 H 3
 S ^TMP("LRHY ASH",$J,LRAA,LRAD,LRAN)=""
 G SINGLE
 G END
 ;  Adding urgency to the display
 S LRTEST=$O(^LRO(68,LRAA,1,LRAD,1,LRAN,4,0))
 S LRURG=$P($G(^LRO(68,LRAA,1,LRAD,1,LRAN,4,LRTEST,0)),U,2)
 ;
 ; Blink urgency if MED-EMERGE
 W !,$S($D(^LAB(62.05,+LRURG,0)):$P(^(0),U),1:"")," "
 ;
 ;
 D EDIT
 ;
 ;
 I $G(LREND) W !,"Please start over..." K LREND,LRIDTNEW
 D END
 QUIT
 ;
END ;
 K ^TMP("LRHY ASH",$J)
 K LRAA,LRAD,LRAN,LRBLOOD,LRARIVE,LRBLOOD,LRCE,LRDAT,LRDFN,LRIDT
 K LRDLA,LRDLC,LRDPF,LRDRAW,LRDT0,LRDTO,LRHYDUZ,LRHYNISH,LRPRAC
 K LRSN,LRTEST,LRURG,PNM,SSN,VAIN,VADM,LRHYTECH
 QUIT
 ;
EDIT ;
 S LRDFN=$P(^LRO(68,LRAA,1,LRAD,1,LRAN,0),U)
 S LRIDT=$P(^LRO(68,LRAA,1,LRAD,1,LRAN,3),U,5) ; old LRIDT
CHECK ;
 ;
PAST ;
 K LRHYDUZ
 ; look at Micro And CH subscripted tests
 I $D(^LR(LRDFN,"CH",LRIDT)) S LRSS="CH"
 E  S LRSS="MI"
 ;
 ;  run review option here and show it there has been a previous 
 ;  updating of this accession
 ;
 K LRARIVE
 ;
 ;
 W !
 ;  WE need an event to stop non-LRLAB key holders from entering
 ;  LAB ARRIVAL TIME
BACK G:'$D(^XUSEC("LRLAB",DUZ)) THERE
 G THERE
THERE ;
 W !
 K %DT
 D NOW^%DTC
 S %DT="AESRZ"
 S %DT("A")="Please enter updated DRAW/COLLECTION TIME: "
 ;
 ; Only default if lrlab owner
 I +LRDRAW7>0 S LRDRAW7=$$Y2K^LRX(LRDRAW7)
 E  S LRDRAW7=$$Y2K^LRX($P(^LRO(68,LRAA,1,LRAD,1,LRAN,3),U))
 I $D(^XUSEC("LRLAB",DUZ)) S %DT("B")=$P(LRDRAW7,"@")_"@"_$E($P(LRDRAW7,"@",2),1,5)
 ;
 I X="" G THERE
 I $D(DTOUT)!($D(DUOUT)) S LREND=1 QUIT
 I Y=-1 S LRUP="YES"
 E  S LRUP="NO"
 D NOW^%DTC S LRDRAW=%
 ;
 ;
 I $D(^XUSEC("LRLAB",DUZ)) G TIC
 ;
GUY ;  COLLECTOR DEMOGRAPHICS stuff this into LR...99 COMMENT FIELD.
 K LRPON
 S LRHYNISH=$P(^VA(200,LRHYTECH,0),U,2)
 I '$D(^XUSEC("LRLAB",LRHYTECH)) S LRPON=$P($G(^VA(200,LRHYTECH,0)),U)
 W !!!
 ;
 ; This global serves as an interim solution until lab files can
 ; be updated
TIC ;
 S LRARIVE=$G(LRARIVE,$P(^LRO(68,LRAA,1,LRAD,1,LRAN,3),U,3))
 S LRPTARIV=$G(LRPTARIV,LRARIVE)
 ;
 S LRAAX5=LRAA,LRADX6=LRAD,LRANX6=LRAN
 S LRHYDUZ=LRHYTECH
SET ;
 S $P(^LRO(69,LRDAT,1,LRSN,3),U)=$G(LRDRAW)
 S $P(^LRO(68,LRAA,1,LRAD,1,LRAN,3),U,3)=$G(LRDRAW)
 S $P(^LRO(68,LRAA,1,LRAD,1,LRAN,3),U)=$G(LRDRAW)
 ;
 H 2
 G SINGLE
 QUIT
PTSCAN ;
 ;
 W !!,"Please swipe PATIENT ID CARD or Type SSN: "
 R X:9999999 W !
 Q:X[U
 Q:X=""
 I $L(X)'=9 W !,"NO SUCH PATIENT" QUIT
 S DFN=$O(^DPT("SSN",X,0))
 S PNM=$P(^DPT(DFN,0),U) W !,PNM
 S LRDFN=$G(^DPT(DFN,"LR"))
ACCNX ;
 S LRAA=0
 F  S LRAA=$O(^TMP("LRHY ASH",$J,LRAA)) Q:+LRAA'>0  D
 .  S LRAD=0
 .  F  S LRAD=$O(^TMP("LRHY ASH",$J,LRAA,LRAD)) Q:+LRAD'>0  D
 ..  S LRAN=0
 ..  F  S LRAN=$O(^TMP("LRHY ASH",$J,LRAA,LRAD,LRAN)) Q:+LRAN'>0  D P2
 QUIT
P2 ;
 N LRX S LRX=0
 F  S LRX=$O(^LRO(68,LRAA,1,LRAD,1,LRAN,4,"B",LRX)) Q:+LRX'>0  D
 .  I LRDFN'=+^LRO(68,LRAA,1,LRAD,1,LRAN,0) W !,"WRONG PATIENT" QUIT
 .  W !,^LRO(68,LRAA,1,LRAD,1,LRAN,.2),?20,$P(^LAB(60,LRX,0),U)
 .  W !,"EVERYTHING MATCHES UP ",$P($P(^VA(200,LRHYTECH,0),U),",",2),", GREAT JOB!"
 .  H 3
 QUIT
LABIN ;
 D ^LRHYU4
 ;
 Q:LRAN=-1
 ;
 I $L(X)'=10 S X=$G(^LRO(68,LRAA,1,LRAD,1,LRAN,.3))
 S LRUID=X
 S DA=$O(^LRHY(69.87,"B",LRUID,0))
 I '$G(DA) W !,"Incorrect UID try again..." H 2 G LABIN
 D NOW^%DTC K LRLABIN S LRLABIN=%
 K DIE
 S DIE=69.87
 S DR="10///"_LRLABIN_";12///"_DUZ
 D ^DIE
 G LABIN
DISPLAY ;
 S X="D"
 D ^LRHYU5 I 'LRAN W !,"MUST ENTER UID, TRY AGAIN" H 2 QUIT
 S LRUID=X
 S ZTRTN="D1^LRHYBC1" D IO^LRWU
 QUIT
D1 ;
 I $L(LRUID)'=10 S LRUID=X
 S LRDA=$O(^LRHY(69.87,"B",LRUID,0))
 ;
 ; FIX FOR NOT RUN PL
 I +$G(LRDA)'>0 W !,"NO Entry in HOWDY SPECIMEN TIMES BY UID File. Run Phlebotomy log." H 1 QUIT
 I '$G(^LRHY(69.87,LRDA,2)) D
 .  K DA,DR S DIE="^LRHY(69.87,",DA=LRDA,DR="2///"_$P(^LRO(68,LRAA,1,LRAD,1,LRAN,3),U) D ^DIE
 W !,"UID: ",LRUID
 W !,"WALK-UP SCAN TIME:",?50,$$Y2K^LRX(^LRHY(69.87,LRDA,2))
 W !,"TIME LABELS PRINTED:",?50,$$Y2K^LRX(^LRHY(69.87,LRDA,4))
 W !,"COLLECTOR:",?50,?50,$P(^VA(200,^LRHY(69.87,LRDA,6),0),U)
 W !,"TIME SPECIMEN COLLECTED:",?50,$$Y2K^LRX(^LRHY(69.87,LRDA,8))
 Q:'$D(^LRHY(69.87,LRDA,10))
 W !,"TIME SCANNED INTO LAB:",?50,$$Y2K^LRX(^LRHY(69.87,LRDA,10))
 I $G(^LRHY(69.87,LRDA,12)) W !,"RECEIVED INTO LAB BY: ",?50,$P(^VA(200,^LRHY(69.87,LRDA,12),0),U)
 QUIT
BINBRD ;
 S ZTRTN="D2^LRHYBC1",ZTSAVE("PNM")=""
 S LRBBRD=$O(^LRHY(69.86,7,54,"B",0)) I $G(LRBBRD) S ZTIO=LRBBRD,ZTDTH=$H S:$D(ZTQUEUED) ZTREQ="@" D ^%ZTLOAD
 QUIT
D2 ;
 U IO
 W !,"PT:",PNM
 D ^%ZISC
 QUIT
