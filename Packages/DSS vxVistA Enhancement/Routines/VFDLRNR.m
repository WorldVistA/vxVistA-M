VFDLRNR ;DSS/FHS - EDIT MULTI NORMAL RANGE #21660
 ;;2013.1;VENDOR - DOCUMENT STORAGE SYS;;05 May 2014;Build 3
 ;DSS/FHS - Dec 10, 2014
 Q
EN ;Entry point to enter or edit 21660 [Multi-normal Range file]
 I '$D(^XUSEC("LRLIASON",+$G(DUZ))) D  Q
 . D EN^DDIOL("You do not have the proper Security Key  *** Access denied","","!!")
 K DIC,DIE,DIE("NO^"),X,Y,DR,DA
 S DIC="^VFDLR(21660,",DIE=DIC
 S DIC(0)="AEQNML"
 S DIC("DR")="[VFDLR MULTI-NORMAL RANGE]",DR=DIC("DR")
 D ^DIC Q:$G(Y)<1
 I '$P($G(Y),U,3) D
 . K DIE("NO^")
 . S DIE("NO^")="Other value"
 . S DA=+Y D ^DIE
 Q
TEST ; Entry point to test file configuration
 ;Used to verify data entry
 I '$D(^XUSEC("LRLIASON",DUZ)) D  Q
 . W !!,"You do not have the proper security Keys",!
 N A,CHK,SAVESEX,I,J,X,Y
 N AGE,AGETYP,NODE,D0,DA,DIR,DIC,DIE,LRDUZ,LRNG
 N DIRUT,DUOUT
 N LRTST,LRTSTN,LRVVRFY,LRVVIEN,LRDUZ
 N STR,SEX,SEXY,LRSPEC,LRTST,LRNGX,S
 S LRVVRFY=1,LRVVIEN=1 ; Testing flag when calling RANGE^VFDLRX(TST,SPEC,LRNG)
 S DIR(0)="P^60:AQENM",DIR("S")="I $D(^VFDLR(21660,""AC"",+Y))"
 D ^DIR
 I $S($G(DUOUT):1,$G(DIRUT):1,1:0) W !,"No test selected",! Q
 S LRTST(+Y)=$P(Y,U,2),LRTST=+Y
 K DIR S DIR(0)="P^61:AQENM",DIR("S")="I $D(^VFDLR(21660,""AC"",LRTST,+Y))"
 D ^DIR
 I $S($G(DUOUT):1,$G(DIRUT):1,1:0) W !,"No specimen selected",! Q
 S LRSPEC(+Y)=$P(Y,U,2),LRSPEC=+Y
 K DIR S DIR(0)="21660,3" D ^DIR
 I $S($G(DUOUT):1,$G(DIRUT):1,1:0) W !,"No Sex selected",! Q
 S (SEXY,SEX)=Y
 K DIR S DIR(0)="21660,4" D ^DIR
 I $S($G(DUOUT):1,$G(DIRUT):1,1:0) W !,"No age type selected",! Q
 S AGETYP=Y(0)
 K DIR S DIR(0)="N^1:150" D ^DIR
 I $S($G(DUOUT):1,$G(DIRUT):1,1:0) W !,"NO patient age entered",! Q
 S AGE(2)=Y_AGETYP
 K DIR S DIR(0)="PO^4:EMQ" D ^DIR
 I $S($G(DUOUT):1,$G(DIRUT):1,1:0) W !,"No Institution selected",!
 S:+Y>0 LRDUZ(2)=+Y
 K DIR
 S LRNG=LRTST(LRTST)_"^^^^^^"
 D RANGE^VFDLRX(LRTST,LRSPEC)
 D DISPLAY
 Q
DISPLAY ;Print the results of lookup
 N DIC,DIE,DA,DR,S
 S STR=LRTST(LRTST)_" - "_LRSPEC(LRSPEC)_" - "_AGE(2)_" - "_SEXY
 I $G(LRDUZ) S STR=STR_" : "_$P(^DIC(4,LRDUZ,0),U)
 I '$G(LRVVIEN) D  Q
 . W !!,"* * * * No Match found for: "_STR,!
 W !!,"Results for: ",STR,!
 W "  Normal low: ",$P(LRNG,U,2),?22," Normal high: ",$P(LRNG,U,3),!
 W "Critical Low: ",$P(LRNG,U,4),?22,"Critcial high: ",$p(LRNG,U,5),!
 S DIC="^VFDLR(21660,",DA=LRVVIEN,S=0,DR="0:99"
 W !,"Source [IEN]="_DA
 D EN^DIQ
 Q
CUM1 ;Collect Cum header
 K ^XTMP("VFDLR","CUM"),LRX,LRXSPEC,LRMH,LRIEN,VAL,LRXSB
 S LRX=0
 S LRX=$O(^LAB(64.5,1,1,LRX)) Q:LRX<1  W !,LRX," = ",^(LRX,0) D
 . S LRXSPEC=+$P(^LAB(64.5,1,1,LRX,1,1,0),U,2) Q:'LRXSPEC
 . S LRMH=0 S LRMH=$O(^LAB(64.5,1,1,LRX,1,1,LRMH)) Q:LRMH<1  W !,LRMH," = ",^(LRMH,0) D
 . . S LRIEN=0 F  S LRIEN=$O(^LAB(64.5,1,1,LRX,1,1,LRMH,LRIEN)) Q:LRIEN<1  W !,^(LRIEN,0) D
 . . . S VAL=+$P(^(0),";",2) Q:'VAL
 . . . S LRXSB(LRXSPEC,VAL)="",LRXSB(LRXSPEC,VAL,"B",LRX,LRMH,LRIEN)=""
 Q
CUM2 ;
 I $D(LRXSB(LRXSPEC,VAL)) D
 . S LRRNG=$P(^LR(LRDFN,LRSS,LRIDT,VAL),U,5)
 . S LRNLOW=$P(LRRNG,"!",2),LRNHIGH=$P(LRRNG,"!",3)
 . S LRXSB(LRXSPEC,VAL)=LRNLOW_U_LRNHIGH
 Q
LRDATA(LRDFN,LRSS,LRSPEC,LRTST) ; Find Normal Range from ^LR(LRDFN,LRSS,X,LRSB)
 ; Called form LRDIST1
 ; LRTST=Pointer to ^LAB(60,LRTST
 ; Returns X
 N LRDATA,LRSB
 S X="",LRSB=+$P(^LAB(60,LRTST,0),";",2) Q:'LRSB
 S:'$G(LRIDT) LRIDT=0
 S LRDATA=0 F  S LRIDT=$O(^LR(LRDFN,LRSS,LRIDT)) Q:LRIDT<1!($G(LRDATA))  D
 . Q:'$P(^LR(LRDFN,LRSS,LRIDT,0),U,3)
 . Q:$P(^LR(LRDFN,LRSS,LRIDT,0),U,5)'=LRSPEC
 . Q:'$D(^LR(LRDFN,LRSS,LRIDT,LRSB))
 . S LRDATA=$P(^LR(LRDFN,LRSS,LRIDT,LRSB),U,5)
 . S X=$TR(LRDATA,"!",U)
 W:$G(LRDBUG) !,"X= ",X
 Q
TSTRES(LRSB,LRSPEC) ;Pull normal range from ^XTMP("LRNR","U",$J,LRSB)
 K VFDLRNR,VFDLO,VFDHI,VFDSTR
 N LRIDT,LRY,LRSS
 S VFDTST=0,LRSS="CH"
 I '$D(^XTMP("LRNR","U",$J,LRSB,LRSPEC)) Q 0
 S LRIDT=$P(^XTMP("LRNR","U",$J,LRSB,LRSPEC),"~",2)
 S LRY=$$TSTRES^LRRPU(LRDFN,LRSS,LRIDT,LRSB)
 S (LRLO,VFDLO)=$P(LRY,U,3),(LRHI,VFDHI)=$P(LRY,U,4)
 S VFDTST=1
 Q VFDLO_" - "_VFDHI
 ;
TESTCUM ;LR70SUM CUM TEST DATA
 ;EN^LR7OSUM(Y,DFN,SDATE,EDATE,COUNT,LRGIOM,SUBHEAD)
 ; Report stored ^TMP("LRNR",$J,"O"
 ; ENTRY POINT = VFDLR^LR7OSUM
 S Y="^TMP(""LRNRX"","_$J_",""O"")",DFN=2,EDATE=3141102,LRGIOM="",SDATE=3141203.2359
 S SUBHEAD="",COUNT=""
 S VFDIEN=$G(^XTMP("LRNR","I",0))+1,^XTMP("LRNR","I",0)=VFDIEN
 Q
