APCDALV1 ; IHS/CMI/TUCSON - VISIT CREATION CONT. ; [ 11/24/03  2:15 PM ]
 ;;2.0;IHS RPMS/PCC Data Entry;**3,7**;MAR 09, 1999
 ;IHS/CMI/LAB - added display of VCN if it exists
 ;
INIT ;EP;INITIALIZATION/EDIT INPUT VARIABLES
 K APCDAFLG,APCDALVR("APCDAFLG"),APCDVSIT("NEW"),APCDALVR("APCDVSIT","NEW")
 I $D(APCDALVR)\10 S APCDAX="" F APCDAL=0:0 S APCDAX=$O(APCDALVR(APCDAX)) Q:APCDAX=""  S @APCDAX=APCDALVR(APCDAX)
 S U="^",APCDVSIT=""
 S:$D(ZTQUEUED) APCDAUTO="" ; default to auto mode if in background
 D EDIT
 Q:$D(APCDAFLG)
 Q
 ;
EDIT ; EDIT PASSED VARIABLES
 I $D(APCDADF),APCDADF=+APCDADF,APCDADF>0,APCDADF<4
 E  K APCDADF ;        kill it if it isn't right
 S:$P(APCDDATE,".",2)="" APCDDATE=+APCDDATE_".12"
 S APCDDATE=$E(APCDDATE,1,12)
 S:'$D(APCDTYPE) APCDTYPE="I"
 S:'$D(APCDCAT) APCDCAT="A"
 S:$E(APCDPAT)="`" APCDPAT=$E(APCDPAT,2,99)
 I '$D(^AUPNPAT(APCDPAT,0)) S APCDAFLG=3,APCDAFLG("ERR")=".05^"_APCDPAT_"^PATIENT NOT IN AUPNPAT GLOBAL" Q
 S:$E(APCDLOC)="`" APCDLOC=$E(APCDLOC,2,99)
 I '$D(^AUTTLOC(APCDLOC,0)) S APCDAFLG=3,APCDAFLG("ERR")=".06^"_APCDLOC_"^LOCATION PTR NOT IN AUTTLOC" Q
 I $D(APCDOLOC),APCDOLOC?.E1C.E S APCDAFLG=3,APCDAFLG("ERR")="2101^"_APCDOLOC_"^OUTSIDE LOCATION FAILED INPUT TX" Q
 I $G(APCDOLOC)]"",$L(APCDOLOC)<2!($L(APCDOLOC)>50) S APCDAFLG=3,APCDAFLG("ERR")="2101^"_APCDOLOC_"^OUTSIDE LOCATION FAILED INPUT TX" Q
 I $D(APCDCLN),APCDCLN="" K APCDCLN Q
 Q:'$D(APCDCLN)
 S:$E(APCDCLN)="`" APCDCLN=$E(APCDCLN,2,99)
 I APCDCLN?1N.N,'$D(^DIC(40.7,APCDCLN,0)) S APCDAFLG=3,APCDAFLG("ERR")=".08^"_APCDCLN_"^CLINIC NOT VALID" Q
 I APCDCLN'?1N.N S X=APCDCLN,DIC="^DIC(40.7,",DIC(0)="M" D ^DIC S:+Y>0 APCDCLN=+Y
 I APCDCLN'?1N.N S APCDAFLG=3,APCDAFLG("ERR")=".08^"_APCDCLN_"^CLINIC NOT VALID" Q
 I $D(APCDTBP) S X="`"_APCDTBP I '$D(X) S APCDAFLG=3,APCDAFLG("ERR")=".04^"_APCDTPB_"^.04 VALUE MUST BE IN INTERNAL FORMAT AND PASS INPUT TX" Q
12 ;
 I $D(APCDPVL),'$D(^AUPNVSIT(APCDPVL))!($P($G(^AUPNVSIT(APCDPVL,0)),U,11)) S APCDAFLG=3,APCDAFLG("ERR")=".12^"_APCDPVL_"^MUST BE INTERNAL VALUE AND VALID VISIT PTR" Q
16 ;
 I $G(APCDAPPT)]"" S %=$$EXTSET^XBFUNC(9000010,.16,APCDAPPT) I %="" S APCDAFLG=3,APCDAFLG("ERR")=".16^"_APCDAPPT_"^WALKIN / APPT FAILED INPUT TX" Q
17 ;
 I $G(APCDEVM)]"",'APCDEVM S APCDAFLG=3,APCDAFLG("ERR")=".17^"_APCDEVM_"^EVAL&MAN NOT VALID INTERNAL FORMAT" Q
 I $G(APCDEVM) S %=$P($G(^DD(9000010,.17,12.1)),"=",2) S X=$$FIND1^APCDDIC(81,APCDEVM,"I",%) I 'X S APCDAFLG=3,APCDAFLG("ERR")=".17^"_APCDEVM_"^EVAL&MAN FAILED INPUT TX" Q
18 ;
 I $G(APCDCODT)]"" S X=$$FMTE^XLFDT(APCDCODT) X $P(^DD(9000010,.18,0),U,5,99) I '$D(X) S APCDAFLG=3,APCDAFLG("ERR")=".18^"_APCDCODT_"^CHECK OUT DATE/TIME FAILED INPUT TX" Q
19 ;
 I $G(APCDLS)]"" S %=$$EXTSET^XBFUNC(9000010,.19,APCDLS) I %="" S APCDAFLG=3,APCDAFLG("ERR")=".19^"_APCDLS_"^LEVEL OF SERVICE FAILED INPUT TX" Q
21 ;
 I $G(APCDVELG)]"",'APCDVELG S APCDAFLG=3,APCDAFLG("ERR")=".21^"_APCDVELG_"^VA ELIG NOT VALID INTERNAL FORMAT" Q
 I $G(APCDVELG) S %=$P($G(^DD(9000010,.21,12.1)),"=",2) S X=$$FIND1^APCDDIC(8,APCDVELG,"I",%) I 'X S APCDAFLG=3,APCDAFLG("ERR")=".21^"_APCDVELG_"^VA ELIG FAILED INPUT TX" Q
22 ;
 I $G(APCDHL)]"",'APCDHL S APCDAFLG=3,APCDAFLG("ERR")=".22^"_APCDHL_"^HOSPITAL LOCATION NOT VALID INTERNAL FORMAT" Q
 I $G(APCDHL) S %=$P($G(^DD(9000010,.22,12.1)),"=",2) S X=$$FIND1^APCDDIC(44,APCDHL,"I",%) I 'X S APCDAFLG=3,APCDAFLG("ERR")=".22^"_APCDHL_"^HOSPITAL LOCATION FAILED INPUT TX" Q
24 ;
 I $G(APCDOPT)]"",'APCDOPT S APCDAFLG=3,APCDAFLG("ERR")=".24^"_APCDOPT_"^OPTION USED TO CREATE NOT VALID INTERNAL FORMAT" Q
 I $G(APCDOPT) S %=$P($G(^DD(9000010,.24,12.1)),"=",2) S X=$$FIND1^APCDDIC(19,APCDOPT,"I",%) I 'X S APCDAFLG=3,APCDAFLG("ERR")=".24^"_APCDOPT_"^OPTION USED TO CREATE FAILED INPUT TX" Q
25  ;
 I $G(APCDPROT)]"",'APCDPROT S APCDAFLG=3,APCDAFLG("ERR")=".25^"_APCDPROT_"^PROTOCOL USED TO CREATE NOT VALID INTERNAL FORMAT" Q
 I $G(APCDPROT) S %=$P($G(^DD(9000010,.25,12.1)),"=",2) S X=$$FIND1^APCDDIC(101,APCDPROT,"I",%) I 'X S APCDAFLG=3,APCDAFLG("ERR")=".25^"_APCDPROT_"^PROTOCOL USED TO CREATE FAILED INPUT TX" Q
26 ;
 I $G(APCDAPDT)]"" S X=$$FMTE^XLFDT(APCDAPDT) X $P(^DD(9000010,.26,0),U,5,99) I '$D(X) S APCDAFLG=3,APCDAFLG("ERR")=".26^"_APCDAPDT_"^APPT DATE/TIME FAILED INPUT TX - MUST BE IN INTERNAL FM FORMAT" Q
 Q
 ;
 ;--------------------------------------------------------------
 ;
OPTION ;EP;GET OPTION FROM USER
 F APCDAL=0:0 D OPTION2 Q:APCDAO
 Q
 ;
OPTION2 ; LET USER SELECT OPTION
 W !!,"PATIENT: ",$P(^DPT(APCDPAT,0),U)," has VISITs, same date, location.",!
 W !,"1  Create New VISIT"
 W !,"2  Exit without selecting VISIT"
 W !,"3  Display one of the existing VISITs"
 I $D(^XUSEC("APCDZVMRG",DUZ)),'$D(APCDALV(4)) W !,"4  Merge two VISITS"
 W !!,"Or select one of the following existing VISITs:",!
 F APCDAI=0:0 S APCDAI=$O(APCDALV(APCDAI)) Q:APCDAI=""  S APCDAX=APCDALV(APCDAI) D WRITE
 S DIR(0)="N^1:"_APCDAC_":0",DIR("A")="Choose one",DIR("?")="Choose one of the numbers listed above" S:$D(APCDADF) DIR("B")=APCDADF D ^DIR K DIR
 I $D(DIRUT) S APCDAO=2 Q
 S Y=+Y
 I Y=3 D DISPLAY Q
 I Y<($S('$D(APCDALV(4)):5,1:4)) S APCDAO=Y Q
 S APCDAO=Y,APCDVSIT=APCDALV(Y)
 Q
 ;
WRITE ; WRITE VISITS FOR SELECT
 S APCDA11=$G(^AUPNVSIT(APCDAX,11)),APCDAX=^AUPNVSIT(APCDAX,0)
 S APCDAT=$P(+APCDAX,".",2),APCDAT=$S(APCDAT="":"<NONE>",$L(APCDAT)=1:APCDAT_"0:00 ",1:$E(APCDAT,1,2)_":"_$E(APCDAT,3,4)_$E("00",1,2-$L($E(APCDAT,3,4)))_" ")
 W !,APCDAI,"  TIME: ",APCDAT,"TYPE: ",$P(APCDAX,U,3),"  CATEGORY: ",$P(APCDAX,U,7),"  CLINIC: ",$S($P(APCDAX,U,8)]"":$E($P(^DIC(40.7,$P(APCDAX,U,8),0),U),1,10),1:"<NONE>"),?56,"DEC: ",$S($P(APCDAX,U,9):$P(APCDAX,U,9),1:0)
 I $P(APCDA11,U,3)]"" W ?64,"VCN: ",$P(APCDA11,U,3)
 I $P(APCDAX,U,22) W !?3,"Hospital Location: ",$P($G(^SC($P(APCDAX,U,22),0)),U)
 K APCDAT
 Q
 ;
DISPLAY ; DISPLAY VISIT FOR USER
 I APCDAC=4 S APCDVDSP=APCDALV(APCDAC),APCDVDSP("NO IOF")="" D ^APCDVDSP Q
 S DIR(0)="NO^"_$S('$D(APCDALV(4)):5,1:4)_":"_APCDAC_":0",DIR("A")="Which one",DIR("?")="Enter the number associated with the visit you wish to display" D ^DIR K DIR
 Q:$D(DIRUT)
 S APCDVDSP=APCDALV(+Y),APCDVDSP("NO IOF")="" D ^APCDVDSP
 Q
 ;
MRG ;EP - merge two visits together
 W ! S DIR(0)="NO^"_$S('$D(APCDALV(4)):5,1:4)_":"_APCDAC_":0",DIR("A")="Choose 'FROM' Visit",DIR("?")="Enter the number associated with the visit you wish to merge from (the one to be deleted)" D ^DIR K DIR
 Q:$D(DIRUT)
 S APCDVMF=APCDALV(+Y)
 W ! S DIR(0)="NO^"_$S('$D(APCDALV(4)):5,1:4)_":"_APCDAC_":0",DIR("A")="Choose 'TO' Visit",DIR("?")="Enter the number associated with the visit you wish to merge into (the one to keep)" D ^DIR K DIR
 Q:$D(DIRUT)
 S APCDVMT=APCDALV(+Y)
 I APCDVMF=APCDVMT W !!,$C(7),$C(7),"'From' and 'To' the same.  Try Again!" Q
 W !!,"******* FROM VISIT *******" S APCDVDSP=APCDVMF,APCDVDSP("NO IOF")="" D ^APCDVDSP
 D PAUSE
 W !!,"******* TO VISIT *******" S APCDVDSP=APCDVMT,APCDVDSP("NO IOF")="" D ^APCDVDSP
 NEW APCDCAT,APCDCLN,APCDDATE,APCDDOB,APCDDOD,APCDLOC,APCDPAT,APCDSEX,APCDTYPE,APCDVSIT,APCDVMX,APCDVV,AUPNPAT,AUPNSEX,AUPNDAYS,AUPNDOB,AUPNVSIT,AUPNDOD
 D EN1^APCDVMRG
 Q
PAUSE ;EP
 Q:$E(IOST)'="C"!(IO'=IO(0))
 S DIR(0)="EO",DIR("A")="Press return to continue...." D ^DIR K DIR S:$D(DUOUT) DIRUT=1
 Q
