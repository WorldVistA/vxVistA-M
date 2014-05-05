IBTRD ;ALB/AAS - CLAIMS TRACKING - DENIAL/ APPEALS ; 10-AUG-1993
 ;;2.0; INTEGRATED BILLING ;**1,199**; 21-MAR-94
 ;;Per VHA Directive 10-93-142, this routine should not be modified.
 ;
% ;
EN ; -- main entry point for IBT APPEAL/DENAIL EDITOR
 I '$D(DT) D DT^DICRW
 K XQORS,VALMEVL,IBTRD,DFN,IBCNS,IBTRN,IBTRV,IBTRC,IBFASTXT,VALMQUIT
 D ASK
 G:$D(VALMQUIT) ENQ
 I IBTRD["DPT(" S IBTYPE="P",DFN=+IBTRD D EN^VALM("IBT APPEAL/DENIAL EDITOR") G ENQ
 I IBTRD["DIC(" S IBTYPE="I",IBCNS=+IBTRD D EN^VALM("IBT APPEAL/DENIAL INS EDITOR")
ENQ K XQORS,VALMEVL,IBTRD,DFN,IBCNS,IBTRN,IBTRV,IBTRC,IBFASTXT,VALMQUIT
 K IBAMT,IBAPR,IBADG,IBDA,IBDGCR,IBDGCRU1,IBDV,IBETYP,IBETYPD,IBI,IBICD,IBLCNT,IBSEL,IBT,IBTEXT,IBTNOD,IBTSAV,VAUTD
 K IBAPEAL,IBCDFN,IBCNT,IBDEN,IBDENIAL,IBDENIAL,IBPARNT,IBPEN,IBPENAL,IBTCOD,IBTRDD,IBTRSV,IBTYPE,VAINDT,VA,VALMBCK,OFFSET,I1,I3,IBNEW,IBDENT,IBOE,Z1,T,SDCNT
 D KVAR^VADPT
 Q
 ;
HDR ; -- header code
 I $G(IBTYPE)="P" D HDRP
 I $G(IBTYPE)="I" D HDRI
 Q
 ;
HDRI ; -- header code for by ins. co.
 S VALMHDR(1)="Denials and Appeals for: "_$P(^DIC(36,+IBCNS,0),"^")
 S VALMHDR(2)=" "
 Q
 ;
HDRP ; -- header code for by pat.
 D PID^VADPT
 S VALMHDR(1)="Denials and Appeals for: "_$$PT^IBTUTL1(DFN)
 S VALMHDR(2)=" "
 Q
 ;
INIT ; -- init variables and list array
 S U="^",VALMCNT=0,VALMBG=1
 K ^TMP("IBTRC",$J),^TMP("IBTRCDX",$J)
 K I,X,XQORNOD,DA,DR,DIE,DNM,DQ,IBTRC
 I '$G(IBTRD),$G(DFN) S IBTRD=DFN_";DPT("
 I '$G(IBTRD) S VALMQUIT=""
 S IBTRSV=""
 Q:$D(VALMQUIT)
 ;
BLD ; -- build list
 K ^TMP("IBTRD",$J),^TMP("IBTRDDX",$J)
 N IBI,J,IBTRC,IBTRCD,IBTRCD1,IBTRN
 I IBTRD["DPT(" S IBTYPE="P",DFN=+IBTRD D BLDP
 I IBTRD["DIC(" S IBTYPE="I",IBCNS=+IBTRD D BLDI
 Q
 ;
BLDI ; -- Build list of appeals/denials by ins. co.
 D HDRI
 S (IBTRC,IBCNT,VALMCNT)=0
 S IBDEN=$O(^IBE(356.7,"ACODE",20,0))
 S IBTRC=0 F  S IBTRC=$O(^IBT(356.2,"AIACT",IBCNS,IBDEN,IBTRC)) Q:'IBTRC  D 1
 S IBPEN=$O(^IBE(356.7,"ACODE",30,0))
 S IBTRC=0 F  S IBTRC=$O(^IBT(356.2,"AIACT",IBCNS,IBPEN,IBTRC)) Q:'IBTRC  D 1
 Q
 ;
BLDP ; -- Build list of appeals/denials by patient
 D HDRP
 S (IBTRC,IBCNT,VALMCNT)=0
 S VALMSG=$$MSG^IBTUTL3(DFN)
 S IBDEN=$O(^IBE(356.7,"ACODE",20,0))
 S IBTRC=0 F  S IBTRC=$O(^IBT(356.2,"APACT",DFN,IBDEN,IBTRC)) Q:'IBTRC  D 1
 S IBPEN=$O(^IBE(356.7,"ACODE",30,0))
 S IBTRC=0 F  S IBTRC=$O(^IBT(356.2,"APACT",DFN,IBPEN,IBTRC)) Q:'IBTRC  D 1
 Q
1 ; -- first add denial, then add appeal
 S IBTRN=$P(^IBT(356.2,+IBTRC,0),"^",2)
 S IBTRSV=+IBTRC
 D 2
 N IBTRC,IBTRCD,IBTRCD1
 S IBAPEAL=$O(^IBE(356.11,"ACODE",60,0)) ; find appeals
 S IBTRC=0 F  S IBTRC=$O(^IBT(356.2,"AP",IBTRSV,IBTRC)) Q:'IBTRC  D 2
 ;
 Q
2 ; -- add items to list
 S IBTRCD=$G(^IBT(356.2,+IBTRC,0))
 S IBTRCD1=$G(^IBT(356.2,+IBTRC,1))
 Q:'+$P(IBTRCD,"^",19)  ;quit if inactive
 ;
 ; -- if not the denial, must be from parent
 I IBTRC'=IBTRSV&($P(IBTRCD,"^",18)'=IBTRSV) Q
 ;
 S IBCNT=IBCNT+1
 W "."
 I IBTYPE="I" S DFN=$P(IBTRCD,"^",5) D PID^VADPT
 S X=""
 S X=$$SETFLD^VALM1(IBCNT,X,"NUMBER")
 ;
 I IBTYPE="I" D
 .S X=$$SETFLD^VALM1($P(^DPT(DFN,0),"^"),X,"PATIENT")
 .S X=$$SETFLD^VALM1(VA("BID"),X,"ID")
 ;
 I IBTYPE="P" D
 .S X=$$SETFLD^VALM1($P($G(^DIC(36,+$P(IBTRCD,"^",8),0)),"^"),X,"INS CO")
 .S X=$$SETFLD^VALM1($$GRP^IBCNS(+$P($G(^DPT(DFN,.312,+$P(IBTRCD1,"^",5),0)),"^",18)),X,"POLICY")
 ;
 S X=$$SETFLD^VALM1($P($$DAT1^IBOUTL(+IBTRCD,"2P")," "),X,"DATE")
 I $P(IBTRCD,"^",11) S X=$$SETFLD^VALM1($$EXPAND^IBTRE(356.2,.11,$P(IBTRCD,"^",11)),X,"ACTION")
 I $P(IBTRCD,"^",11)="" S X=$$SETFLD^VALM1($P($G(^IBE(356.11,+$P(IBTRCD,"^",4),0)),"^",3),X,"ACTION")
 ;
 S X=$$SETFLD^VALM1($P($G(^IBE(356.6,+$P(^IBT(356,+IBTRN,0),"^",18),0)),"^",2),X,"EVENT")
 S X=$$SETFLD^VALM1($$DAT1^IBOUTL(+$P(^IBT(356,+IBTRN,0),"^",6),"2P"),X,"EV DATE")
 S X=$$SETFLD^VALM1($$EXPAND^IBTRE(356.2,.04,$P(IBTRCD,"^",4)),X,"TYPE")
 S X=$$SETFLD^VALM1($J($$DAY^IBTUTL3($P(IBTRCD,"^",15),$P(IBTRCD,"^",16),IBTRN),3),X,"DAYS")
 S X=$$SETFLD^VALM1($$EXPAND^IBTRE(356,.31,$P(^IBT(356,IBTRN,0),"^",31)),X,"ROI")
 S X=$$SETFLD^VALM1($P(IBTRCD,"^",6),X,"CONTACT")
 S X=$$SETFLD^VALM1($P(IBTRCD,"^",7),X,"PHONE")
 S X=$$SETFLD^VALM1($P(IBTRCD,"^",9),X,"REF NO")
 D SET(X)
 Q
 ;
SET(X) ; -- set arrays
 S VALMCNT=VALMCNT+1
 S ^TMP("IBTRD",$J,VALMCNT,0)=X
 S ^TMP("IBTRD",$J,"IDX",VALMCNT,IBCNT)=""
 S ^TMP("IBTRDDX",$J,IBCNT)=VALMCNT_"^"_IBTRC
 Q
HELP ; -- help code
 S X="?" D DISP^XQORM1 W !!
 Q
 ;
EXIT ; -- exit code
 K ^TMP("IBTRD",$J),^TMP("IBTRDDX",$J),IBTRD
 D CLEAN^VALM10
 Q
 ;
ASK ; -- ask for patient or ins. co.
 N DIR
 S DIR(0)="350.9,4.02",DIR("A")="Select Patient Name or Insurance Co."
 N DPTNOFZY S DPTNOFZY=1  ;Suppress PATIENT file fuzzy lookups
 D ^DIR K DIR I $D(DIRUT) S VALMQUIT="" G ASKQ
 S IBTRD=Y
 I +IBTRD<1 S VALMQUIT=""
ASKQ Q
