IBCEQ1 ;BSL,ALB/TMK - PROVIDER ID QUERY ;25-AUG-03
 ;;2.0;INTEGRATED BILLING;**232,356,349**;21-MAR-94;Build 46
 ;;Per VHA Directive 2004-038, this routine should not be modified.
 ;
 ;QUERY TOOL HELPS IDENTIFY PLANS THAT ARE LACKING PROVIDER ID
 ;INFO OR HAVE BAD PROVIDER ID DATA FOR E-BILLING
 ;
 ;CONDITIONS TO IDENTIFY:
 ;1-BLUE CROSS LINKED TO 1500 ONLY (1) HARD ERROR
 ;2-BLUE SHIELD LINKED TO UB-04 ONLY (2) WARNING
 ;3-BLUE CROSS ID APPLIED TO BOTH FORMS (0) WARNING
 ;4-BLUE CROSS OR BLUE SHIELD IDs EXIST FOR AN INS CO, BUT ONE OR
 ;  MORE OF THE INSURANCE COMPANY'S PLANS DOES NOT HAVE AN
 ;  ELECTRONIC PLAN TYPE OF 'BL'
 ;5-NON BLUE CROSS/SHIELD ID FOR AN INS COMPANY WITH BLUE PLAN(S)
 ;6-VAD000 as an ID but not flagged as a UPIN
 ;
EN ;
 N POP,%ZIS,ZTSK,ZTRTN,ZTDESC,IBREBLD,IBSENDM,IBTO,DIR,X,Y,DUOUT,DTOUT,Z
 S IBREBLD=$S('$D(^XTMP("IB_PLAN232")):1,1:0)
 I $D(^XTMP("IB_PLAN232")) D
 . S DIR("?")="IF YOU ANSWER NO, REPORT WILL BE RUN FROM THE EXISTING QUERY DATA",DIR("?",1)="IF YOU ANSWER YES, A NEW QUERY WILL BE RUN"
 . S DIR(0)="YA",DIR("A",1)="THE EXTRACT GLOBAL FOR THIS QUERY ALREADY EXISTS",DIR("A")="DO YOU WANT TO DELETE IT AND RERUN THE QUERY?: ",DIR("B")="NO" W ! D ^DIR K DIR
 . Q:$D(DUOUT)!$D(DTOUT)!'Y
 . S IBREBLD=1
 ;
 N XMINSTR,Z,ZTSAVE
 K ^TMP("XMY",$J),^TMP("XMY0",$J)
 S XMINSTR("ADDR FLAGS")="R"
 D TOWHOM^XMXAPIU(DUZ,"","S",.XMINSTR)
 S Z="" F  S Z=$O(^TMP("XMY",$J,Z)) Q:Z=""  S IBTO(Z)=""
 K ^TMP("XMY",$J),^TMP("XMY0",$J)
 ;
 S %ZIS="QM" D ^%ZIS G:POP EN1Q
 I $D(IO("Q")) D  G EN1Q
 . S ZTRTN="ENT^IBCEQ1("_IBREBLD_",.IBTO)",ZTDESC="IB - HIPAA ENHANCEMENTS PROV ID QUERY",ZTSAVE("IBTO(")=""
 . D ^%ZTLOAD
 . W !!,$S($D(ZTSK):"Task # "_ZTSK_" has been queued.",1:"Unable to queue this job.")
 . K ZTSK,IO("Q") D HOME^%ZIS
 U IO
 D ENT(IBREBLD,.IBTO)
EN1Q Q
 ;
ENT(IBREBLD,IBTO) ; Queued job enter here
 ;
 N LOOP,Z
 K ^TMP($J,"SENDMSG")
 S ^TMP($J,"SENDMSG")=$S(IBREBLD:1,1:0)
 S Z="" F  S Z=$O(IBTO(Z)) Q:Z=""  S ^TMP($J,"SENDMSG",0,Z)=""
 I $G(IBREBLD) D
 . ; Rebld query
 . K ^XTMP("IB_PLAN232")
 . S ^XTMP("IB_PLAN232")="",^XTMP("IB_PLAN232",0)=$$FMADD^XLFDT(DT,45)_U_DT_"^IB PATCH 232 PROV ID QUERY"
 . ;
 . ; loop thru 355.91 (IB INSURANCE CO LEVEL BILLING PROV ID)
 . ;   then 355.9 (IB BILLING PRACTITIONER ID)
 . F LOOP=355.91,355.9 D LP
 . ;
 ;
 D RPTOUT^IBCEQ1A
 K ^TMP($J,"SENDMSG")
 Q
 ;
LP ; Loop through ids
 N IB,PTYP,PAYER,PLANIEN,FTA,IEPLAN,IPROV,PPROV,EDII,EDIP,PAYERP,TYPCOV,IBPMBPID,PTYPNM,IBI3,IBI0,SEQ,BLUE,TOT,NBLUE,DIR,DTOUT,DUOUT,X,Z,Z0,Z1,BL,UPIN,BCR,BSH
 S (SEQ,X,TOT,NBLUE,BLUE)=0,(BCR,BSH,UPIN)=""
 S Z="" F  S Z=$O(^IBE(355.97,Z)) Q:'Z  S Z0=$G(^(Z,0)) D
 . I $P(Z,U)["BLUE CROSS" S BCR=Z Q
 . I $P(Z,U)["BLUE SHIELD" S BSH=Z Q
 . I $P(Z,U)["UPIN" S UPIN=Z Q
 S:UPIN="" UPIN=22 S:BCR="" BCR=1 S:BSH="" BSH=2
 F  S X=$O(^IBA(LOOP,X)) Q:+X=0  D
 . S (PAYER,FTA,PLANIEN,IEPLAN,IPROV,PPROV,EDII,EDIP,PAYERP,TYPCOV,IBPMBPID,PTYPNM)=""
 . S SEQ=SEQ+1
 . S IB=$G(^IBA(LOOP,X,0))
 . S PTYP=$P(IB,U,6) ; prov id type ien
 . Q:PTYP=""  ; no prov type
 . S PTYPNM=$P($G(^IBE(355.97,PTYP,0)),U) ; prov id type desc
 . S PAYERP=$S(LOOP[".91":+IB,1:+$P(IB,U,2)) ;ins co ien
 . S IBI0=$G(^DIC(36,PAYERP,0)),IBI3=$G(^(3)),PAYER=$P(IBI0,U)
 . Q:$P(IBI0,U,5)!(IBI0="")  ; ins co inactive/deleted
 . S EDIP=$P(IBI3,U,2) ; edi id# prof
 . S EDII=$P(IBI3,U,4) ; edi id# inst
 . S IEPLAN=$P(IBI3,U,9) ; elec ins type ?1N
 . S PPROV=$P(IBI0,U,17) ; prof. prov#
 . S IPROV=$P(IBI0,U,11) ; hosp. prov#
 . S TYPCOV=$P(IBI0,U,13) ; type of cov ien;file 355.2
 . S FTA=$P(IB,U,4) ; form type applied; 0:both, 1:ub, 2:1500
 . S IBPMBPID=X_";"_LOOP
 . I $P(IB,U,7)="VAD000",PTYP'=UPIN D SET(6)
 . ;
 . I PTYP'=BCR&(PTYP'=BSH) D  Q    ; not BC/BS
 .. ; Only do following check once per insurance co
 .. Q:$D(^XTMP("IB_PLAN232",3,PAYERP))
 .. S ^XTMP("IB_PLAN232",3,PAYERP)=""
 .. ; Check if BC/BS ids exist at all for ins co
 .. Q:$O(^IBA(355.9,"AC",1,PAYERP,0))!$O(^IBA(355.9,"AC",2,PAYERP,0))!$O(^IBA(355.91,"AC",PAYERP,1,0))!$O(^IBA(355.91,"AC",PAYERP,2,0))
 .. S BL=0
 .. S Z1=0 F  S Z1=$O(^IBA(355.3,"B",PAYERP,Z1)) Q:'Z1  D
 ... I '$P($G(^IBA(355.3,Z1,0)),U,11),$P($G(^(0)),U,15)="BL" S PLANIEN=Z1,BL=1 D SET(5)
 .. S:BL NBLUE=NBLUE+1
 . ;
 . S BLUE=BLUE+1
 . ; ERROR - FORM TYPE=2:1500 AND PTYP=1:BC
 . I PTYP=1&(FTA=2) D SET(1) Q
 . ;
 . I PTYP=2&(FTA=1) D SET(2) Q  ; BS applied to just UB
 . I FTA=0&(PTYP=1) D SET(3) Q  ; BC applied to both forms
 . ;
 . ; Only do following check once per insurance co
 . I '$D(^XTMP("IB_PLAN232",2,PAYERP)) D  ; Checks plans not BL
 .. S Z1=0,^XTMP("IB_PLAN232",2,PAYERP)=""
 .. F  S Z1=$O(^IBA(355.3,"B",PAYERP,Z1)) Q:'Z1  D
 ... I $P($G(^IBA(355.3,Z1,0)),U,15)'="BL",'$P(^(0),U,11) S PLANIEN=Z1 D SET(4) Q
 ;
 ; 3RD PC XTMP(IB_PLAN232)=TOTAL BLUES WITH NO BLUE IDS
 S $P(^XTMP("IB_PLAN232"),U,3)=$P($G(^XTMP("IB_PLAN232")),U,3)+NBLUE
 ;
 ; 4TH PC XTMP(IB_PLAN232)=TOT NUMBER SCANNED
 S $P(^XTMP("IB_PLAN232"),U,4)=$P($G(^XTMP("IB_PLAN232")),U,4)+SEQ
 ;
 ; 5TH PC XTMP(IB_PLAN232)=TOT BLUES IDS FOUND
 S $P(^XTMP("IB_PLAN232"),U,5)=$P($G(^XTMP("IB_PLAN232")),U,5)+BLUE
 ;
 ; 6TH PC XTMP(IB_PLAN232)=TOTAL ERRORS FOUND
 S $P(^XTMP("IB_PLAN232"),U,6)=$P($G(^XTMP("IB_PLAN232")),U,6)+TOT
 Q
 ;
SET(Z) ;SET VALUES INTO SAVE GLOBAL
 ; Z=REASON WHY WE'RE SETTING IT
 ; 1. PAYER-ins co name (36)
 ; 2. PLAN-grp name (355.3)
 ; 3. GROUP-grp # (355.3)
 ; 4. FTA-form typ (355.9)
 ; 5. EPLAN-"BL" (355.3)
 ; 6. IEPLAN-elec ins typ (36)
 ; 7. IPROV-hosp prov# (36)
 ; 8. PPROV-prof prov# (36)
 ; 9. EDII-inst edi id# (36)
 ;10. EDIP-prof edi id# (36)
 ;11. PAYERP-ins co ien (36)
 ;12. TYPCOV-type of cov ien (36)
 ;13. PLANIEN-ien of file (355.3)
 ;14. IBPMBPID-355.9 or 355.91;ien of file
 ;15. PTYPNM-prov id type desc (355.9)
 ;16. Z-reason
 ;
 N A,DUP
 ;
 S A=$O(^XTMP("IB_PLAN232",1," "),-1)+1,TOT=TOT+1
 S ^XTMP("IB_PLAN232",1,A,0)=PAYER_U_""_U_""_U_FTA_U_""_U_IEPLAN_U_""_U_""_U_""_U_""_U_PAYERP_U_TYPCOV_U_PLANIEN_U_IBPMBPID_U_PTYPNM_U_Z
 Q
 ;
