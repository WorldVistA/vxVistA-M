FBNHPC1 ;AISC/CMR-POST COMMITMENTS TO 1358 cont. ;9/20/94
 ;;3.5;FEE BASIS;;JAN 30, 1995
 ;;Per VHA Directive 10-93-142, this routine should not be modified.
CHECK S ^XTMP("FBPOST",FBIFN)=""
 Q:$E(FBPAYDT,1,5)<$E(FBABD,1,5)
 S FBTRDYS=0 K FBELSE
 S FBERR="" D GETRAT^FBNHEP2 Q:FBERR  D CHECK^FBNHEP2 Q:FBERR
 S FBIFN=FBHIFN
 D
 .I $E(FBABD,1,5)<$E(FBPAYDT,1,5)&($E(FBDD,1,5)>$E(FBPAYDT,1,5)) S FBTRDYS=FBDAYS D CALC Q
 .I $E(FBABD,1,5)<$E(FBPAYDT,1,5)&($E(FBDD,1,5)'>$E(FBPAYDT,1,5)) S FBTRDYS=$E(FBDD,6,7)-1,FBTRDYS=$S(FBTRDYS>0:FBTRDYS,1:0) S:FBTRDYS'>0 FBELSE=1 D CALC Q
 .I $E(FBABD,1,5)=$E(FBPAYDT,1,5)&($E(FBDD,1,5)>$E(FBPAYDT,1,5)) S FBTRDYS=(FBDAYS-$E(FBABD,6,7))+1 D CALC Q
 .I FBABD=FBDD S FBTRDYS=1 D CALC Q
 .I $E(FBABD,1,5)=$E(FBPAYDT,1,5)&($E(FBDD,1,5)'>$E(FBPAYDT,1,5)) S FBTRDYS=FBDD-FBABD,FBTRDYS=$S(FBTRDYS>0:FBTRDYS,1:0) S:FBTRDYS'>0 FBELSE=1 D CALC Q
 Q:$D(FBELSE)
 ;ifcap calls for posting
 I 'FBNHCC,$D(^PRC(424,"E",+$P(FBZ,"^",3)_";"_+FBIFN_";"_$P(FBOBN,"-",2)_";"_FBMM)) Q
 S FBSEQ="" I 'FBNHCC S X=FBOBN,PRCS("TYPE")="FB" D EN1^PRCSUT31 Q:Y=""  S FBSEQ=Y
 S DFN=$P(FBZ,"^",3),FBNAME=$$NAME^FBCHREQ2(DFN),FBSSN=$$SSN^FBAAUTL(DFN),FBVEN=$P(FBZ,"^",2)
 S PRCS("TYPE")="FB" I 'FBNHCC D NOW^%DTC S FBPOSDT=%,X=FBOBN_"^"_FBPOSDT_"^"_FBDEFP_"^^"_FBSEQ_"^"_FBNAME_"  ("_FBSSN_")"_"^"_+DFN_";"_+FBIFN_";"_$P(FBOBN,"-",2)_";"_FBMM D EN2^PRCS58
 I 'FBNHCC G:+Y=0 ERR^FBNHPC
 S FBTOTAL=FBTOTAL+FBTRDYS,FBTOT=FBTOT+FBDEFP,FBCNT=FBCNT+1
 S ^TMP($J,"FBNHPC",FBNAME,FBCNT)=FBSSN_"^"_FBTRDYS_"^"_FBDEFP_"^"_$G(FBSEQ)_"^"_+FBVEN
 Q
 ;
PRT S FBNAME="" F  S FBNAME=$O(^TMP($J,"FBNHPC",FBNAME)) Q:FBNAME']""!($G(FBOUT))  F FBCNT=0:0 S FBCNT=$O(^TMP($J,"FBNHPC",FBNAME,FBCNT)) Q:'FBCNT!($G(FBOUT))  D
 .S Y(0)=$G(^TMP($J,"FBNHPC",FBNAME,FBCNT)),FBSSN=$P(Y(0),"^"),FBTRDYS=$P(Y(0),"^",2),FBDEFP=$P(Y(0),"^",3),FBSEQ=$P(Y(0),"^",4),FBVEN=$P(Y(0),"^",5)
 .I $Y+3>IOSL,$E(IOST,1,2)["C-" S DIR(0)="E" D ^DIR K DIR S:'Y FBOUT=1 Q:FBOUT
 .I ($Y+3)>IOSL W @IOF D HED
 .W !,FBSEQ,?7,$E(FBNAME,1,20),?30,FBSSN,?44,$E($$VNAME^FBNHEXP(FBVEN),1,20),?66,FBTRDYS,?70,$J($FN(FBDEFP,",",2),10)
 Q
 ;
HED W !?11,"C O M M U N I T Y   N U R S I N G   H O M E   R E P O R T",!
 S I="",$P(I,"-",59)="-" W ?10,I K I W !
 I FBNHCC W ?5,"Estimated Funds for: ",$P("Jan^Feb^Mar^Apr^May^Jun^Jul^Aug^Sep^Oct^Nov^Dec","^",+FBMM)," ",FBYY,!!
 I 'FBNHCC W ?5,"Postings for Obligation Number: ",$P(FBOBN,"-",2),!!,"Ref #"
 W ?7,"Veteran",?30,"SSN",?44,"Vendor",?65,"Days",?74,"Total",!,Q,!
 Q
 ;
CALC S FBENDFLG=$S(FBDD'>FBENDDT:1,1:""),FBENDDT=$S(FBDD>FBENDDT:FBENDDT,1:FBDD),FBAABDT=FBABD D CALC^FBNHEP2
 S FBENDDT=FBPAYEDT K FB
 Q
