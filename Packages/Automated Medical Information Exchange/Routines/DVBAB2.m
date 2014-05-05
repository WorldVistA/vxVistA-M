DVBAB2 ;ALB/KLB - CAPRI RO AMIS REPORT CONT. ;05/01/00
 ;;2.7;AMIE;**35,42**;Apr 10, 1995
 ;
DAY30 K ^TMP("DVBC",$J)
 F FA=DTRPT-.1:0 S FA=$O(^DPT(PNAM,"S",FA)) Q:FA=""!(FA>EDATE)  I $D(^(FA,0)) S L=^(0),C=+L S YY=$P(L,U,2) I YY'="N"&(YY'="C")&(YY'="NA")&(YY'="CA")&(YY'="PC")&(YY'="PCA") I FA'>EDATE S ^TMP("DVBC",$J,9999999-FA,FA)=""
 Q:'$D(^TMP("DVBC",$J))  S FB=$O(^TMP("DVBC",$J,0)),FA=$O(^TMP("DVBC",$J,FB,0)) I FA]"" S X2=FA,X1=$S(DTSCHEDC]"":DTSCHEDC,1:DVBCNOW) D ^%DTC I X>30 S TOT("30DAYEX")=TOT("30DAYEX")+1
 Q
 ;
PENDCNT I X'<0&(X'>90) S TOT("P90")=TOT("P90")+1
 I X>90&(X'>120) S TOT("P121")=TOT("P121")+1
 I X>120&(X'>150) S TOT("P151")=TOT("P151")+1
 I X>150&(X'>180) S TOT("P181")=TOT("P181")+1
 I X>180&(X'>365) S TOT("P365")=TOT("P365")+1
 I X>365 S TOT("P366")=TOT("P366")+1
 Q
 ;
SET S DTA=^DVB(396.3,REQDA,0),DTREQ=$P(DTA,U,2),XRONUM=$P(DTA,U,3),XRONUM=$S($D(^DIC(4,+XRONUM,99)):$P(^(99),U,1),1:0) Q:XRONUM'=RONUM&(RONUM'="ALL")
 ; Next 2 lines check for specific division  SPH/ALB - 9/3/02
 I DVBDIV'="" I '$D(^DVB(396.3,REQDA,1)) Q
 I DVBDIV'="" I $P(^DVB(396.3,REQDA,1),"^",4)'=DVBDIV Q
 K XRONUM S DTRPT=$P(DTA,U,5),DTSCHEDC=$P(DTA,U,6),DTRQCMP=$P(DTA,U,7),DTTRANS=$P(DTA,U,12),DTREL=$P(DTA,U,14),RQSTAT=$P(DTA,U,18),DTCAN=$P(DTA,U,19),PRIO=$P(DTA,U,10) K DTA
 I DTRPT="",DTCAN]"" S DTRPT=DTCAN
 Q:DTRPT=""  ;requests never printed
 I DTREL'<BDATE,DTREL'>EDATE D DAY30
 I DTRPT'<BDATE,DTRPT'>EDATE S TOT("SENT")=TOT("SENT")+1
 I DTRPT'<BDATE,DTRPT'>EDATE,RQSTAT'["X" S X1=$S(DTSCHEDC]"":DTSCHEDC,1:DVBCNOW),X2=DTRPT D ^%DTC I X>3 S TOT("3DAYSCH")=TOT("3DAYSCH")+1
 I DTREL'<BDATE&(DTREL'>EDATE),RQSTAT="C"!(RQSTAT="R") S:PRIO'="E" DVBCPCTM=$$PROCDAY^DVBCUTL2(REQDA) S:PRIO="E" DVBCPCTM=$$INSFTME^DVBCUTA1(REQDA) S TOT("DAYS")=TOT("DAYS")+DVBCPCTM K DVBCPCTM
 I DTRPT'>EDATE,"^P^S^T"[RQSTAT S TOT("PENDADJ")=TOT("PENDADJ")+1,X1=EDATE,X2=DTRPT D ^%DTC,PENDCNT
 I DTRPT'>EDATE,"^C^CT^R^RX^X^"[RQSTAT,(+DTREL>EDATE)!(+DTCAN>EDATE) S TOT("PENDADJ")=TOT("PENDADJ")+1,X1=EDATE,X2=DTRPT D ^%DTC,PENDCNT
 I DTREL'<BDATE&(DTREL'>EDATE),RQSTAT["C"!(RQSTAT="R") S TOT("COMPLETED")=TOT("COMPLETED")+1
 I DTRPT'<BDATE,DTRPT'>EDATE,PRIO="E" S TOT("INSUFF")=TOT("INSUFF")+1
 I DTCAN'<BDATE&(DTCAN'>EDATE),RQSTAT="X"!(RQSTAT="RX") S TOT("INCOMPLETE")=TOT("INCOMPLETE")+1
 K DTRPT Q
 ;
GO S DVBABCNT=0
 S %DT="TS",X="NOW" D ^%DT S DVBCNOW=Y
 S PNAM="" F JJ=0:0 S PNAM=$O(^DVB(396.3,"B",PNAM)) Q:PNAM=""  F REQDA=0:0 S REQDA=$O(^DVB(396.3,"B",PNAM,REQDA)) Q:REQDA=""  D SET
 S TOT("AVGDAYS")=0 I TOT("COMPLETED")>0 S TOT("AVGDAYS")=TOT("DAYS")/TOT("COMPLETED"),TOT("AVGDAYS")=$J(TOT("AVGDAYS"),5,1)
 D BULLTXT^DVBCAMR1
 F JI=0:0 S JI=$O(^TMP($J,JI)) Q:JI=""  S DVBABCNT=DVBABCNT+1,MSG(DVBABCNT)=^TMP($J,JI,0)
 S:'$D(XMY) SBULL="N" I SBULL="Y" D SEND
 ;
EXIT K BDATE,%DT,DVBABCNT,C,DTCAN,DTREL,DTREQ,DTRQCMP,DTSCHEDC,DTTRANS,DVBCNOW,DVBCPCTM,EDATE,FA,FB,JI,JJ,L,PNAM,PRIO,REQDA,RONUM,RQSTAT,SBULL,TOT,X,X1,X2,XMDUZ,XMMG,Y,YY
 Q
 ;
BULL S XMDUZ=$P(^VA(200,DUZ,0),U),XMMG=$S($D(^VA(200,DUZ,0)):$P(^(0),U,1),1:""),XMY(DUZ)=""
 S XMSUB="RO AMIS 290 Report -  " S Y=BDATE X ^DD("DD") S XMSUB=XMSUB_Y S Y=EDATE X ^DD("DD") S XMSUB=XMSUB_" to "_Y,XMTEXT="^TMP($J,"
 Q
 ;
SEND D ^XMD K XMY,XMTEXT,XMSUB
 S MSG(1)=">>> Mail message transmitted. <<<"
 Q
 ;
