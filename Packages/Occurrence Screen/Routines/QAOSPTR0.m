QAOSPTR0 ;HISC/DAD-REVIEW LEVEL TRACKING REPORT ;10/19/92  15:24
 ;;3.0;Occurrence Screen;;09/14/1993
 D ^QAQDATE G:QAQQUIT EXIT
 K %ZIS S %ZIS="QM" D ^%ZIS G:POP EXIT I $D(IO("Q")) S ZTDESC="Review level tracking report",ZTRTN="ENTSK^QAOSPTR0",ZTSAVE("QAQ*")="" D ^%ZTLOAD G EXIT
ENTSK ;
 S QAOSCLIN=$O(^QA(741.2,"C",1,0)),QAOSPEER=$O(^QA(741.2,"C",2,0)),QAOSMGMT=$O(^QA(741.2,"C",3,0)),QAOSCMTE=$O(^QA(741.2,"C",4,0)) K ^TMP($J,"QAOSPTR")
 F QAOSD0=0:0 S QAOSD0=$O(^QA(741,"AD",0,QAOSD0)) Q:QAOSD0'>0  D:$D(^QA(741,QAOSD0,"REVR","B",QAOSCLIN)) LOOP1
 U IO D ^QAOSPTR1
EXIT ;
 W ! D ^%ZISC
 K %ZIS,ACTION,COUNT,DATE,FIND,HEAD2,LEVEL,LOC,PAGE,PAT,POP,PT,QAOSCLIN,QAOSCMTE,QAOSD0,QAOSD1,QAOSD2,QAOSDT,QAOSMGMT,QAOSPEER,QAOSQUIT,QAOSREVR,QAOSRVW,QAOSZERO,REVR,SCRN,SERV,SRV,SSN,TODAY,UNDL,X,Y,ZTDESC,ZTRTN,^TMP($J,"QAOSPTR")
 K %DT,D,DI,DIC,DQ,I,TYPE,Y,Z D K^QAQDATE S:$D(ZTQUEUED) ZTREQ="@"
 Q
LOOP1 ;
 S QAOSQUIT=0,QAOSZERO=^QA(741,QAOSD0,0),SCRN=+$G(^("SCRN")),PAT=$S($D(^DPT(+QAOSZERO,0))#2:^(0),1:+QAOSZERO),SSN=$P(PAT,"^",9),PAT=$P(PAT,"^"),SCRN=$S($D(^QA(741.1,SCRN,0))#2:+^(0),1:SCRN)
 S SERV=$P(QAOSZERO,"^",6),SERV=$S(SERV="":"~UNKNOWN",$D(^DIC(49,SERV,0))#2:$P(^(0),"^"),1:"~UNKNOWN"),DATE=$P(QAOSZERO,"^",3)
 Q:(DATE<QAQNBEG)!(DATE>QAQNEND)
 S ^TMP($J,"QAOSPTR",SERV,SCRN,PAT,DATE)=SSN
 F QAOSREVR=QAOSCLIN,QAOSPEER,QAOSMGMT Q:QAOSQUIT  S COUNT=1 I $D(^QA(741,QAOSD0,"REVR","B",QAOSREVR)) F QAOSD1=0:0 S QAOSD1=$O(^QA(741,QAOSD0,"REVR","B",QAOSREVR,QAOSD1)) Q:QAOSD1'>0!QAOSQUIT  D REVRLOOP
 Q:QAOSQUIT  S COUNT=1 F QAOSD1=0:0 S QAOSD1=$O(^QA(741,QAOSD0,"CMTE",QAOSD1)) Q:QAOSD1'>0  D CMTELOOP
 Q
REVRLOOP ;
 S QAOSRVW=$G(^QA(741,QAOSD0,"REVR",QAOSD1,0)),FIND=$P(QAOSRVW,"^",5),FIND=$S(FIND'>0:"",$D(^QA(741.6,FIND,0))#2:$P(^(0),"^",2),1:"")
 I FIND="",QAOSREVR=QAOSCLIN K ^TMP($J,"QAOSPTR",SERV,SCRN,PAT,DATE) S QAOSQUIT=1 Q
 S QAOSD2=$O(^QA(741,QAOSD0,"REVR",QAOSD1,2,0)),ACTION=$S(QAOSD2'>0:"",$D(^QA(741,QAOSD0,"REVR",QAOSD1,2,QAOSD2,0))#2:+^(0),1:""),ACTION=$S(ACTION'>0:"",$D(^QA(741.7,ACTION,0))#2:$P(^(0),"^",3),1:"")
 S X=$S(QAOSREVR=QAOSMGMT:ACTION,1:FIND) S:X]"" ^TMP($J,"QAOSPTR",SERV,SCRN,PAT,DATE,QAOSREVR,COUNT)=X,COUNT=COUNT+1
 Q
CMTELOOP ;
 S QAOSRVW=$G(^QA(741,QAOSD0,"CMTE",QAOSD1,0)),FIND=$P(QAOSRVW,"^",2),FIND=$S(FIND'>0:"",$D(^QA(741.6,FIND,0))#2:$P(^(0),"^",2),1:"")
 S:FIND]"" ^TMP($J,"QAOSPTR",SERV,SCRN,PAT,DATE,QAOSCMTE,COUNT)=FIND,COUNT=COUNT+1
 Q
