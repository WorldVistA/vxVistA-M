VFDXTRTM ;DSS/RAC - Watch for changes in routine checksums. ; 02/05/2014 12:43
 ;;2013.0;DSS,INC VXVISTA OPEN SOURCE;;07 Feb 2014;Build 2
 ;Copyright 1995-2014,Document Storage Systems Inc. All Rights Reserved
 ;
A N DA,DIC,CNT,NOW,MODE,OS,RTN,RN,RSUM,TEST,VFDAR,XMB,X0,XMTEXT,Y,ZTQUEUED
 N VCNT S VCNT=0
 K ^TMP($J)
 S CNT=0,NOW=$$HTFM^XLFDT($H),U="^"
 S RSUM=^%ZOSF("RSUM1"),TEST=^%ZOSF("TEST")
 S MODE=$G(^XTV(8989.3,1,"RM")) I "n"[$E(MODE) Q
 G ALL:"a"=$E(MODE)
 ;
SEL S RTN=""
 F  S RTN=$O(^XTV(8989.3,1,"RM1","B",RTN)) Q:RTN=""  D
 . I RTN["*" D RANGE($P(RTN,"*")) Q
 . D CHK(RTN)
 . Q
 ;
EXIT ;
 D LOST
 G:CNT<0 DN ;No changes
 G:'$D(VFDAR) DN ; No data
 ; Build report from VFDAR
 N CHG,I,J,K,LOST
 S (J,K,CHG,LOST)=0
 F I=1:1:2 S K=K+1,^TMP($J,K,0)=$P($T(VFDHDR+I),";;",2)
 S CHG=CHG+1
 F  S J=$O(VFDAR("VFDRTMON","C",J)) Q:'J  D
 . N RTN,OCK,NCK,X
 . S K=K+1,X=VFDAR("VFDRTMON","C",J)
 . S ^TMP($J,K,0)=$E($J($P(X,U),16),1,16)_"  "_$E($J($P(X,U,2),13),1,13)_"  "_$E($J($P(X,U,3),13),1,13)_"  "_$J($P(X,U,4),13)
 . Q
 I $D(VFDAR("VFDRTMON","L")) D
 . S LOST=LOST+1,J=0
 . S K=K+1,^TMP($J,K,0)=$P($T(VFDHDR+3),";;",2)
 . F  S J=$O(VFDAR("VFDRTMON","L",J)) Q:'J  D
 . . N RTN,OCK,NCK,X
 . . S K=K+1,X=VFDAR("VFDRTMON","L",J)
 . . S ^TMP($J,K,0)=$E($J($P(X,U),16),1,16)_"  "_$E($J($P(X,U,2),13),1,13)_"  "_$E($J($P(X,U,3),13),1,13)_"  "_$J($P(X,U,4),13)
 . . Q
 . Q
SBULL ;
 S XMB="XTRMON",XMTEXT="^TMP($J,",XMB(1)=$$FMTE^XLFDT(NOW,1),XMB(2)=CNT
 X ^%ZOSF("UCI") S XMB(3)=Y
 D ^XMB
DN ;
 K CNT,RN,RTN,VFDAR,XMB,^TMP($J)
 Q
 ;
RANGE(RTN) ;Check a N-space
 S RN=RTN D CHK(RTN) ;Check for rtn with namespace name
 F  S RN=$O(^$ROUTINE(RN)) Q:$E(RN,1,$L(RTN))'=RTN  D CHK(RN)
 Q
 ;
ALL ;Check all routines
 S RN="" F  S RN=$O(^$ROUTINE(RN)) Q:RN=""  D CHK(RN)
 G EXIT
 ;
CHK(RN) ;Check one routine
 N $ET,$ES,DIE,DR S $ET="D CHKERR^VFDRTMON Q"
 W:'VCNT ! S VCNT=VCNT+1
 S X=RN X TEST Q:'$T
 S DA=$O(^DIC(9.8,"B",RN,0)) I DA<1 D  Q:DA'>0  ;See if RN is in file
 . S X=RN,DIC="^DIC(9.8,",DIC(0)="ML" D FILE^DICN ;No, so add
 . S DA=+Y I DA>0 S DIE=DIC,DR="1///R" D ^DIE ;Set routine flag
 . Q
 S X0=^DIC(9.8,DA,0),X=RN X RSUM I '$D(ZTQUEUED),'(VCNT#100) W $J(VCNT,8)
 Q:(Y<0)!(Y=+$P(X0,U,5))
 S CNT=CNT+1,VFDAR("VFDRTMON","C",CNT)=RN_U
 S:$P(X0,U,5)>0 $P(VFDAR("VFDRTMON","C",CNT),U,2)=$P(X0,U,5)_U_Y_U_"Changed"
 S:$P(X0,U,5)<1 $P(VFDAR("VFDRTMON","C",CNT),U,2)=U_Y_U_"New"
 S $P(^DIC(9.8,DA,0),U,5,6)=Y_U_NOW
 Q
 ;
CHKERR ;Handle an error during check
 S $ET="D ^%ZTER G UNWIND^%ZTER"
 S CNT=CNT+1,^TMP($J,CNT,0)=RN_" Caused an error: "_$$EC^%ZOSV
 S Y=-1,$EC="" ;Set Y=-1 to stop test
 Q
 ;
LOST ;Look for routines no-longer in the system
 N DA,DIK,IX
 I '$D(ZTQUEUED) W !,"Checking for LOST routines."
 S RTN=""
 F  S RTN=$O(^DIC(9.8,"B",RTN)) Q:RTN=""  D
 . Q:$E(RTN)="%"
 . S IX=$O(^DIC(9.8,"B",RTN,0)),X0=$G(^DIC(9.8,+IX,0)) Q:$P(X0,U,2)="PK"
 . S X=RTN X TEST Q:$T
 . S CNT=CNT+1,VFDAR("VFDRTMON","L",CNT)=RTN_U_U_U_"Not in UCI"
 . S DA=IX,DIK="^DIC(9.8," D ^DIK
 . Q
 Q
 ;
VFDHDR ; Write header for report
 ;;  Routine Name    Old Checksum   New Checksum       Notes
 ;;----------------  -------------  -------------  -------------
 ;;=============================================================
