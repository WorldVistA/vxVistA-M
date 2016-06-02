VFDESIG ;DSS/SMP - E-SIGNATURE DISPLAY ;May 29, 2014 16:15
 ;;2013.0;DSS,INC VXVISTA OPEN SOURCE;**8,16**;;Build 2
 ;Copyright 1995-2014,Document Storage Systems Inc. All Rights Reserved
 ;
 Q
 ;
EN() ;
 N X,Y,Z,RET
 I $G(^%ZOSF("ZVX"))'["VX" Q ; Not vxVistA
 D GETWP^VFDCXPR(.RET,"~VFD ESIG DISPLAY~1")
 I '$D(RET(1)) Q 1
 S X=$$REPEAT^XLFSTR("*",80)
 W !,X,!,"*",$$RJ^XLFSTR("*",79)
 F Y=1:1 Q:'$D(RET(Y))  W "*  ",$$LJ^XLFSTR(RET(Y),74),"  *",!
 W "*",$$RJ^XLFSTR("*",79),!,X,!
 Q $$CONFIRM
 ;
CONFIRM() ; Ask the user to confirm message
 N X,Y,DIR,DTOUT,DUOUT,DIRUT,DIROUT
 S DIR(0)="YA",DIR("A")="Do you accept?  ",DIR("B")="NO"
 D ^DIR W ! I $D(DIRUT) Q 0
 Q Y
 ;
