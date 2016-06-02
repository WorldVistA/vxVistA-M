VFDPRGCLK ;DSS/SMP - VFD Program Codes Location Lookup ; 04/17/2015 15:20
 ;;2013.1;DSS,INC VXVISTA OPEN SOURCE;**25**;28 Jan 2013;Build 16
 ;Copyright 1995-2015,Document Storage Systems Inc. All Rights Reserved
 ;
 ; This routine is used for a special lookup on subfile 21630.0051
 ; LOCATIONS multiple within the VFD PROGRAM CODE file.
 ;
 N INST,TEMP
 S Y=1
 S INST=$$INST(X) I INST<0 S Y=-1 Q
 S TEMP=$$ACT4LOC^VFDPRGC(+INST,DA)
 I 'TEMP S TEMP=$$FUT4LOC^VFDPRGC(+INST,DA)
 I TEMP S Y=TEMP Q
 S Y=$$ADD(INST,DA,1)
 Q
 ;
ADD(LOC,PRGC,ASK,SDT) ; Add new LOCATION
 N VFDERR,VFDFDA,VFDIEN
 I $G(ASK) Q:'$$ASK(LOC) -1
 S VFDFDA(21630.0051,"+1,"_PRGC_",",.01)=+LOC
 S VFDFDA(21630.0051,"+1,"_PRGC_",",.02)=$S($G(SDT):SDT,1:DT)
 D UPDATE^DIE(,"VFDFDA","VFDIEN","VFDERR")
 I $D(VFDERR) Q -1
 Q +VFDIEN(1)
 ;
ASK(LOC) ;
 N X,Y,Z,DIR,DTOUT,DUOUT,DIRUT,DIROUT
 S DIR(0)="Y",DIR("A")="  Are you adding '"_$P(LOC,U,2)_"' as a new LOCATION"
 S DIR("B")="No" D ^DIR Q:$D(DIRUT) 0
 Q Y
 ;
INST(X) ; Find Institution
 N Y,DIC
 S DIC=4,DIC(0)="EQ"
 D ^DIC
 Q Y
