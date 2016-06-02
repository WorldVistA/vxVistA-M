VFDATA1 ;DSS/LM/SMP - Data Table Utilities ; 11/03/2015 15:00
 ;;15.0;DSS,INC VXVISTA OPEN SOURCE;**26**;01 Dec 2009;Build 1
 ;Copyright 1995-2015,Document Storage Systems Inc. All Rights Reserved
 ;
 Q
 ;
MPORT ;[PUBLIC] VFD TABLE IMPORT interactive OPTION
 ;
 ;
 N DIC,X,Y
 W !,"This option imports data to a VFD TABLE file entry.",!
 ;S DIC=21611,DIC(0)="AEQM" D ^DIC Q:Y<0
 S Y=$$GET Q:Y<0
 N VFDHFS,VFDIEN,VFDNAM S VFDIEN=+Y,VFDNAM=$P(Y,U,2)
 S VFDHFS=$$GET1^DIQ(21611,+Y,5)
 I '$L(VFDHFS) D  Q
 .W !!,"Table '"_VFDNAM_"' does not have a HOST FILE defined."
 .W !,"Please edit this entry using option 'Enter/Edit VFD TABLE'"
 .W !,"and supply a value for the HOST FILE field.  Then retry."
 .W !
 .Q
 N DIR S DIR(0)="Y",DIR("A")="Are you sure"
 W !!,"Import and replace data for table "_VFDNAM_" from"
 W !,VFDHFS,! D ^DIR Q:$D(DIRUT)
 I '(Y=1) W !,"Import aborted." Q
 N VFDARY,VFDRSLT S VFDARY(1)=".01^`"_VFDIEN
 D IMPORT^VFDATA(.VFDRSLT,.VFDARY)
 I VFDRSLT<0 D  Q
 .W !,"The import request failed with the following error:"
 .W !,$P(VFDRSLT,U,2)
 .Q
 W !,"Data imported to table IEN="_VFDIEN
 Q
MDLTE ;[PUBLIC] VFD TABLE DELETE DATA interactive OPTION
 ;
 ;
 N DIC,X,Y
 W !,"This option deletes data from a VFD TABLE file entry."
 W !,"Row and column headers and data cell values are deleted."
 W !,"Non-data fields are not changed.",!
 ;S DIC=21611,DIC(0)="AEQM" D ^DIC Q:Y<0
 S Y=$$GET() Q:Y<0
 N VFDIEN,VFDNAM S VFDIEN=+Y,VFDNAM=$P(Y,U,2)
 N DIR S DIR(0)="Y",DIR("A")="Are you sure"
 W !!,"Imported data will be deleted from table "_VFDNAM
 D ^DIR Q:$D(DIRUT)
 I '(Y=1) W !,"Import aborted." Q
 D DELETE^VFDATA(VFDIEN)
 W !,"Delete request has been processed."
 Q
 ;
 ;
EXPORT ; [PUBLIC] VFD TABLE EXPORT interactive OPTION
 ;
 ;
 N X,Y,DIR,FILE,PATH
 W !,"This option exports data from a VFD TABLE file entry.",!
 S Y=$$GET Q:Y<0
 S PATH=$$PATH Q:PATH=-2
 D EXPORT^VFDATA2(+Y,PATH)
 Q
 ;
 ;=====================================================================
 ; PRIVATE
 ;=====================================================================
GET() ;
 N X,Y,DIC
 S DIC=21611,DIC(0)="AEQM" D ^DIC
 Q Y
 ;
PATH() ;
 N DEF S DEF=$$DEFDIR^%ZISH()
 Q $S($L($T(ASKPATH^VFDXTR)):$$ASKPATH^VFDXTR,1:DEF)
 ;
EXPALL ;
 N X,PATH
 S PATH=$$PATH Q:PATH=-1
 S X=0 F  S X=$O(^VFDV(21611,X)) Q:'X  D EXPORT^VFDATA2(X,PATH)
 Q
