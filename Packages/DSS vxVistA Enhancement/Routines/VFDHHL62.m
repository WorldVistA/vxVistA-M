VFDHHL62 ;DSS/PDW -lab shipping config file load from HL7X LAB 60 STAGING   ; 02 Mar 2012 13:04
 ;;2012.1.1;VENDOR - DOCUMENT STORAGE SYS;;24 Jun 2013;Build 136
 ;Copyright 1995-2013,Document Storage Systems Inc. All Rights Reserved
 ;DSS/BUILD - VFDHHL HL7 EXCHANGE 2012.1.1 * 06/23/15 * vxdev64v2k8_VX13 * 2012.1 T22
 ;DSS/BUILD - VFDHHL HL7 EXCHANGE 2012.1.1 * 02/02/14 * vxdev64v2k8_DEVXOS * 2012.1.1T21
 Q
EN ;
LSCF ; Lab Shipping Configuration file . . . VFDACCDA, VFDINSDA, VFDLSCDA
 N DIR S DIR(0)="SO^C:Check Staging File;B:Build Lab Shipping Configuration file"
 D ^DIR
 I Y'="B",Y'="C" Q
 I Y="B" D LSCBFILE G LSCF
 I Y="C" D LSCHECK("") G LSCF
 Q  ;
LSCF1 ; return VFDLSCDA,VFDINSDA
 N DIC,L,D
LSCF2 ;
L ;lab shipping config
 S DIC=62.9,DIC(0)="AEQMLZ" D ^DIC
 I Y'>0 Q
 M L=Y
D ;division 
 I $G(VFDINST)  S DD=+VFDINST,DD(0,0)=$$GET1^DIQ(4,DD,.01) G LSCDSP
C4 S DIC=4,DIC(0)="AEQMZ" D ^DIC
 I +Y'>0  W !,"A choice must be made" G C4
 S VFDINST=+Y K DD M DD=Y
 ;
LSCDSP ;
 W !!!,"Lab Shipping Configuration",?30,L(0,0)
 ;W !,"Accession",?30,A(0,0)
 W !,"Division",?30,DD(0,0),!
 K DIR S DIR(0)="YO",DIR("A")="Is the above correct?",DIR("B")="N" D ^DIR
 I Y'=1 G LSCF2
 S VFDINSDA=+DD,VFDLSCDA=+L
 Q
LSCHECK(ACTION) ;LabShpCon Check$$RETURN # bad entries of staging file
 ;ACTION CHECKONLY,"", OR DISPLAY
 S ACTION=$G(ACTION)
 N VFDERT,VFDLR6DA,NM,XFLD S VFDERT=0
 S VFDLR6DA=0 F  S VFDLR6DA=$O(^VFD(21626.01,VFDLR6DA)) Q:VFDLR6DA'>0  W "." D
 .N VFDTMP,VFDLR6,VFDMSG
 .D GETS^DIQ(21626.01,VFDLR6DA_",",".01;.02;.03;.09;.14;.42;.43;.44;.45","IE","VFDTMP","VFDMSG")
 .M VFDLR6=VFDTMP(21626.01,VFDLR6DA_",")
 .I $G(VFDLR6(.09,"I"))'="B" Q  ;type '= both
 .N VFDER S VFDER=0
 . ;/PDW 6/13/2013 $G( ADDED
 .F XFLD=.01,.02,.03,.09,.14,.42,.43,.44,.45 I '$L($G(VFDLR6(XFLD,"E"))) S VFDER=$G(VFDER)+1
 .;PDW/JUN 18 2013
 .I $G(VFDER) D
 ..I ACTION'="CHECKONLY" F XFLD=.01,.02,.03,.09,.14,.42,.43,.44,.45 D FIELD^DID(21626.01,XFLD,"","LABEL","NM") D
 ...; pdw 18 Jun 13
 ...W !,XFLD,?5,$E(NM("LABEL"),1,20),?30,$G(VFDLR6(XFLD,"E")),?60,$G(VFDLR6(XFLD,"I"))
 ..W !,VFDER,?5,"Error(s) in ",$G(VFDLR6(.01,"I"))
 ..W !
 .I VFDER S VFDERT=VFDERT+1
 W !,"Total bad entries ",?20,VFDERT
 Q VFDERT
LSCBFILE ; Lab Shipping Configuration build file entries
 N XX,VFDLR6DA,VFDINSDA,VFDLSCDA
 D LSCF1 I $G(VFDINSDA),$G(VFDLSCDA) I 1
 E  W !,"Lab Shipping Configuraftion or Division not selected" D E Q
 W !,"Prechecking the Staging File",!
 S XX=$$LSCHECK("CHECKONLY")
 I XX>0 W !,"There are ",XX," incomplete entries in the staging file" D E ;Q
 W !
 S VFDLR6DA=0 F  S VFDLR6DA=$O(^VFD(21626.01,VFDLR6DA)) Q:VFDLR6DA'>0  W "." D LSCFDA(VFDLR6DA)
 Q
LSCFDA(VFDLR6DA) ;
 N VFDTMP,VFDLR6,VFDMSG,VFDFDA,VFDFLDS,FLD,VFDFLDS
 S VFDFLDS=".01;.02;.03;.09;.14;.42;.43;.44;.45"
 D GETS^DIQ(21626.01,VFDLR6DA_",",VFDFLDS,"IE","VFDTMP","VFDMSG")
 M VFDLR6=VFDTMP(21626.01,VFDLR6DA_",")
 F I=1:1 S FLD=$P(VFDFLDS,";",I) Q:FLD'>0  F S="I","E" S VFDLR6(FLD,S)=$G(VFDLR6(FLD,S))
 I $G(VFDLR6(.09,"I"))'="B" Q  ;type '= both
 N VFDFDA,Z,VFDMSG,VFDVIEN
 S Z(.01)=VFDLR6(.03,"I"),Z(.02)=VFDLR6(.14,"I")
 S Z(.025)=VFDINSDA,Z(.05)=VFDLR6(.43,"I"),Z(.06)=VFDLR6(.44,"I")
 S Z(.07)=VFDLR6(.45,"I"),Z(5.1)=VFDLR6(.01,"I"),Z(5.2)=VFDLR6(.02,"E")
 M VFDFDA(62.9001,"?+1,"_VFDLSCDA_",")=Z
 ;D Q("VFDFDA") W !
 D UPDATE^DIE("S","VFDFDA","VFDVIEN","VFDMSG")
 ;D Q("VFDFDA"),Q("VFDVIEN")
 I $D(VFDMSG) D Q("VFDMSG"),Q("VFDFDA") D E
 K VFDFDA W "."
 Q
E N DIR S DIR(0)="EO",DIR("A")="<CR> - Continue" D ^DIR
 Q
 ;
Q(VAL) ; DISPLAY VAR WITH $Q
 N X,X1 S (X,X1)=VAL
 Q:'$D(@VAL)
 I $E(X,$L(X))=")" S X1=$E(X,1,$L(X)-1)
 I $D(@X)#10 W !,X,"=",@X
 F  S X=$Q(@X) Q:X'[X1  W !,X,"=",@X
 Q
CHANGES ;
 ;25May10 no laygo on LAB Field of Staging file
 ;changed DATANAME to DESIRED DATANAME
 ; update flag value of DONE 'NA'NP'SO' with HL7X when item added to HL7X tables 
 ; modified MOVE to be restricted to choices OF DONE NA, DONE NP
 ;28 May corrected display of panels
 ;VFDHHL6B created to split routine functions/space
 ;corrected AutoInst DA setting to be =AIDA from=92 (a result of new seletion not hard coded 92)
 ;AILOAD Modified runtime reporting of problems to be a hang, not a stop needing user keystroke.
 ;AILOAD changed lookup in the AI file  to be the ^LAB(62.4,AIDA,"AC",code index on field 6 UI CODE
 ; 9Jan11 entered corrections for Excel uploading on mode critera testing and refinements.
 ; 3 Mar 11 added 'Exact Match' for Add only Update only criteria
 ; 29 Mar 11 added code to ZZ LAB name and print
 ; 23 mar 12 added fields loading .41-.45
 ; 27 Mar 12 added sections to stuff LAB SHIPPING CONFIGURATION file 62.9
 ; 26 APR 12 changed division selection over to institution selection.
 ; 13 Jun 13 added $G to LSCHECK code
 ; 18 Jun 13 I $G(VFDER)
 ; " added ,$G(VFDLR6(XFLD,"E")
