VFDHHLMP ;DSS/PDW - META DICTIONARY LOADER;09 Mar 2011 13:31
 ;;2012.1.1;VENDOR - DOCUMENT STORAGE SYS;;24 Jun 2013;Build 136
 ;Copyright 1995-2013,Document Storage Systems Inc. All Rights Reserved
 ;DSS/BUILD - VFDHHL HL7 EXCHANGE 2012.1.1 * 06/23/15 * vxdev64v2k8_VX13 * 2012.1 T22
 ;DSS/BUILD - VFDHHL HL7 EXCHANGE 2012.1.1 * 02/02/14 * vxdev64v2k8_DEVXOS * 2012.1.1T21
 ;Copyright 1995-2010,Document Storage Systems Inc.,All Rights Reserved
 ;.01          NAME                             D0,0                      1    F
 ;.02          PARENT FILE                        "                       2    P
 ;.03          FILE/SUB-FILE NUMBER               "                       3    N
 ;.04          FILE/SUB-FILE NAME                 "                       4    F
 ;.05          FIELD NUMBER                       "                       5    N
 ;.06          FIELD NAME                         "                       6    F
 ;.07          DD ID                              "                       7    F
 ;1.01         PATH                             D0,1                      1    F
 ;1.02         BRIEF PATH                         "                       2    F
START ; select from DIC
 ;Loop till field selected
 ; accumulate iens, tree, levels
 ;loc(I,"FILE")=;LOC(I,"FLD")=,LOC(I,"FLDNM")=
 ;
 ;LOC(3,"BRIEF")=PTNT:LS:LS  ==> PTNT:ALS:ALS
 ;LOC(3,"DDZ")=ALIAS^MF^^0;1^K:$L(X)>30!($L(X)<3) X I $D(X) S DG20NAME=X,(X,DG20NA
 ;ME)=$$FORMAT^XLFNAME7(.DG20NAME,3,30) K:'$L(X) X,DG20NAME
 ;LOC(3,"FILE")=2.01 ==> "FILENUM" ?
 ;LOC(3,"FILENM")=ALIAS
 ;LOC(3,"FLD")=.01
 ;LOC(3,"FLDNM")=ALIAS
 ;LOC(3,"ID")=2:1:.01
 ;LOC(3,"MULT")=0
 ;==> LOC (3,"ENTRY")="2.01,.01"
 ;PATH=PATIENT:ALIAS:ALIAS
EN ;
 N DIC,LOC W @IOF
EN1 ;
 W !!,?10,"Add a New Meta Dictionary 'ITEM'",!
 W !!,"Select File:",!,"or enter '**' to review Meta Dictionary",!
 K DIR S DIR(0)="FO",DIR("A")="Select" D ^DIR
 I Y["," D  G EN1
 .Q
 .N DD,FLD,LOC,DDLOC,L
 .S DD=$P(Y,","),FLD=$P(Y,",",2)
 .I '$D(^DD(DD,FLD)) W !,"NOT FOUND:",?20,Y Q
 .D DDTREE(DD,FLD)
 .S L=$O(LOC("A"),-1)
 .F I=L:-1:0 W !,?((L-I)*5),LOC(I,"DDNM")
 .D DDTREE
 I X="**" D  G EN1
 .F  K DIC S DIC=21625.02,DIC(0)="AEQM" D ^DIC S DA=+Y Q:DA'>0  D EN^DIQ
 S DIC=1,DIC(0)="EQZM"
 D ^DIC
 Q:Y'>0
 S DIC=^DIC(+Y,0,"GL")
 N LOC,LEV,STOP,PATH,I,DIR,DIC,X,MSG
 S LEV=0
 S LOC(LEV,"FILE")=1,LOC(LEV,"FLD")=0,LOC(LEV,"FLDNM")="FILE"
 S LEV=1
 S LOC(LEV,"FILE")=+Y,LOC(LEV,"FLD")=+Y,LOC(LEV,"FLDNM")=Y(0,0),LOC(LEV,"FLDPAR")=LOC(LEV-1,"FLDNM")
 S LOC(LEV,"DDZ")=U_+Y
 S X="" F II=1:1:LEV S X=X_LOC(II,"FLD")_$S(II<LEV:",",1:"")
 S LOC(LEV,"ID")=X
FLDEN ;entry point for selecting another field at the same level after reseting LEV and LOC
 D FLDSEL I $G(STOP) K LEV,LOC G EN1 ;return to file selection
 S X="" F II=1:1:LEV-1 S X=X_LOC(II,"FLDNM")_"/"
 S PATH=X_LOC(LEV,"FLDNM")
 W @IOF
 W !,"Review Field Selected",!!
 S PATH=LOC(LEV,"PATH"),BRFPATH=$$BRIEF^VFDHHLOT(PATH)
 F I=1:1:LEV W !,?I,LOC(I,"FLD"),?(I+10),LOC(I,"FLDNM")
 W !!,"FIELD:",?10,LOC(LEV,"FLDNM"),?35,"ID: ",?40,LOC(LEV,"ID")
 W !,"PATH:",?13,PATH
 W !,"BRIEF PATH:",?13,BRFPATH
 I $D(^VFDH(21625.02,"D",BRFPATH)) W !,?5,">> The Above Already ExiSts" D E D LEVBACK G FLDEN ;***
 ;I $D(^VFDH(21625.02,"D",BRFPATH)) W !,?5,">> The Above Already ExiSts" D E G FLDEN ;***
 K DIR S DIR(0)="YO",DIR("A")="Is the above correct ? ",DIR("B")="Y"
 D ^DIR
 I Y=1 D FILE
 D LEVBACK G FLDEN ;***
 G EN1
 Q
FLDSEL ; loops down into the file with go commands
 S LEV=LEV+1
 N DIC,DD,MULT
 S PATH=""
 F II=1:1:LEV-1 S PATH=PATH_LOC(II,"FLDNM")_"/"
 W !!,PATH,!
 I LEV=1 S DD=LOC(LEV-1,"FILE") I 1
 E  S DD=+$P(LOC(LEV-1,"DDZ"),U,2)
 ;D Q("LEV"),Q("DD")
 S DIC="^DD("_DD_",",DIC(0)="AEQMZI"
 S DIC("DID01")="W ""   "",$$EXT^DIC2(0,.01,$P(^DD(DI,Y,0),U))"
 S DIC("S")="I Y>.001,$P(^(0),U,2)'[""C"" Q:'$D(^(9))  I ^(9)'=U"
 S DIC("W")="S %=$P(^(0),U,2) I % W $S($P(^DD(+%,.01,0),U,2)[""W"":""  (word-processing)"",1:""  (multiple)"")"
 D
 .N DD
 .D ^DIC
 I Y'>0 S STOP=-1 Q
 ;I Y'>0 D LEVBACK G FLDEN ;K LOC(LEV) S STOP=-1 Q  ;***
 S LOC(LEV,"FILE")=DD,LOC(LEV,"FLD")=+Y,LOC(LEV,"FLDNM")=Y(0,0)
 S LOC(LEV,"DDZ")=^DD(DD,+Y,0),LOC(LEV,"FILENM")=LOC(LEV-1,"FLDNM")
 S LOC(LEV,"PATH")=PATH_LOC(LEV,"FLDNM"),LOC(LEV,"BRIEF")=$$BRIEF^VFDHHLOT(LOC(LEV,"PATH"))
 S X="" F II=1:1:LEV S X=X_LOC(II,"FLD")_$S(II<LEV:",",1:"")
 S LOC(LEV,"ID")=X
 N FIELD,ATTRIB
 S ATTRIB="TYPE;GLOBAL SUBSCRIPT LOCATION;MULTIPLE-VALUED;TITLE;LABEL"
 D FIELD^DID(LOC(LEV,"FILE"),LOC(LEV,"FLD"),"Z",ATTRIB,"FIELD","MSG")
 S LOC(LEV,"MULT")=FIELD("MULTIPLE-VALUED")
 ;M LOC(LEV,"DID")=FIELD
 ;D Q("LOC")
 I LOC(LEV,"MULT") G FLDSEL
 Q
FILE ;
 S XX=$NA(LOC(LEV))
 ;LOC(3,"BRIEF")=PTNT:LS:LS
 ;LOC(3,"DDZ")=ALIAS^MF^^0;1^K:$L(X)>30!($L(X)<3) X I $D(X) S DG20NAME=X,(X,DG20NA
 ;ME)=$$FORMAT^XLFNAME7(.DG20NAME,3,30) K:'$L(X) X,DG20NAME
 ;LOC(3,"FILE")=2.01
 ;LOC(3,"FILENM")=ALIAS
 ;LOC(3,"FLD")=.01
 ;LOC(3,"FLDNM")=ALIAS
 ;LOC(3,"ID")=2:1:.01
 ;LOC(3,"MULT")=0
 ;PATH=PATIENT:ALIAS:ALIAS
 N PATH,FDA,VIEN
 S PATH=LOC(LEV,"PATH"),BRFPATH=$$BRIEF^VFDHHLOT(PATH)
 I $D(^VFDH(21625.02,"D",BRFPATH)) W !!,">> The Above Already Exists" D E Q
 N Z,FLD,FDAMSG
 M FLD=LOC(LEV)
 ;   .01          NAME
 ;   .02          PARENT FILE
 ;   .03          FILE/SUB-FILE NUMBER
 ;   .04          FILE/SUB-FILE NAME
 ;   .05          FIELD NUMBER
 ;   .06          FIELD NAME
 ;   .07          DD ID
 ;   1.01         PATH
 ;   1.02         BRIEF PATH
UPDATE ;
 S Z(.01)=FLD("FILE")_","_FLD("FLD")_" "_$$BRIEF^VFDHHLOT(PATH)
 S Z(.02)=$$GET1^DIQ(1,LOC(1,"FILE")_",",.01),Z(.03)=FLD("FILE"),Z(.04)=FLD("FILENM")
 S Z(.05)=FLD("FLD"),Z(.06)=FLD("FLDNM"),Z(.07)=FLD("ID") S Z(1.01)=PATH,Z(1.02)=$$BRIEF^VFDHHLOT(PATH)
 M FDA(21625.02,"?+1,")=Z
 D UPDATE^DIE("E","FDA","VIEN","MSG")
 I $D(MSG) D Q("MSG"),Q("FDA") D E Q
 ;D Q("VIEN")
 W !,"ITEM ADDED: ",?15,FLD("FLDNM"),?45,FLD("PATH") ;D Q("Z")
 Q
LEVBACK ; backup one level ":"
 K LOC(LEV) S LEV=LEV-1
 Q
HL7XMAP ; MAP ELEMENTS WITHIN HL7 EXCHANGE
DDTREE ;build array for mapping from DDTREE LOC array
 M DDTREE=LOC N LOC,LL
 S LL=$O(DDTREE("A"),-1)
 S X=DDTREE(LL,"DDNM") ;
 S DIC=1,DIC(0)="EQZM"
 D ^DIC
 Q:Y'>0
 S DIC=^DIC(+Y,0,"GL")
 N LOC,LEV,STOP,PATH,I,DIR,DIC,X,MSG
 S LEV=0
 S LOC(LEV,"FILE")=1,LOC(LEV,"FLD")=0,LOC(LEV,"FLDNM")="FILE"
 S LEV=1
 S LOC(LEV,"FILE")=+Y,LOC(LEV,"FLD")=+Y,LOC(LEV,"FLDNM")=Y(0,0),LOC(LEV,"FLDPAR")=LOC(LEV-1,"FLDNM")
 S LOC(LEV,"DDZ")=U_+Y
 S X="" F II=1:1:LEV S X=X_LOC(II,"FLD")_$S(II<LEV:",",1:"")
 S LOC(LEV,"ID")=X
 Q
EDTELEM ;EP EDIT ELEMENTS
 N X1,X2
 F  S X1=$$SELTRAN^VFDHHLOE() Q:X1'>0  D
 . F  S X2=$$SELSEG^VFDHHLOE(X1) Q:X2'>0  D
 .. S DA=X2,DA(1)=X1,DR="[VFDHHLO ELEMENTS EDIT]"
 .. S DDSFILE=21625.01,DDSFILE(1)=21625.0101
 ..D ^DDS
 Q
MAPEDIT ;EP EDIT ELEMENTS
 N X1,X2
 F  S X1=$$SELTRAN^VFDHHLOE() Q:X1'>0  D
 . F  S X2=$$SELSEG^VFDHHLOE(X1) Q:X2'>0  D
 .. F  S X3=$$SELELMT^VFDHHLOE(X1,X2) Q:X3'>0  D ELMTMAP
 Q
ELMTMAP ;
 N DA,DIE,DIC,DR
 S DA=X3,DA(1)=X2,DA(2)=X1 D Q("DA")
 S IENS=$$IENS^DILF(.DA)
 S DIE=$$ROOT^DILFD(21625.0102,IENS),DIC(0)="AEQM"
 S DR="1.01;10"
 D ^DIE
 Q
GET(REC,DLM,XX) ; where XX = VAR_"="_I  ex: XX="PATNM=1"
 ; Set VAR = piece I of REC using delimiter DLM
 N Y,I S Y=$P(XX,"="),I=$P(XX,"=",2),@Y=$P(REC,DLM,I)
 Q
SET(REC,DLM,XX) ; where XX = VAR_U_I  ex: XX="1=PATNUM"
 ; Set VAR into piece I of REC using delimiter DLM
 N Y,I S I=$P(XX,"="),Y=$P(XX,"=",2)
 I Y'=+Y,Y'="" S $P(REC,DLM,I)=$G(@Y) I 1
 E  S $P(REC,DLM,I)=YOUT ;
 Q
Q(VAL) ; DISPLAY VAR WITH $Q
 N X,X1 S (X,X1)=VAL
 Q:'$D(@VAL)
 I $E(X,$L(X))=")" S X1=$E(X,1,$L(X)-1)
 I $D(@X)#10 W !,X,"=",@X
 F  S X=$Q(@X) Q:X'[X1  W !,X,"=",@X
 Q
E K DIR S DIR(0)="EO",DIR("A")="<CR> - Continue" D ^DIR K DIR Q
REDO ; change old form to new forms for .01
 N VFDTMP,VFDFLD,VFDFDA,Z
 S IEN=0
 F  S IEN=$O(^VFDH(21625.02,IEN)) Q:IEN'>0  S IENS=IEN_"," K VFDTMP,VFDFLD D REDOIEN
 Q
REDOIEN ;
 K VFDFDA,Z,VFDFLD,VFDTMP
 D GETS^DIQ(21625.02,IENS,"**","","VFDTMP","MSG")
 M VFDFLD=VFDTMP(21625.02,IENS)
 S VFDFLD(1.01)=$TR(VFDFLD(1.01),":","/"),VFDFLD(1.02)=$TR(VFDFLD(1.02),":","/")
 S VFDFLD(.07)=$TR(VFDFLD(.07),":",",")
 K Z,VFDFDA
 S Z(.01)=VFDFLD(.03)_","_VFDFLD(.05)_" "_VFDFLD(1.02)
 S Z(1.01)=VFDFLD(1.01),Z(1.02)=VFDFLD(1.02),Z(.07)=VFDFLD(.07)
 M VFDFDA(21625.02,IENS)=Z
 D UPDATE^DIE("S","VFDFDA",IENS,"MSG")
 Q
