VFDHHL6B ;DSS/PDW -HL7X LAB 60 STAGING LOAD, AI, FIXES   ; 02 Mar 2012 13:04
 ;;2012.1.1;VENDOR - DOCUMENT STORAGE SYS;;24 Jun 2013;Build 136
 ;Copyright 1995-2013,Document Storage Systems Inc. All Rights Reserved
 ;DSS/BUILD - VFDHHL HL7 EXCHANGE 2012.1.1 * 06/23/15 * vxdev64v2k8_VX13 * 2012.1 T22
 ;DSS/BUILD - VFDHHL HL7 EXCHANGE 2012.1.1 * 02/02/14 * vxdev64v2k8_DEVXOS * 2012.1.1T21
LOAD ; 
 N DELIM,TAB
 ;read in an Excel sheet with $C(9) as a delimiter
LIST ;
 N FLDS,VCUR,VLDSEQ
 S FLDS=".01;.02;.03;.04;.05;.06;.07;.08;.09;.1;.12;.13;.14;.15;.16;.17;.18;.19;.31;.32"
 S FLDS=FLDS_";.41;.42;.43;.44;.45"
 ;DSS/PDW add specimen fields 20131106
 S FLDS=FLDS_";.51;.52;.53;.54;.55"
 ;end DSS/PDW add specimen fields 20131106
 W !,"The order of columns is:"
 F I=1:1 S FLD=$P(FLDS,";",I) Q:FLD'>0  S VCUR(FLD)=""
 N ROW,ITEMC1,ITEMC2,ROWLAST
 S FLD=0 F I=1:1 S FLD=$O(VCUR(FLD)) Q:FLD'>0  S FLDSEQ(I)=FLD
 S I=I-1,ROWLAST=(I\2)+(I#2)
 F ROW=1:1:ROWLAST D
 . S ITEMC1=ROW,FLD=FLDSEQ(ITEMC1) ;ROW COLUMN 1
 . D FIELD^DID(21626.01,FLD,"","LABEL","NM")
 . W !,ITEMC1,?3
 . W:ITEMC1>26 "A" S CC=ITEMC1#26 W $S(CC=0:"Z",1:$C(CC+64))
 . W ?6,FLD,?11,$E(NM("LABEL"),1,20)
 . ;S ITEMC2=ROW*2 I '$D(FLDSEQ(ITEMC2)) Q  ; ROW COLUMN 2
 . S ITEMC2=ROWLAST+ROW I '$D(FLDSEQ(ITEMC2)) Q  ; ROW COLUMN 2
 . S FLD=FLDSEQ(ITEMC2)
 . D FIELD^DID(21626.01,FLD,"","LABEL","NM")
 . W ?40,ITEMC2,?43 W:ITEMC2>26 "A" S CC=ITEMC2#26 W $S(CC=0:"Z",1:$C(CC+64))
 . W ?46,FLD,?51,$E(NM("LABEL"),1,20)
 K DIR S DIR(0)="SO^A:Add New(s);U:Update Only [NO NEW(s)]" D ^DIR
 I Y'="A",Y'="U" Q
 S MODE=Y
 D CHUILOAD^VFDHHL01
LOADG S G=$NA(^TMP("VFDHCHUI",$J))
 I $O(@G@("A"),-1)'>0 W !!,"No Data to Load" D E Q
 W !!,"Filing Rows into scratch entries",!!
 F I=1:1:5 W !,I,!,@G@(I,0) Q:'$D(@G@(I+1,0))
 K DIR S DIR(0)="YO",DIR("A")="IS THE ABOVE CORRECT ?",DIR("B")="N"
 D ^DIR I Y'=1 W !,"QUITING",! Q
 S TAB=$C(9)
 S DELIM=TAB ; ; set delimiter
 W !!,?10,">> Mode is: ",$S(MODE="U":"Update Only <<",1:"New Only <<"),!
 S ROW=0 F  S ROW=$O(@G@(ROW)) Q:ROW'>0  D
 .S XX=@G@(ROW,0)  Q:XX'[DELIM  D FILEROW(XX) ;W !,ROW,?8,XX
 W !!
 Q
FILEROW(XX) ;
 ; this list of fields is used as the key for exporting and importing
 ; nulls coming in do not remove existing field values.
 ;S FLDS=".01;.02;.03;.04;.05;.06;.07;.08;.09;.1;.12;.13;.14;.15;.16;.17;.18;.19;.31;.32"
 N MSG,FDA,VIEN,VXIEN
 N ID,CLNM,IEN60,TOP,COLL,DATANM,SUB,ATOMCOS,TYPE,SYNM
 N NM60,URG,ACC,PRTNM,FLG1,FLG2,FLG3,SYNKEY,INTFACE,PRTORD
 N NVLC,SHPCFG,SPCCNT,SHPCND,PKGCNT,RFRNCLW,RFRNCHG,CRTCLW,CRTCLHG,UNTS
 ;XX is passed in, DELIM is $C(9) TAB ised in excell
 ;the yy="variable=N" passed into the GET(yy
 ;      sets variable=$P(XX,"^",N)
 F YY="ID=1","CLNM=2","IEN60=3","TOP=4","COLL=5" D GET(XX,DELIM,YY)
 F YY="DATANM=6","SUB=7","ATOMCOS=8","TYPE=9","SYNM=10" D GET(XX,DELIM,YY)
 F YY="NM60=11","URG=12","ACC=13","PRTNM=14" D GET(XX,DELIM,YY)
 F YY="FLG1=15","FLG2=16","FLG3=17","SYNKEY=18" D GET(XX,DELIM,YY)
 F YY="INTFACE=19","PRTORD=20" D GET(XX,DELIM,YY)
 F YY="NVLC=21","SHPCFG=22","SPCCNT=23","SHPCND=24","PKGCNT=25" D GET(XX,DELIM,YY)
 ;DSS/PDW NOV 8 2013 EXPAND INTO lAB 60 FLD 100 SITE/SPECIMEN
 F YY="RFRNCLW=26","RFRNCHG=27","CRTCLW=28","CRTCLHG=29","UNTS=30" D
 .D GET(XX,DELIM,YY)
 ;end DSS/PDW NOV 8 2013 EXPAND INTO lAB 60 FLD 100 SITE/SPECIMEN
 ;mode determines either only update existing or add new entries
 S IENS=$S(MODE="U":"?1,",1:"?+1,")
 ;entered testing for mode Add New or Update only 9Jan11
 N DIC,X S DIC=21626.01,X=ID,DIC(0)="XQM" D ^DIC
MODE I MODE="U",Y<0 W !,?5,ID,?20,"Is NEW - not filed" Q
 I MODE="A",Y>0 W !,?5,ID,?20,"Exists - not filed" Q
 S Z(.01)=ID,Z(.02)=CLNM,Z(.03)=IEN60,Z(.04)=TOP,Z(.05)=COLL
 S Z(.06)=DATANM,Z(.07)=SUB,Z(.08)=ATOMCOS,Z(.09)=TYPE,Z(.1)=SYNM
 S Z(.12)=NM60,Z(.13)=URG,Z(.14)=ACC,Z(.15)=PRTNM,Z(.16)=FLG1
 S Z(.17)=FLG2,Z(.18)=FLG3,Z(.19)=SYNKEY,Z(.31)=INTFACE,Z(.32)=PRTORD
 S Z(.41)=NVLC,Z(.42)=SHPCFG,Z(.43)=SPCCNT,Z(.44)=SHPCND,Z(.45)=PKGCNT
 ;DSS/PDW NOV 8 2013 EXPAND INTO lAB 60 FLD 100 SITE/SPECIMEN
 S Z(.51)=RFRNCLW,Z(.52)=RFRNCHG,Z(.53)=CRTCLW,Z(.54)=CRTCLHG,Z(.55)=UNTS
 ;end DSS/PDW NOV 8 2013 EXPAND INTO lAB 60 FLD 100 SITE/SPECIMEN
 ; DO NOT allow nulls to delete current contents
 N FLD S FLD=0 F  S FLD=$O(Z(FLD)) Q:FLD'>0  I Z(FLD)="" K Z(FLD)
 M FDA(21626.01,IENS)=Z
FILE D UPDATE^DIE("S","FDA","VIEN","MSG")
 ;D Q("NM"),Q("FDA"),Q("VIEN"),Q("MSG")
 ;I $D(MSG) D Q("FDA"),Q("VIEN"),Q("MSG") D E
 I '+$D(VIEN) W !,Z(.01),?15,"Error:NOT FILED" D Q("Z") Q
 W !,?5,Z(.01),?20,"Filed"
 K MSG,FDA
 Q
AILOAD ; load result codes into the AI file
 K DIC
 S DIC=21625.01,DIC(0)="AEQM",DIC("S")="I $D(^VFDH(21625.01,+Y,30,""B"",""VXLRESULT""))"
 D ^DIC
 Q:Y'>0
 S VFDHSYS=+Y
AISEL K DIC
 S DIC=62.4,DIC(0)="AEQM",DIC("S")="I $E($P(^(0),U),1,5)=""VFDLA""" D ^DIC
 G:Y'>0 AILOAD
 S VFDAISYS=+Y
 W !!,"HL7 EXCHANGE FILE:",?23,$$GET1^DIQ(21625.01,VFDHSYS_",",.01)
 W !,"AUTO INSTRUMENT FILE:",?23,$$GET1^DIQ(62.4,VFDAISYS_",",.01)
 K DIR S DIR(0)="YO",DIR("A")="Is the above correct?",DIR("B")="N" D ^DIR
 I Y'=1 G AISEL
 ;process vxlresult codes into AI file using VFDSYS and VFDAISYS
 S VFDHTAB="VXLRESULT",AIDA=VFDAISYS
 D TABPATH^VFDHHLOT(.VFDHRTN,VFDHSYS,VFDHTAB) ;21625.0106^501,15,
 S FILENUM=+VFDHRTN,TABIENS=$P(VFDHRTN,U,2)
 S GBL=$$ROOT^DILFD(FILENUM,","_TABIENS,1) ;^VFDH(21625.01,15,30,501,10)
 ;^VFDH(21625.01,15,30,501,10,1,0)="3000100^BAND NEUTROPHILS^12^70^3"
 ;^VFDH(21625.01,15,30,501,10,2,0)="25000000^ GLUCOSE^230^72^1"
 S RESDA=0 F  S RESDA=$O(@GBL@(RESDA)) Q:RESDA'>0  D AISTUFF1(RESDA)
 Q
 ;***
AISTUFF1(RESIEN) ;Stuff the entry RESIEN into the AI file
 I $G(FILENUM) G AISTUFFC
 N VFDHSYS,VFDHTAB,FILENUM,IENS,RSLT,RES,LN,FLDVAL,FLD,VAL,XX,AI,DN,AI,FDA
 S VFDHTAB="VXLRESULT"
 D TABPATH^VFDHHLOT(.VFDHRTN,VFDHSYS,VFDHTAB)
 S FILENUM=+VFDHRTN,TABIENS=$P(VFDHRTN,U,2)
AISTUFFC ; pull values and stuff into AI
 N IENS,FLD,FLDVAL,RESIENS,RESFLD,L60G,L60,L60FLD,LAB
 S RESIENS=RESIEN_","_TABIENS
 ;get HL7X result fields into RESFLD(FLD)=VAL
 D GET^VFDCDDR0(.HL7GFLD,FILENUM,RESIENS,".01;.02;.03","E")
 ;^TMP("DSIC",2844,1)="21625.0106^1,512,15,^.01^3000100^E"
 M HL7FLD=@HL7GFLD
 ;HL7FLD(1)="21625.0106^1,501,15,^.01^3000100^E"
 ;HL7FLD(2)="21625.0106^1,501,15,^.02^BAND NEUTROPHILS^E"
 ;HL7FLD(3)="21625.0106^1,501,15,^.03^12^E"
 S LN=0 F  S LN=$O(HL7FLD(LN)) Q:LN'>0  D
 . S FLD=$P(HL7FLD(LN),U,3),VAL=$P(HL7FLD(LN),U,4)
 . S RESFLD(FLD)=VAL
 ;I $E(IOST)="C" D Q("RESFLD")
 ;get LAB fields LAB(FLD)=VAL
 S L60DA=RESFLD(.03)
 D GET^VFDCDDR0(.L60G,60,L60DA,".01;400;51","E")
 M L60=@L60G
 ;L60(1)="60^12,^.01^BAND NEUTROPHILS^E"
 ;L60(2)="60^12,^51^BANDS^E"
 ;L60(3)="60^12,^400^BANDS^E"
 I $P(@L60G@(1),U)=-1 W !!,"BAD DATA" D Q("RESFLD") H 2 Q
 ;build LAB(fld)=val array
 S LN=0 F  S LN=$O(L60(LN)) Q:LN'>0  D
 . S FLD=$P(L60(LN),U,3),VAL=$P(L60(LN),U,4)
 . S LAB(FLD)=VAL
 ;I $E(IOST)="C" D Q("LAB")
 I '$L(LAB(400)) W !!,RESFLD(.01)," has no data name - skipping",! H 2 Q
 ;setup FDA for AI
 K AIFLD
 S AIFLD(.01)=LAB(.01),AIFLD(6)=RESFLD(.01),AIFLD(14)="YES"
 S AIFLD(18)="YES",AIFLD(19)=LAB(51)_" " ;May 16 2013 add a space (not per Dan)
 K VIENS,MSG,DA
 ;look for existing  entry. Different UI codes can use the same LAB test
 I $D(^LAB(62.4,AIDA,3,"AC",RESFLD(.01))) S (DA,VIENS(1))=$O(^LAB(62.4,AIDA,3,"AC",RESFLD(.01),0))
 I $G(DA),$E(IOST)="C" W !!,RESFLD(.01),?20,AIFLD(.01)," FOUND at: ",DA
 ;file if entry already exists then return to process next record
AIOLD I $G(DA) D  Q
 .S IENS=DA_","_AIDA_","
 .K FDA,MSG
 .M FDA(62.41,IENS)=AIFLD
 .D FILE^DIE("SE","FDA","MSG")
 .I $D(MSG) D Q("MSG"),E
 .D Q($NA(^LAB(62.4,AIDA,3,DA)))
 ;
AINEW ;new record needed
 ;I $E(IOST)="C" D Q("FDA"),Q("MSG")
 K IENS,FDA,MSG
 S IENS="+1,"_AIDA_","
 M FDA(62.41,IENS)=AIFLD
 D UPDATE^DIE("SE","FDA","VIENS","MSG")
 I $E(IOST)="C" W !!,RESFLD(.01),?20,AIFLD(.01),?50,"ADDED: ",VIENS(1) D
 .D Q($NA(^LAB(62.4,AIDA,3,VIENS(1))))
 Q
 ;
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
DIQDISP(FILE,IENS,FLDS) ;display
 K CVALS
 D GETS^DIQ(FILE,IENS,FLDS,"N","CVALS","MSG")
 K VCUR,FLDSEQ
 ;display blanks if entry deleted
CVALS I $D(CVALS) M VCUR=CVALS(FILE,IENS)
 N ROW,ITEMC1,ITEMC2,ROWLAST
VCUR S FLD=0 F I=1:1 S FLD=$O(VCUR(FLD)) Q:FLD'>0  S FLDSEQ(I)=FLD
 S I=I-1,ROWLAST=(I\2)+(I#2)
 F ROW=1:1:ROWLAST D
 . S ITEMC1=ROW*2-1,FLD=FLDSEQ(ITEMC1) ;ROW COLUMN 1
 . D FIELD^DID(FILE,FLD,"","LABEL","NM")
 . W !,$E(NM("LABEL"),1,14),":",?17,$E($G(VCUR(FLD)),1,32)
 . S ITEMC2=ROW*2 I '$D(FLDSEQ(ITEMC2)) Q  ; ROW COLUMN 2
 . S FLD=FLDSEQ(ITEMC2)
 . D FIELD^DID(FILE,FLD,"","LABEL","NM")
 . W ?40,$E(NM("LABEL"),1,14),":",?57,$E($G(VCUR(FLD)),1,32)
 Q
LAB60ZZ ; Add ZZ to all lab routine names & print names entries 1:19999
 S DA=0
 F XX=1:1 S DA=$O(^LAB(60,DA)) Q:DA>19999  D EDIT60(DA)
 Q
EDIT60(DA) ;
 S VFD60NM=$$GET1^DIQ(60,DA,.01),VFD60PN=$$GET1^DIQ(60,DA,51)
 I $E(VFD60NM,1,2)="ZZ" S X=VFD60NM
 I $E(VFD60NM,2)'="Z" S X="Z"_VFD60NM
 I $E(VFD60NM,2)'="Z" S X="Z"_VFD60NM
 I $E(VFD60PN,1,2)="ZZ" S Y=VFD60PN
 I $E(VFD60PN,2)'="Z" S Y="Z"_VFD60PN
 I $E(VFD60PN,2)'="Z" S Y="Z"_VFD60PN
 S Y=$E(Y,1,7)
 S DIE=60,DR=".01///"_X_";51////"_Y
 D ^DIE
 W "." I '(XX#500) W XX,"-",DA
 Q
SYNFIX ; scan and redo the Synonym entries where the 60.1,.01 SYNONYM = 60,.01 name
 ; The .01 test name is not to be in the Synonym list ^LAB(60,,5,  #60.1
 S START=40000
 I '$G(START) W !,"SET variable START = IEN" Q
 S IEN=START-1
 S I=0 F  S IEN=$O(^LAB(60,IEN)) Q:IEN'>0  S LRNM=$P(^LAB(60,IEN,0),U) I $D(^LAB(60,IEN,5,"B",LRNM)) W !,IEN,?10,LRNM S I=I+1 W ?40,I D SYNDEL(IEN)
 Q
SYNDEL(IEN) ; kill off duplicate synonym and reindex 
 N DIK,DA,LRNM
 S LRNM=$$GET1^DIQ(60,IEN_",",.01)
 S DA=$O(^LAB(60,IEN,5,"B",LRNM,0)) I 'DA W "X" Q
 S DA(1)=IEN,DIK=$$ROOT^DILFD(60.1,","_IEN_",") D ^DIK ; kill synonym
 K DIK,DA
 S DIK="^LAB(60,",DIK(1)=".01^B",DA=IEN D EN^DIK ;index .01-B
 Q
ORDFIX ; scan and redo the ORDER entries where the 101.43,.01 SYNONYM = 101.432,.01 name
 ; The .01 test name is not to be in the Synonym list ^ORD(101.43,,2,  #101.43
 N ORDNM,IEN,DIK,START,DA
 S START=3997
 S IEN=START-1
 S I=0 F  S IEN=$O(^ORD(101.43,IEN)) Q:IEN'>0  S ORDNM=$P(^ORD(101.43,IEN,0),U) I $D(^ORD(101.43,IEN,2,"B",ORDNM)) W !,IEN,?10,ORDNM S I=I+1 W ?40,I D ORDDEL(IEN)
 Q
ORDDEL(IEN) ; kill off duplicate synonym and reindex 
 N DIK,DA,LRNM
 S ORDNM=$$GET1^DIQ(101.43,IEN_",",.01)
 S DA=$O(^ORD(101.43,IEN,2,"B",ORDNM,0)) I 'DA W "X" Q
 S DA(1)=IEN,DIK=$$ROOT^DILFD(101.432,","_IEN_",") D ^DIK ; kill synonym
 K DIK,DA
 S DIK="^ORD(101.43,",DIK(1)=".01^B",DA=IEN D EN^DIK ;index .01-B
 Q
ORD2FIX ; scan for duplicates in the B index
 ; if so keep the first with BOTH and edit the other to Niether
 ;
 ; The .01 test name is not to be in the Synonym list ^ORD(101.43,,2,  #101.43
 N ORDNM,IEN,DIK,START,DA
 S START=3997
 S IEN1=START-1
 S I=0 F  S IEN1=$O(^ORD(101.43,IEN1)) Q:IEN1'>0  S ORDNM=$P(^ORD(101.43,IEN1,0),U) S IEN2=$O(^ORD(101.43,"B",ORDNM,IEN1)) I IEN2 D
 .S I=I+1 W !,I,?5,IEN1,?15,ORDNM  D ORD2NIT(IEN1,IEN2)
 Q
ORD2NIT(IEN1,IEN2) ; edit to neither
 N X1,X2
 S X1=$$GET1^DIQ(101.43,IEN1,60.7,"I"),X2=$$GET1^DIQ(101.43,IEN2,60.7,"I")
 W ?35,X1,?40,IEN2,?50,X2
 I X1'="N",X2'="N" I 1
 E  Q
 W ?55,"fix"
 ; set IEN2 to N
 N VFDFDA,VMSG
 S VFDFDA(101.43,IEN2_",",60.7)="N" D Q("VFDFDA") W !
 D FILE^DIE(,"VFDFDA","VMSG")
 I $D(VMSG) D Q("VMSG") D E
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
 ; 2 Jun 2012 add code to correct duplicate in synonym SYNFIX
 ; 4 JUN 12 add code to remove duplicates in ORD(101.43 field 2
 ; 5 Jun 12 add code to edit 101.43 duplicates type to Neither
 ; 16 MAY 2013 append a space to the field 19 Remark Prefix from file 60 #51 Print Name
 ; 06 Nov 2013 add in fields to expand site/specimen in file 60 field #100
 ; 22 Nov 2013 changed calls from DISCDDR0 to VFDCDDR0
