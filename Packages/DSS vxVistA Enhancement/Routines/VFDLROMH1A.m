VFDLROMH1A ;DSS/TFF - Cerner to vxVistA Flat File Parser Utilities;10/03/2013
 ;;2013.1;VENDOR - DOCUMENT STORAGE SYS;**27**;05 May 2014;Build 3
 ;Copyright 1995-2014,Document Storage Systems Inc. All Rights Reserved
 ;
 ;
 ;
 ;
 Q
 ;
 ;============================================================================
 ;                  FLAT FILE PARSER USER DIALOG AND HELP
 ;============================================================================
 ;
DOTS(MD) ; WRITE DOTS TO THE SCREEN
 N WR D SWITCH("|TNT|") S WR="W "_$S($D(MD):"!,MD,!",1:""".""")
 X WR D SWITCH("PARSE")
 Q
 ;
HELP ; PIECE HELP CODE
 N I W !! F I=1:1 Q:$P($T(PIECE+I),";;",2)=""  D
 .W I," ",$P($T(PIECE+I),";;",2),!
 Q
 ;
PIECE ; TEST PIECES BY CERNER EXTRACT HEADER
 ;;ORDER_ID
 ;;ACCESSION_NBR
 ;;LAST_UPDATE_PROVIDER_ID
 ;;PHYSICIAN_FIRSTNAME
 ;;PHYSICIAN_LASTNAME
 ;;SPECIMEN_CODE
 ;;SPECIMEN
 ;;TASK_ASSAY_CD
 ;;TNAME
 ;;CATALOG_CD
 ;;RESULT
 ;;UNITS_CD
 ;;TUNITS
 ;;VERIFIED_DT_TM
 ;;CURRENT_START_DT_TIME
 ;;VERIFIED_PERSON_ID
 ;;VERIFIED_FIRSTNAME
 ;;VERIFIED_LASTNAME
 ;;NORMAL_HIGH
 ;;NORMAL_LOW
 ;;CRITICAL_LOW
 ;;CRITICAL_HIGH
 ;;STATE_ID
 ;;FAC_CD
 ;;CASE_NUM
 ;;PERSON_ID
 ;;PersonFirstName
 ;;NAME_LAST
 ;;SEX_CD
 ;;DESCRIPTION
 ;;BIRTH_DT_TM
 ;;NORMALCY_CD
 ;;NORMALCY
 ;;FASTING
 ;;PRIORITY
 ;;LOCATION_UNIT_CODE
 ;;LOCATION_UNIT_CODE_DESCRIPTION
 ;;RESULT_COMMENTS
 ;;SPECIMENT_COMMENTS
 ;;RESULT_FOOT_NOTE
 ;;INTERPRETATIVE_NOTE_FLAG
 ;;INTERPRETATIVE_NOTE_TEXT
 ;;CollectionSample_code
 ;;CollectionSample_name
 ;;
 Q
 ;
ASKPATH(DF) ; FUNCTION TO GET THE HFS PATH
 N DIR,DIRUT,X,Y,SL S:$G(DF)="" DF=$G(^VFDLRT("ASKPATH"))
 S DIR("A")="Set PATH",DIR(0)="FOr^1:255",DIR("B")=$G(DF) D ^DIR
 I $D(DIRUT) Q ""
 F SL="/","\" I Y[SL&($E(Y,$L(Y))'[SL) S Y=Y_SL
 S ^VFDLRT("ASKPATH")=Y
 Q Y
 ;
ASKFILE(DF) ; FM DIC TO GET THE FILENAME FROM THE USER
 N DIR,DIRUT,X,Y S:$G(DF)="" DF=$G(^VFDLRT("ASKFILE"))
 W ! S DIR("A")="Filename"
 S DIR(0)="FrO^1:100",DIR("B")=$G(DF) D ^DIR
 I $D(DIRUT) Q ""
 S ^VFDLRT("ASKFILE")=Y
 Q Y
 ;
 ;============================================================================
 ;                       FLAT FILE PARSER UTILITIES
 ;============================================================================
 ;
GETUSER(ID) ; IDENTIFIY A PROVIDER FROM THE VFD X-REF OF FILE 200
 ;            NOT FOUND OR NOT UNIQUE RETURN 0
 ;
 N VFD,FAIL,FLG Q:'$D(^VA(200,"VFD",ID)) 0
 S VFD="" F  S VFD=$O(^VA(200,"VFD",ID,VFD)) Q:VFD=""  D  Q:$D(FAIL)
 .I +$G(FLG)'=0&($G(FLG)'=VFD) S FAIL=1 Q
 .S FLG=VFD
 I $D(FAIL) Q 0
 Q +$G(FLG)
 ;
 ;============================================================================
 ;                             PARSE END REPORT
 ;============================================================================
 ;
REPORT(D) ;
 N FLE,PATH,FOUT,POP,MSGID,TSTID,PM
 S FLE=$$UP^XLFSTR(^VFDLRT("ASKFILE")),PATH=^VFDLRT("ASKPATH")
 S FOUT="Cerner_ExceptionLog_"_$P($$NOW^XLFDT,".")_".txt"
 D OPEN^%ZISH("FILE1",PATH,FOUT,"A") Q:POP
 U IO D HEADER^VFDLROMH1 W !!
 S MSGID=""  F  S MSGID=$O(^VFDLRT(FLE,"PANEL",MSGID)) Q:MSGID=""  D
 .I ^VFDLRT(FLE,"PANEL",MSGID)="F" D
 ..W "GROUP "_MSGID_" HAS FAILED! ===========================================",!!
 ..S TSTID="" F  S TSTID=$O(^VFDLRT(FLE,"B",MSGID,TSTID)) Q:TSTID=""  D
 ...W "TEST: "_TSTID,!
 ...W "---------------------------------------------------------------",!!
 ...W "RECORD",!!,$TR(^VFDLRT(FLE,MSGID,TSTID,0),D,U),!!
 ...W "PROCESS MESSAGE",!!
 ...S PM=0 F  S PM=$O(^VFDLRT(FLE,MSGID,TSTID,1,PM)) Q:'PM  D
 ....W ^VFDLRT(FLE,MSGID,TSTID,1,PM),!
 ...W "---------------------------------------------------------------",!!
 ..W "=======================================================================",!!
 D CLOSE^%ZISH("FILE1")
 Q
 ;
EXCEPT(DESC,DATA) ; SAVE EXCEPTION TO 21603
 ;
 ;VXDT=[Optional] date.time of exception (FileMan internal format)
 ;APPL=[Optional] Application name
 ;DESC=[Optional] Short description
 ;HLID=[Optional] Associated HL7 message control ID
 ;SVER=[Optional] Severity code -
 ;                     1:FATAL
 ;                     2:WARNING
 ;                     3:INFORMATIONAL
 ;                     4:DEBUG ONLY
 ;                     5:VERBOSE DEBUG
 ;                     6:VERY VERBOSE
 ;                     9:OTHER
 ;DATA=[Optional] Summary data, up to 80 characters
 ;VFDXVARS=[Optional] Array (by reference) =or= $NAME(Array),
 ;     subscripted 1, 2, 3, ... where each value names a variable
 ;     to be saved.  Alternatively, ARRAY(1)="*" to save ALL.
 ;
 ;
 N ARRAY S ARRAY(2)="*"
 D XCPT^VFDXX($$NOW^XLFDT,"CERNER MIGRATION",DESC,,1,$G(DATA),.ARRAY)
 Q
 ;
TESTCT(TYP,S) ; FAILED TEST COUNT
 N FLE,I,CT,PAN,TST,TFAL,FLG S TYP=+$G(TYP)
 S FLE=$$UP^XLFSTR($G(^VFDLRT("ASKFILE"))) Q:FLE=""
 I TYP=-1 D  W:'$D(S) $G(TFAL) Q $G(TFAL)
 .S I="" F  S I=$O(^VFDLRT(FLE,"B",I)) Q:I=""  D
 ..S CT="" F  S CT=$O(^VFDLRT(FLE,"B",I,CT)) Q:CT=""  S TFAL=$G(TFAL)+1
 S CT=0,PAN="" F  S PAN=$O(^VFDLRT(FLE,"PANEL",PAN)) Q:PAN=""  D
 .I TYP=0,^VFDLRT(FLE,"PANEL",PAN)="F" S CT=CT+1 K FLG D
 ..S TST=$NA(^VFDLRT(FLE,PAN)) F  S TST=$Q(@TST) Q:TST=""!($QS(TST,2)'=PAN)  D
 ...I @TST["MESSAGE: FAIL" S:$QS(TST,3)'=$G(FLG) TFAL=$G(TFAL)+1,FLG=$QS(TST,3)
 .E  I TYP=1,^VFDLRT(FLE,"PANEL",PAN)="C" S CT=CT+1 D
 ..S TST=$NA(^VFDLRT(FLE,PAN)) F  S TST=$Q(@TST) Q:TST=""!($QS(TST,2)'=PAN)  D
 ...I $QS(TST,4)=0 S TFAL=$G(TFAL)+1
 .E  I TYP=99 S CT=CT+1 D
 ..S TST=$NA(^VFDLRT(FLE,PAN)) F  S TST=$Q(@TST) Q:TST=""!($QS(TST,2)'=PAN)  D
 ...I $QS(TST,4)=0 S TFAL=$G(TFAL)+1
 W $S(TYP=1:"C",TYP=99:"ALL",1:"F")," GROUPS: ",+$G(CT),!
 W $S(TYP=1:"C",TYP=99:"ALL",1:"F")," TESTS: ",+$G(TFAL),!
 Q
 ;
ACCTASK() ; WAS FILE 63 POPULATED TASK
 N ZTRTN,ZTDESC,ZTDTH,ZTIO,ZTUCI,ZTCPU,ZTPRI,ZTSAVE,ZTKIL,ZTSYNC,ZTSK
 S ZTRTN="ACC^VFDLROMH1A",ZTDESC="LAB MIGRATION ACCESSION CHECK"
 S ZTDTH=$$FMTH^XLFDT($$NOW^XLFDT),ZTIO="LRRESOURCE",ZTPRI=10
 D ^%ZTLOAD
 Q $G(ZTSK,-1)
 ;
ACC ; WAS FILE 63 POPULATED?
 N FLE,I,CT,ACC,LRDFN,TO K ^VFDLRT("ACC")
 S FLE=$$UP^XLFSTR($G(^VFDLRT("ASKFILE"))) Q:FLE=""
 S I=$NA(^VFDLRT(FLE)) F  S I=$Q(@I) Q:I=""  Q:$QS(I,1)'=FLE  D
 .Q:$QS(I,4)'=0  Q:$G(ACC)=$P(@I,$C(31),2)
 .Q:'$D(^VFDLRT(FLE,"B",$P(@I,$C(31)),$P(@I,$C(31),8)))
 .K ACC S ACC=$P(@I,$C(31),2)
 .S LRDFN=$$PER(@I) I +LRDFN<1 D
 ..I $G(ACC)'="",'$$ACCESS2(ACC) S TO=$G(TO)+1,^VFDLRT("ACC",ACC)=""
 .E  I $G(ACC)'="",'$$ACCESS1(ACC) S TO=$G(TO)+1,^VFDLRT("ACC",ACC)=""
 S ^VFDLRT("ACC")=TO
 ;W "TOTAL FAILED: ",$G(TO),!!
 ;S I="" F  S I=$O(FAIL(I)) Q:I=""  W I,!
 Q
 ;
ACCESS1(ACC,LRDFN) ; DOES IT EXIST WITH THE LRDFN
 N LAB,FLG S LAB=$NA(^LR(LRDFN))
 F  S LAB=$Q(@LAB) Q:LAB=""!($QS(LAB,1)'=LRDFN)  D  Q:$D(FLG)
 .Q:$QS(LAB,4)'="ORU"
 .I $P(@LAB,U)=ACC S FLG=1
 Q +$G(FLG)
 ;
ACCESS2(ACC) ; DOES IT EXIST IN 63
 N LAB,FLG S LAB=$NA(^LR)
 F  S LAB=$Q(@LAB) Q:LAB=""  D  Q:$D(FLG)
 .Q:$QS(LAB,4)'="ORU"
 .I $P(@LAB,U)=ACC S FLG=1
 Q +$G(FLG)
 ;
PER(LN) ; RETURN LRDFN
 N DFN S DFN=$$DFN^VFDDFN($$TRIM^XLFSTR($P(LN,$C(31),23)),,"MRN") I +DFN<1 Q 0
 I $$DT^VFDXLF($$GET1^DIQ(2,DFN_",",.03))'=$$DT^VFDXLF($P($$TRIM^XLFSTR($P(LN,$C(31),31))," ")) Q 0
 I $P($$GET1^DIQ(2,DFN_",",.01),",")'=$$UP^XLFSTR($$TRIM^XLFSTR($P(LN,$C(31),28))) Q 0
 I $$GET1^DIQ(2,DFN_",",.02,"I")'=$$TRANS^VFDHHLOT("OMH-CERNER","SEX",$$TRIM^XLFSTR($P($P(RT,D,29),".")),"FM") Q 0
 Q +$$LRDFN^LR7OR1(DFN)
 ;
 ;============================================================================
 ;                      HOST FILE USER SUPPORT FUNCTIONS
 ;============================================================================
 ;
MANUAL ; MANUAL CLOSE OF THE DEVICES
 N I,IEN,F S I=$NA(IO(1)) F  S I=$Q(@I) Q:I=""  Q:$QS(I,1)'=1  D
 .I $E($QS(I,2),1,5)'="|TNT|" S IO=$QS(I,2) D
 ..S IEN="" F  S IEN=$O(^TMP("XUDEVICE",$J,IEN)) Q:IEN=""  D
 ...I ^TMP("XUDEVICE",$J,IEN,"IO")=IO D 
 ....D CLOSE^%ZISH(^TMP("XUDEVICE",$J,IEN,0))
 D HOME^%ZIS
 Q
 ;
SWITCH(TAG) ; SWITCH THE DEVICE AND RETURN THE CURRENT
 N IEN,NEW
 I TAG["|TNT|" S NEW=$P(IO("HOME"),U,2) D HOME^%ZIS Q NEW
 K IEN S IEN=$O(^TMP("XUDEVICE",$J,"B",TAG,"")) Q:IEN=""
 S IO=$G(^TMP("XUDEVICE",$J,IEN,"IO")) D USE^%ZISUTL(TAG)
 Q
 ;
FILE(TAG) ; OUTPUT FILE INCREMENT CREATION AND OPEN
 N POP,FILESPEC,LIST,I,NEW,FNAME Q:$G(OUTPUT)="" 0
 K FILESPEC S FILESPEC("CERNER_TO_VISTA_"_TAG_"_"_$$DT^XLFDT_"*.TXT")=""
 I $$LIST^%ZISH(PATH,"FILESPEC","LIST") S I="" F  S I=$O(LIST(I)) Q:I=""  D
 .I $TR($P($P(I,"_",6),"."),"()")>=+$G(NEW) D
 ..S NEW=$TR($P($P(I,"_",6),"."),"()","")+1
 S:'$D(NEW) NEW=1
 S FNAME="CERNER_TO_VISTA_"_TAG_"_"_$$DT^XLFDT_"_("_NEW_").TXT"
 D OPEN^%ZISH(TAG,OUTPUT,FNAME,"W")
 I POP Q 0
 Q 1
 ;
READ(PATH,FILE,WAIT) ; READ AN HFS FILE
 N POP,RI,CT,LN,TMP S (POP,CT)=-1
READ1 ;
 D OPEN^%ZISH("READ",PATH,FILE,"R") Q:POP
 S RI=0 D USE^%ZISUTL("READ") F RI=RI:1 R LN:DTIME Q:$$STATUS^%ZISH  D  Q:POP
 .Q:RI<CT!(RI=CT)
 .S TMP(RI)=LN,CT=RI
 .D CLOSE^%ZISH("READ")
 .W TMP(RI),!! K TMP(RI) H +$G(WAIT) G READ1
 D CLOSE^%ZISH("READ")
 Q
 ;
DELETE(PATH) ; DELETE A FILE FROM THE HFS
 N FILESPEC,LIST,I,FILE
 S FILESPEC("*.TXT")="",FILESPEC("*.txt")=""
 I $$LIST^%ZISH(PATH,"FILESPEC","LIST") D
 .S I="" F  S I=$O(LIST(I)) Q:I=""  W I,!
 Q:'$D(LIST)
 S FILE=$$ASKFILE Q:FILE=""
 I $$DEL^%ZISH(PATH,FILE) W !,"SUCCESS"
 E  W !,"FAILURE"
 Q
 ;
EXTRACT ;
 N FILE,I,SP,CT
 S FILE=$$UP^XLFSTR($G(^VFDLRT("ASKFILE"))) Q:FILE=""
 K ^VFDLRT(FILE,"SPEC"),^VFDLRT(FILE,"COLL")
 S I=0 F  S I=$O(^VFDLRT(FILE,I)) Q:'I  D
 .K SP S SP=$P(^VFDLRT(FILE,I),U,6) I SP'="" D
 ..S:'$D(^VFDLRT(FILE,"SPEC",SP)) ^VFDLRT(FILE,"SPEC",SP)=$P(^VFDLRT(FILE,I),U,7)
 .K CT S CT=$P(^VFDLRT(FILE,I),U,43) I CT'="" D
 ..S:'$D(^VFDLRT(FILE,"COLL",CT)) ^VFDLRT(FILE,"COLL",CT)=$P(^VFDLRT(FILE,I),U,44)
 Q
 ;
 ;============================================================================
 ;                                  REPAIR
 ;============================================================================
 ;
REPAIR(GO) ; Repair GLOBALGROUP based on NM SUCCESS
 N GL,FNAM,NM,MSGID,CT
 S GL="" F  S GL=$O(^VFDLRT(GL)) Q:GL=""  D
 . Q:GL="ASKPATH"!(GL="ASKFILE")!(GL="GLOBALGROUP")
 . S FNAM(GL)=""
 S MSGID=0 F  S MSGID=$O(^VFDLRT("GLOBALGROUP",MSGID)) Q:'MSGID  D
 . Q:$E(^VFDLRT("GLOBALGROUP",MSGID),1)'="C"
 . I $$REPAIR1(MSGID,.FNAM) D
 . . S CT=$G(CT)+1
 . . S:$D(GO) ^VFDLRT("GLOBALGROUP",MSGID)="F"
 W !!,"TOTAL GROUPS RESET: ",+$G(CT),!
 Q
 ;
REPAIR1(MSGID,FNAM) ;
 N NM,FLG
 S NM="" F  S NM=$O(FNAM(NM)) Q:NM=""  D
 . Q:'$D(^VFDLRT(NM,"PANEL",MSGID))
 . I '$D(^VFDLRT(NM,"SUCCESS",MSGID)) S FLG=1
 . I $D(^VFDLRT(NM,"SUCCESS",MSGID)) K FLG
 Q +$G(FLG)
