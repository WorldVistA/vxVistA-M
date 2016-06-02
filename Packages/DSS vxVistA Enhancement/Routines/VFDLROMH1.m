VFDLROMH1 ;DSS/TFF - Cerner to vxVistA Flat File Parser;10/03/2013
 ;;2013.1;VENDOR - DOCUMENT STORAGE SYS;**27**;05 May 2014;Build 3
 ;Copyright 1995-2014,Document Storage Systems Inc. All Rights Reserved
 ;
 ;
 ;
 Q
 ;
START(GO,SLNT,DELM) ; USER BLOCK - THIS IS BEFORE TASKING OFF AND GLOBAL CREATION
 N PATH,CERNER,NM,OUTPUT,DIR,DIRUT,X,Y,D S D=$C(31),GO=+$G(GO)
 Q:'$D(DUZ)
 I GO=0 W !!,"ONE was NOT passed so I will NOT submit these results.",!!
 ;
 W !,"Set the historical data in file location."
 S PATH=$$ASKPATH^VFDLROMH1A Q:PATH=""
 S CERNER=$$ASKFILE^VFDLROMH1A Q:CERNER=""  S NM=$$UP^XLFSTR(CERNER)
 ;W !!,"Set the output log location."
 ;S OUTPUT=$$ASKPATH^VFDLROMH1A(,"O") Q:OUTPUT=""
 W !! D MANUAL^VFDLROMH1A
 ;
 I '$D(^VFDLRT(NM,0)) D
 . S DIR("A")="Global doesn't exist. Do you want to create it",DIR("B")="YES"
 E  S DIR("A")="Would you like to rebuild the global",DIR("B")="NO"
 S DIR(0)="Y" D ^DIR
 I Y>0,'$$FTG(PATH,CERNER,+$G(SLNT)) W "Failed to create global.",!
 I '$D(^VFDLRT(NM,0)) D  D MANUAL^VFDLROMH1A Q
 . W !,"Global doesn't exist. Exiting the utility."
 ;
 D TASK
 Q
 ;
SCHEDUL ; SCHEDULED TASK ENTRY  -  NO PROMPTS
 ;
 ; REQUIREMENTS:
 ; =============
 ; ^VFDLRT("ASKPATH") AS THE LOCATION TO GET FILES
 ; FILE NAME TO BE DIFFERENT FROM ONE ALREAD PARSED
 ;   ________________ DATE .TXT
 ;
 N PATH,FILESPEC,LIST,CERNER,NM,RECS,PANELS,WHOLE,SET,I,ID,GO S GO=1
 S PATH=$G(^VFDLRT("ASKPATH")) Q:PATH=""
 S FILESPEC("Cerner_MigrationOut*")=""
 I $$LIST^%ZISH(PATH,"FILESPEC","LIST") D
 . S CERNER="" F  S CERNER=$O(LIST(CERNER)) Q:CERNER=""  D
 . . Q:$$UP^XLFSTR($P(CERNER,".",2))'="TXT"
 . . S NM=$$UP^XLFSTR(CERNER)
 . . I '$D(^VFDLRT(NM,0)),'$$FTG(PATH,CERNER,1) Q
 . . S RECS=$P(^VFDLRT(NM,0),U,2),PANELS=$P(^VFDLRT(NM,0),U,3)
 . . Q:$P(^VFDLRT(NM,0),U,4)=$P(^VFDLRT(NM,0),U,3)
 . . S $P(^VFDLRT(NM,0),U,5)=0 ;RESET FAILED
 . . ;LAST IEN ^ TOTAL ^ TOTAL PANELS ^ COMPLETED ^ FAILED
 . . S WHOLE=$P(^VFDLRT(NM,0),U,3)
 . . S SET=$P(WHOLE/8,".")+1
 . . F I=SET:SET:WHOLE D  I I+SET>WHOLE S ID=$$SET(I+1,WHOLE),^VFDLRT(NM,"TASK",ID)=""
 . . . S ID=$$SET(I-SET+1,I),^VFDLRT(NM,"TASK",ID)=""
 Q
 ;
FTG(PATH,CERNER,SLNT) ; FILE TO GLOBAL WITH NO OVERFLOW LINES
 ;
 ; GLOBAL HEADER:
 ; --------------
 ; LAST IEN ^ TOTAL ^ TOTAL PANELS ^ COMPLETED ^ FAILED
 ;
 Q:$G(PATH)=""!($G(CERNER)="")!($G(NM)="") 0
 N POP,D,START,STOP,CT,RI,LN,MSGID,TSTID,STR,P,DUPT,DUPC
 K ^VFDLRT(NM)
 S POP=0,^VFDLRT(NM,0)="^0",CT=1,D=$C(31)
 S:$G(DELM)'="" D=DELM
 D OPEN^%ZISH("PARSE",PATH,CERNER,"R") I POP Q 0
 S START=$$NOW^XLFDT,RI=0 D USE^%ZISUTL("PARSE")
 F RI=RI:1 R LN:DTIME Q:$$STATUS^%ZISH  D
 . D:'+$G(SLNT)&('(RI#1000)) DOTS^VFDLROMH1A
 . I $$TRM($P(LN,D))?6.N K MSGID,TSTID D  S CT=CT+1
 . . S MSGID=$$TRM($P(LN,D)),TSTID=$$TRM($P(LN,D,8))
 . . ;
 . . ; DUPS
 . . I $D(^VFDLRT(NM,MSGID,TSTID,0)) S DUP(MSGID,TSTID)=+$G(DUP(MSGID,TSTID))+1 D
 . . I $D(^VFDLRT(NM,MSGID,TSTID,0)) D
 . . . S ^VFDLRT(NM,"DUP",MSGID,TSTID)=$G(^VFDLRT(NM,"DUP",MSGID,TSTID))+1
 . . . I '+$G(SLNT) D DOTS^VFDLROMH1A(" "_MSGID_" - "_TSTID_" IS A DUPLICATE")
 . . ;
 . . S ^VFDLRT(NM,MSGID,TSTID,0)=$TR($$TRM(LN),$C(30),"")
 . . ; X-REFS
 . . S ^VFDLRT(NM,"B",MSGID,TSTID)=""
 . . S ^VFDLRT(NM,"PANEL",MSGID)="",^VFDLRT(NM,"TEST",TSTID,MSGID)=""
 . . ; HEADER
 . . S $P(^VFDLRT(NM,0),U)=MSGID_","_TSTID
 . ;
 . I $$TRM($P(LN,D))'?6.N K STR D
 . . I $L(^VFDLRT(NM,MSGID,TSTID,0),U)+$L(LN,U)=43 D
 . . . S STR=" "_$TR(LN,$C(30),"")_$C(28)
 . . I '$D(STR) S STR=" "_$TR(LN,$C(30),"")
 . . S $E(^VFDLRT(NM,MSGID,TSTID,0),$L(^VFDLRT(NM,MSGID,TSTID,0))+1)=STR
 S P="" F CT=1:1 S P=$O(^VFDLRT(NM,"PANEL",P)) Q:P=""  D
 . S ^VFDLRT(NM,"C",CT,P)="",$P(^VFDLRT(NM,0),U,3)=CT
 S $P(^VFDLRT(NM,0),U,2)=$$TESTCT^VFDLROMH1A(-1,1) ; BUILD THE HEADER BY COUNT
 ; DUP HEADER
 S CT=$NA(^VFDLRT(NM,"DUP")) F  S CT=$Q(@CT) Q:CT=""  Q:$QS(CT,2)'="DUP"  D
 . S DUPT=$G(DUPT)+1,DUPC=$G(DUPC)+@CT
 ; SET THE 6TH PIECE OF THE HEADER TO TOTAL # OF DUPLICATE TESTS
 S $P(^VFDLRT(NM,0),U,6)=$G(DUPT)
 ; SET THE 7TH PIECE OF THE HEADER TO TOTAL # OF DUPLICATIONS
 S $P(^VFDLRT(NM,0),U,7)=$G(DUPC)
 S STOP=$$NOW^XLFDT D CLOSE^%ZISH("PARSE") D:'+$G(SLNT) TIME1
 Q 1
 ;
TIME(START,STOP,TAG) ; TIME PRINT WITH DEVICE SWITCH
 Q:$G(NM)=""
 D SWITCH^VFDLROMH1A(TAG)
TIME1 ; TIME PRINT TO TERMINAL
 N DCT,DIR,DA,X,Y
 W !!,"              TIME STARTED: "_$$FMTE^XLFDT(START,1)
 W !,"            TIME COMPLETED: "_$$FMTE^XLFDT(STOP,1)
 W !,"TOTAL # OF DUPLICATE TESTS: "_$P(^VFDLRT(NM,0),U,6)
 W !,"   TOTAL # OF DUPLICATIONS: "_$P(^VFDLRT(NM,0),U,7)
 W !,"               TIME LAPSED: "_$$FMDIFF^XLFDT(STOP,START,2)_" SECONDS"
 W !,"             TOTAL RECORDS: "_$P(^VFDLRT(NM,0),U,2),!
 I $D(^VFDLRT(NM,"DUP")) W !,"THERE ARE DUPLICATE TESTS...",! D
 . S DIR(0)="Y",DIR("A")="Would you like list the duplicates"
 . S DIR("B")="YES" D ^DIR
 . Q:$D(DIRUT)!(Y<1)
 . S DCT=$NA(^VFDLRT(NM,"DUP")) F  S DCT=$Q(@DCT) Q:DCT=""  Q:$QS(DCT,2)'="DUP"  D
 . . W "MESSAGE ID: ",$QS(DCT,3)," TEST ID: ",$QS(DCT,4),?55," PARSED "
 . . W @DCT," TIMES",!
 . W !,"****ONLY THE LAST OF EACH TEST WILL BE PROCESSED.****",!
 Q
 ;
HEADER ;
 I '$D(NM) N NM S NM=$$UP^XLFSTR(^VFDLRT("ASKFILE"))
 W #,$$REPEAT^XLFSTR("=",79)
 W !,$$CJ^XLFSTR("FILE: "_$G(CERNER,^VFDLRT("ASKFILE")),79),!
 W !,"  TOTAL RECORDS: "_$G(RECS,$P(^VFDLRT(NM,0),U,2))
 W !,"   TOTAL GROUPS: "_$G(PANELS,$P(^VFDLRT(NM,0),U,3))
 W !,"      COMPLETED: "_+$P(^VFDLRT(NM,0),U,4)
 W !,"         FAILED: "_+$P(^VFDLRT(NM,0),U,5)_" (ONLY APPLIES TO LAST PARSE)"
 W !,"DUPLICATE TESTS: "_$P(^VFDLRT(NM,0),U,6),!
 W !,$$REPEAT^XLFSTR("=",79),!
 Q
 ;
TASK ; TASK THE PARSE
 N DIR,DIRUT,X,Y,RECS,PANELS
 S RECS=$P(^VFDLRT(NM,0),U,2),PANELS=$P(^VFDLRT(NM,0),U,3)
 I '$D(CERNER) N CERNER,NM S CERNER=$G(^VFDLRT("ASKFILE")) D  Q:NM=""
 . S NM=$$UP^XLFSTR($G(CERNER))
 S DIR(0)="Y",DIR("A")="Would you like to view the task panel"
 S DIR("B")="YES" D ^DIR
 Q:$D(DIRUT)!(Y<1)
 I $$REFRESH("R") F  Q:'$$REFRESH
 Q
 ;
REFRESH(Y) ;
 N DIR,X,DIRUT,I,ZTSK,IEN,PAN,MAX,SET,WHOLE,ID,CT,TASK,T,CON,IENS,BR
 I '$D(Y) N Y D  Q:$G(Y)["^" 0
 . S DIR(0)="S^R:REFRESH;S:STOP A TASK;X:STOP ALL TASKS;P:PARSE;V:VIEW;I:INQUIRE"
 . S DIR("A")="OPTION",DIR("B")="REFRESH",DIR("T")=10
 . D ^DIR I $G(Y)="" S Y="R"
 ;
 ; REFRESH
 I Y="R" D
 . D HEADER S I="" F  S I=$O(^VFDLRT(NM,"TASK",I)) Q:I=""  D
 . . K ZTSK S ZTSK=I D STAT^%ZTLOAD W !,I,": ",$G(ZTSK(2))
 ;
 ; STOP A TASK
 I Y="X" D
 . S I="" F  S I=$O(^VFDLRT(NM,"TASK",I)) Q:I=""  D
 . . I $$ASKSTOP^%ZTLOAD(I) W !,"Task "_I_" stopped.",!
 . . E  W !,"Failed to stop the task.",!
 I Y="S" D
 . K DIR,X,Y S DIR(0)="NA^1::0",DIR("A")="TASK ID: "
 . W ! D ^DIR Q:$D(DIRUT)
 . I $$ASKSTOP^%ZTLOAD(Y) W !,"Task stopped.",!!
 . E  W !,"Failed to stop the task.",!!
 ;
 ; SETUP THE TASKS FOR PARSING
 I Y="P" D
 . I '$$TM^%ZTLOAD W !!,"**TASKMAN must be running.**",!! Q
 . I $P(^VFDLRT(NM,0),U,4)=$P(^VFDLRT(NM,0),U,3) W !!,"Nothing to parse.",!! Q
 . ;
 . S $P(^VFDLRT(NM,0),U,5)=0 ;RESET FAILED
 . ; LAST IEN ^ TOTAL ^ TOTAL PANELS ^ COMPLETED ^ FAILED
 . S WHOLE=$P(^VFDLRT(NM,0),U,3)
 . K DIR,X,Y I WHOLE>1 D  Q:$D(DIRUT)
 . . S DIR("A")="Indicate an increment for your threads"
 . . S MAX=1000000 S:WHOLE<MAX MAX=WHOLE
 . . S DIR(0)="N^1:"_MAX_":0"
 . . S:WHOLE>1000 DIR("B")=$P(WHOLE/8,".")+1
 . . W ! D ^DIR
 . S SET=$G(Y,300) W !
 . F I=SET:SET:WHOLE D  I I+SET>WHOLE S ID=$$SET(I+1,WHOLE) D W
 . . S ID=$$SET(I-SET+1,I) D W
 ;
 ; VIEW A LOG
 I Y="V" D
 . K DIR,X,Y S DIR(0)="NA^1::0",DIR("A")="TASK ID: "
 . W ! S CON="" D ^DIR Q:$D(DIRUT)
 . Q:$P(^VFDLRT(NM,"TASK",Y),U)=""!($P(^VFDLRT(NM,"TASK",Y),U,2)="")
 . F I=$P(^VFDLRT(NM,"TASK",Y),U):1:$P(^VFDLRT(NM,"TASK",Y),U,2) D  Q:CON[U
 . . K PAN S PAN=$O(^VFDLRT(NM,"C",I,"")) Q:PAN=""
 . . S T="" F  S T=$O(^VFDLRT(NM,"B",PAN,T)) Q:T=""  D  Q:$G(CON)[U
 . . . W ! S IENS="" F  S IENS=$O(^VFDLRT(NM,PAN,T,1,IENS)) Q:IENS=""  D  Q:CON[U
 . . . . I $G(BR)#20=0 R !,"Press any key to continue: ",CON:DTIME Q:CON[U
 . . . . W ^VFDLRT(NM,PAN,T,1,IENS),! S BR=$G(BR)+1
 ;
 ; INQUIRE ABOUT A GROUP-TEST
 I Y="I" D
 . K DIR,X,Y S DIR(0)="NA^1::0",DIR("A")="GROUP ID: "
 . W ! D ^DIR Q:$D(DIRUT)
 . S PAN=Y K DIR,X,Y
 . S DIR(0)="S^",T="" F I=1:1 S T=$O(^VFDLRT(NM,"B",PAN,T)) Q:T=""  D
 . . S DIR(0)=DIR(0)_I_":"_T_";"
 . Q:+$P(DIR(0),U,2)<1 
 . S DIR("A")="SELECT TEST" D ^DIR Q:$D(DIRUT)
 . Q:'$D(^VFDLRT(NM,PAN,Y(0),0))
 . S T=Y(0) W #,$TR(^VFDLRT(NM,PAN,T,0),D,U),!!
 . K DIR,X,Y S DIR(0)="NA^1::0",DIR("A")="SELECT PIECE TO EXTRACT: "
 . S DIR("?")="This is the piece position of the above test you"
 . S DIR("?")=DIR("?")_" would like extracted and identified."
 . S DIR("??")="^D HELP^VFDLROMH1A"
 . F  D ^DIR Q:$D(DIRUT)  D
 . . I $P($T(PIECE+Y^VFDLROMH1A),";;",2)'="" D
 . . . W !,?5,$P($T(PIECE+Y^VFDLROMH1A),";;",2),": " D
 . . . . W $P(^VFDLRT(NM,PAN,T,0),D,Y),!
 Q 1
 ;
W ; SETUP PARSE RANGE
 S ^VFDLRT(NM,"TASK",ID)="" W !,"NEW TASK: ",ID
 Q
 ;
SET(STRT,TOTAL) ; Create a task
 N ZTRTN,ZTDESC,ZTDTH,ZTIO,ZTUCI,ZTCPU,ZTPRI,ZTSAVE,ZTKIL,ZTSYNC,ZTSK
 S ZTRTN="PARSE^VFDLROMH1",ZTDESC="CERNER HISTORICAL MIGRATION"
 S ZTDTH=$$FMTH^XLFDT($$NOW^XLFDT),ZTIO="LRRESOURCE",ZTPRI=10
 S ZTSAVE("STRT")=STRT,ZTSAVE("TOTAL")=TOTAL,ZTSAVE("NM")=NM
 S:$G(DELM)'="" ZTSAVE("DELM")=DELM
 S ZTSAVE("GO")=GO D ^%ZTLOAD
 Q $G(ZTSK)
 ;
PARSE ; PARSE THE FILE AND UPDATE THE LOG FILE
 ;      ON ERROR OR TO UPDATE SUCCESS UPDATE LOG
 ;
 ; VARIABLES SET FROM ZTSAVE : STRT - START COUNT
 ;                            TOTAL - TOTAL NUMBER OF PANELS TO PARSE
 ;                               NM - UPPERCASE FILENAME
 ;                               GO - IF GO THEN SUBMIT ON SUCCESS
 ;
 N D,MI,FAIL,MSGID,TSTID,IENS,LN,SAVE,RECS,TIEN,FAC,DFN,DUZP,VXFLG,VXLO,VXHI
 N VXCL,VXCH,VXUNIT,FS,CP,ICT,COM,ORTEST,XT,PASS
 N P,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18,P19
 S D=$C(31),P="|"
 S:$G(DELM)'="" D=DELM
 ;
 S ^VFDLRT(NM,"TASK",ZTSK)=STRT_U
 ;
 F MI=STRT:1:TOTAL D  S $P(^VFDLRT(NM,"TASK",ZTSK),U,2)=MI I $$S^%ZTLOAD S ZTSTOP=1 Q
 . Q:'$D(^VFDLRT(NM,"C",MI))
 . K MSGID S MSGID=$O(^VFDLRT(NM,"C",MI,"")) Q:MSGID=""
 . ;
 . ; *** CONVERSION **************************************************************
 . I $E($G(^VFDLRT(NM,"PANEL",MSGID)),1)="C" S ^VFDLRT(NM,"PANEL",MSGID)="C"
 . I $E($G(^VFDLRT(NM,"PANEL",MSGID)),1)="F" S ^VFDLRT(NM,"PANEL",MSGID)="F"
 . I $E($G(^VFDLRT("GLOBALGROUP",MSGID)),1)="C" S ^VFDLRT("GLOBALGROUP",MSGID)="C"
 . I $E($G(^VFDLRT("GLOBALGROUP",MSGID)),1)="F" S ^VFDLRT("GLOBALGROUP",MSGID)="F"
 . ; *****************************************************************************
 . ;
 . Q:^VFDLRT(NM,"PANEL",MSGID)="C"!(^VFDLRT(NM,"PANEL",MSGID)="T")
 . ; IF THE GLOBAL WAS DELETED WE STILL DON'T WANT TO RESUBMIT
 . I GO=1,$G(^VFDLRT("GLOBALGROUP",MSGID))="C" S ^VFDLRT(NM,"PANEL",MSGID)="C" Q
 . ;
 . S ^VFDLRT(NM,"PANEL",MSGID)="T"
 . ;
 . K FAIL,SAVE,ORTEST,COM,STR
 . K P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18,P19
 . S TSTID="" F  S TSTID=$O(^VFDLRT(NM,"B",MSGID,TSTID)) Q:TSTID=""  D
 . . K ^VFDLRT(NM,MSGID,TSTID,1) ; KILL ANY OLD MESSAGE - ONLY FOR FAILURE
 . . S LN=$G(^VFDLRT(NM,MSGID,TSTID,0)) Q:LN=""
 . . D M("$"),M("#",MSGID),M("#","----------PROCESS START----------")
 . . ;
 . . ; IDENTIFY EACH TEST FOR THE MESSAGE ID
 . . ; MULTIPLE TEST POSSIBILITIES AS TNAME(9) RESULT(11) UNITS_CD(12) TUNITS(13)
 . . ; FILE 60 = .01:TEST NAME - 13:TEST DD
 . . S TIEN=$$TRANS^VFDHHLOT("OMH-CERNER","RESULT",$$TRM($P(LN,D,8)),"FM")
 . . I +TIEN<1 S FAIL=1 D 
 . . . D M("#","FAIL: UNABLE TO LOCATE TEST - "_$$TRM($P(LN,D,8)))
 . . I +TIEN>0 D
 . . . D M("#","TEST - "_$$GET1^DIQ(60,TIEN_",",.01)_" ------------START")
 . . . S ORTEST(TIEN)=""
 . . ;
 . . ; PEOPLE CHECK AND GET TEST VARIABLES
 . . D PERCK(LN),GETVAR(LN)
 . . ;
 . . ; BUILD THE TEST
 . . ; P#60|DATANAME^VAL^FLG^^LO^HI^CL^CH^UNIT^
 . . S SAVE(TIEN)=TIEN_"|"_$$GET1^DIQ(60,TIEN_",",13)_U_$P(LN,D,11)_U_VXFLG_U_U
 . . S SAVE(TIEN)=SAVE(TIEN)_VXLO_U_VXHI_U_VXCL_U_VXCH_U_VXUNIT_U
 . . ;
 . . ; TEST/RESULT COMMENTS
 . . S:$P(LN,D,38)'=""!($P(LN,D,38)'?." ") SAVE(TIEN,1)=$P(LN,D,38)
 . . ; ADD FASTING TO TEST COMMENTS
 . . K FS S FS=$$TRM($P(LN,D,34))
 . . S:FS'=""!(FS'?." ") SAVE(TIEN,$O(SAVE(TIEN,""),-1)+1)="FASTING: "_FS
 . . ; THE INFAMOUS "I-NOTES"
 . . F CP=1:1:$L($P(LN,D,42),$C(31)) Q:$P(LN,D,42)=""  D
 . . . K ICT S ICT=$O(SAVE(TIEN,""),-1)+1
 . . . S SAVE(TIEN,ICT)=$P($P(LN,D,42),$C(31),CP)
 . . . I SAVE(TIEN,ICT)="" K SAVE(TIEN,ICT)
 . . ; SPECIMEN COMMENTS
 . . S:$P(LN,D,39)'="" COM(1,$O(COM(1,""),-1)+1)=$P(LN,D,39)
 . . ;
 . . ; BUILD STRING
 . . D BLDSTR(LN)
 . . ;
 . . ; FINISHED PROCESSING A TEST
 . . D:+TIEN>0 M("#","TEST - "_$$GET1^DIQ(60,TIEN_",",.01)_" --------------END")
 . . D M("#","-----------PROCESS END-----------")
 . . ;
 . . ; IF THE TEST FAILED
 . . I $D(FAIL) D  Q
 . . . S ^VFDLRT(NM,"PANEL",MSGID)="F"
 . . . I $O(^VFDLRT(NM,"B",MSGID,""),-1)=TSTID D
 . . . . S $P(^VFDLRT(NM,0),U,5)=$P(^VFDLRT(NM,0),U,5)+1
 . . ;
 . . ; IF THE PANEL PASSED
 . . I $O(^VFDLRT(NM,"B",MSGID,""),-1)=TSTID&(^VFDLRT(NM,"PANEL",MSGID)'="F") D
 . . . S ^VFDLRT(NM,"PANEL",MSGID)="C"
 . . . S $P(^VFDLRT(NM,0),U,4)=$P(^VFDLRT(NM,0),U,4)+1
 . ;
 . ; *** FINISH UP THE GROUPING - PASS OR FAIL
 . ;     SUBMIT
 . I '$D(FAIL) K PASS D
 . . I $D(SAVE),GO=1 D THEO^VFDLRPCV(.PASS,.SAVE,.COM,STR,.ORTEST) D
 . . . I +$G(PASS(0))=1 D  Q
 . . . . S ^VFDLRT(NM,"SUCCESS",MSGID)="" K ^VFDLRT(NM,"FAILED",MSGID)
 . . . . S ^VFDLRT("GLOBALGROUP",MSGID)="C"
 . . . S (^VFDLRT(NM,"PANEL",MSGID),^VFDLRT("GLOBALGROUP",MSGID))="F"
 . . . S ^VFDLRT(NM,"FAILED",MSGID)=$P($G(PASS(0)),U,2)
 . . . D EXCEPT^VFDLROMH1A("SUBMIT ERROR - "_$P($G(PASS(0)),U,2))
 . . . S $P(^VFDLRT(NM,0),U,4)=$P(^VFDLRT(NM,0),U,4)-1
 D:'$D(ZTSTOP) REPORT^VFDLROMH1A(D)
 Q
 ;
PERCK(RT) ; Validate People
 ;
 ; FACILITY
 S FAC=$$TRANS^VFDHHLOT("OMH-CERNER","FAC",$$TRM($P($P(RT,D,24),".")),"FM")
 I +FAC<1 S FAIL=1 D
 . D BLDERR(24,"FAC_CD","Facility",$$TRM($P($P(RT,D,24),".")))
 ;
 ; DFN
 S DFN=$$DFN^VFDDFN($$TRM($P(RT,D,23)),,"MRN")
 I +DFN<1 S FAIL=1 D BLDERR(23,"STATE_ID","MRN",$$TRM($P(RT,D,23)))
 I +DFN>0 D
 . ; ADD IN CODE TO ADD IN 0'S 9/4/1961 TO 09/04/1961
 . N STRDOB,STRLAST,STRSEX
 . S STRDOB=$$DT($P($$TRM($P(RT,D,31))," "))
 . I $$GET1^DIQ(2,DFN_",",.03,"I")'=STRDOB D
 . . S FAIL=1 D BLDERR(31,"BIRTH_DT_TM","Patient's DOB",STRDOB)
 . S STRLAST=$$UP^XLFSTR($$TRM($P(RT,D,28)))
 . I $P($$GET1^DIQ(2,DFN_",",.01),",")'=STRLAST D
 . . S FAIL=1 D BLDERR(28,"NAME_LAST","Patient's Last Name",STRLAST)
 . S STRSEX=$$TRANS^VFDHHLOT("OMH-CERNER","SEX",$$TRM($P($P(RT,D,29),".")),"FM")
 . I $$GET1^DIQ(2,DFN_",",.02,"I")'=STRSEX D
 . . S FAIL=1 D BLDERR(29,"SEX_CD","Patient's sex",STRSEX)
 ;
 ; S DUZP=$$GETUSER^VFDLROMH1A($$TRM($P(RT,D,3)))
 S DUZP=$$FIND1^VFDDUZ($$TRM($P(RT,D,3)),"CRNP")
 I +DUZP<1 S FAIL=1 D BLDERR(3,"LAST_UPDATE_PROVIDER_ID","NEW PERSON CRNP",$$TRM($P(RT,D,3)))
 ;
 Q
 ;
GETVAR(RT) ; Get Test Variables
 ;
 ; FLAG(NORMALCY_CD(AF))
 ; NORMAL IN THE EMR IS NULL BUT THE TRANSLATION NEEDS A VALUE SO
 ; IF IT MATCHES A SPACE NULL IT
 ;
 S VXFLG=$$TRANS^VFDHHLOT("OMH-CERNER","RFLAG",$$TRM($P($P(RT,D,32),".")),"FM")
 S:VXFLG?." " VXFLG=""
 I +VXFLG=-1 S FAIL=1 D
 . D M("#","FAIL: REFERENCE FLAG - "_$$TRM($P($P(RT,D,32),".")))
 ; LO/HI
 S VXLO=$$TRM($P(RT,D,20)),VXHI=$$TRM($P(RT,D,19))
 ; CL/CH
 S VXCL=$$TRM($P(RT,D,21)),VXCH=$$TRM($P(RT,D,22))
 ; UNIT
 S VXUNIT=$TR($$TRM($P(RT,D,13)),U,"*")
 Q
 ;
BLDSTR(RT) ; Build the informational string to submit
 ;
 ; *** FIRST PIECE IS EMPTY
 ;
 ; Patient|Date/Time specimen taken|Date/Time Report Completed|
 ; Verify Person|Colletion Sample|Specimen|Accession Number|
 ; Ordering Physician|Ordering Location|Ward Location|Institution|
 ; Ordering UID|Specimen Comment|UID|Requesting Location|
 ; Requesting Div/Div|Accession Institution|Ordered Test
 ;
 ; 2  Patient
 S:+$G(P2)<1 P2=DFN_P I +P2<1 S FAIL=1
 ;
 ; 3  Date/Time specimen taken
 S:'$G(P3) P3=$$DT($$TRM($P(RT,D,15)))_P
 I 'P3 S FAIL=1 D BLDERR(15,"CURRENT_START_DT_TIME","Date/Time specimen taken",$P(P3,P))
 I '$P($P(P3,P),".",2) S P3=$P($P(P3,P),".")_".000001"_P
 ;
 ; 4  Date/Time Report Completed
 S:'$G(P4) P4=$$DT($$TRM($P(RT,D,14)))_P
 I 'P4 S FAIL=1 D BLDERR(14,"VERIFIED_DT_TM","Date/Time Report Completed",$P(P4,P))
 I '$P($P(P4,P),".",2) S P4=$P($P(P4,P),".")_".000001"_P
 ;
 ; 5  Verify Person
 S:+$G(P5)<1 P5=$$FIND1^DIC(200,,"X","LRLAB,AUTOACCEPT")_P
 I +P5<1 S FAIL=1 D M("#","FAIL: Unable to find the ""LRLAB,AUTOACCEPT"" service user in file 200")
 ;
 ; 6  Collection Sample (AS)
 S:+$G(P6)<1 P6=$$TRANS^VFDHHLOT("OMH-CERNER","SAMPLE",$$TRM($P(RT,D,43)),"FM")_P
 I +P6<1 S FAIL=1 D BLDERR(43,"COLLECTIONSAMPLE_CODE","Collection Sample (AS)",$P(P6,P))
 ;
 ; 7  Specimen
 S:+$G(P7)<1 P7=$$TRANS^VFDHHLOT("OMH-CERNER","SPECIMEN",$$TRM($P(RT,D,6)),"FM")_P
 I +P7<1 S FAIL=1 D BLDERR(6,"SPECIMEN_CODE","Specimen",$P(P7,P))
 ;
 ; 8  Accession Number | 13 Ordering UID | 15 UID
 S:'$L($G(P8)) (P8,P13,P15)=$$TRM($P(RT,D,2))_P
 I $L(P8)<1!($L(P8)>20) S FAIL=1 D BLDERR(2,"ACCESSION_NBR","Accession Number",$P(P8,P))
 ;
 ; 9  Ordering Physician
 S:+$G(P9)<1 P9=DUZP_P I +P9<1 S FAIL=1
 ;
 ; 10 Ordering Location | 11 Ward Location | 16 Requesting Location
 S:+$G(P10)<1 (P10,P11,P16)=$$FIND1^DIC(44,,"O","HISTORICAL LOCATION")_P
 I +P10<1 S FAIL=1 D
 . D M("#","FAIL: Ordering, Ward, and Requesting Location IS ""HISTORICAL LOCATION"" which was not found in file 44")
 ;
 ; 12 Institution
 S:+$G(P12)<1 P12=FAC_P I +P12<1 S FAIL=1
 ;
 ; 14 Specimen Comment
 S:+$G(P14)<1 P14=""_P
 ;
 ; 17 Requesting Div/Div
 S:+$G(P17)<1 P17=FAC_P I +P17<1 S FAIL=1
 ;
 ; 18 Accession Institution
 S:+$G(P18)<1 P18=FAC_P I +P18<1 S FAIL=1
 ;
 ; 19 Ordered Test
 S:+$G(P19)<1 P19=$$TRANS^VFDHHLOT("OMH-CERNER","ORDER",$$TRM($P(RT,D,10)),"FM")
 I +P19<1 S FAIL=1 D BLDERR(10,"CATALOG_CD","Ordered Test",$P(P19,P))
 ;
 S STR=P_P2_P3_P4_P5_P6_P7_P8_P9_P10_P11_P12_P13_P14_P15_P16_P17_P18_P19
 Q
 ;
 ;============================================================================
 ;                               MESSAGE BUILDER
 ;============================================================================
 ;
BLDERR(PC,HEAD,DESC,VAL) ; Build error message and send
 N ERR
 S ERR="FAIL: EXTRACT PIECE "_$G(PC)_" ("_$G(HEAD)_") WHICH IS "
 S ERR=ERR_"("_$G(DESC)_") WHICH EQUALS ("_$G(VAL)_")"
 D M("#",ERR)
 Q
 ;
M(ACT,ADD) ; Update the record with a log
 ;           ADD = ADDITIONAL INFORMATION
 N LNUM S LNUM=$O(^VFDLRT(NM,MSGID,TSTID,1,""),-1)+1
 I $G(ACT)="$" D  Q
 . S ^VFDLRT(NM,MSGID,TSTID,1,LNUM)=$$FMTE^XLFDT($$NOW^XLFDT,1)
 I $G(ACT)="#" D
 . S ^VFDLRT(NM,MSGID,TSTID,1,LNUM)="MESSAGE: "_ADD
 . I ADD["FAIL" D EXCEPT^VFDLROMH1A(ADD)
 Q
 ;
 ;============================================================================
 ;                                  SHORTCUTS
 ;============================================================================
 ;
TRM(VAL) ; $$TRIM^XLFSTR
 Q $$TRIM^XLFSTR(VAL)
 ;
DT(VAL) ; DATE FORMATTING
 Q $$DT^VFDXLF(VAL)
