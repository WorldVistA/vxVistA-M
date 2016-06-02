VFDPTCHR1 ;DSS/SMP - RPCs for Patient Characteristics ; 01/08/2015 15:40
 ;;2013.1;DSS,INC VXVISTA OPEN SOURCE;**22**;28 Jan 2013;Build 7
 ;Copyright 1995-2015,Document Storage Systems Inc. All Rights Reserved
 ;
 ; This routine can only be invoked by VFDPTCHR
 ;
 Q
 ;
GET ; Used by RPC - [VFD PT CHAR GET]
 ;
 ; INPUTS:
 ;   DFN  - opt - Patient IEN
 ;   SORT - opt - Boolean, default to 0 - if SORT then return result
 ;                sorted by IEN, if 'SORT, return result by PRIORITY
 ;
 ; OUTPUT: 
 ;   RET(I) = IEN ^ Name ^ Abbreviation
 ;
 ; DFN and SORT newed in VFDPTCHR
 ;
 S RET(1)="0^No Records Found"
 S SORT=$G(SORT)
 I $G(DFN) D GET1(DFN,SORT) I 1
 E  D GETALL(SORT)
 Q
 ;
SAVE ; Used by RPC - [VFD PT CHAR SAVE]
 ; INPUTS:
 ;   DFN   - req - Patient IEN
 ;   DUZ   - req - New Person IEN
 ;   VISIT - req - Visit Date/Time
 ;   CHAR  - opt - List of Patient Characteristics.  CHAR(I) = IEN
 ;
 ; OUTPUT:
 ;   1 if add/edit is successful
 ;  -1^error message
 ;
 N X,ADD,INACT,LIST
 D GET^VFDPTCHR(.LIST,DFN,1)
 ; LIST(I) = IEN ^ NAME ^ ABBREVIATION
 D INDEX(.CHAR,.LIST)
 S X=0 F  S X=$O(CHAR("IEN",X)) Q:'X  D
 .I $D(LIST("IEN",X)) Q  ; Do Nothing
 .I '$D(LIST("IEN",X)) S ADD(X)=""
 S X=0 F  S X=$O(LIST("IEN",X)) Q:'X  D
 .I '$D(CHAR("IEN",X)) D
 ..S Y=$O(LIST("IEN",X,0)),INACT($P(LIST(Y),U,4))=""
 S RET=$$FILE(DFN,VST,.ADD,.INACT)
 Q
 ;
RBROW ; REPORT BROWSER - SHOW ACTIVE CHARACTERISTIC LIST
 N RET,I,ARR,CT,TITLE
 D GET I +$G(RET(1))<1 W $P($G(RET(1)),U,2),!! Q
 ; (NAME) = ABRR ^ CAT
 F I=1:1:2 S ARY(I)=""  ; ADD EMPTY ROWS FOR SPACING
 S CT=$NA(RET) F I=I:1 S CT=$Q(@CT) Q:CT=""  D
 .S ARY(I)=$S($L($P(@CT,U,2))>40:$E($P(@CT,U,2),1,37)_"...",1:$P(@CT,U,2))
 .S $E(ARY(I),44,56)=$P(@CT,U,3),$E(ARY(I),58,79)=$$GET1^DIQ(21602.5,+@CT_",",1.05)
 S $E(TITLE,1)=$$LJ^XLFSTR("Name (listed by priority)",43)
 S $E(TITLE,44)=$$LJ^XLFSTR("Abbreviation",57),$E(TITLE,58)=$$LJ^XLFSTR("Category",79)
 D BROWSE^DDBR("ARY","R",TITLE)
 Q
 ;
 ;=====================================================================
 ;                P R I V A T E   S U B R O U T I N E S
 ;=====================================================================
 ;
FILE(DFN,VDT,ADD,INACT) ; Update PATIENT CHARACTERISTIC Multiple
 ; DFN - pointer to File 2
 ; VDT - Visit date/time
 ; ADD(I) - where I is the IEN in file 21602.5
 ; INACT(I) - where I is the IEN in the PATIENT CHARACTERISTIC Mult
 ;
 N X,I,VFDERR,VFDFDA
 I '$D(ADD),'$D(INACT) Q 1
 I $D(INACT) D
 .S X=0 F  S X=$O(INACT(X)) Q:'X  D
 ..S VFDFDA(2.021605,X_","_DFN_",",.04)=VDT
 ..S VFDFDA(2.021605,X_","_DFN_",",.05)=DUZ
 .D FILE^DIE(,"VFDFDA","VFDERR") K VFDFDA
 I $D(VFDERR) Q "-1^Error Encountered Inactivating"
 I $D(ADD) D
 .S X=0 F I=1:1 S X=$O(ADD(X)) Q:'X  D
 ..S VFDFDA(2.021605,"+"_I_","_DFN_",",.01)=X
 ..S VFDFDA(2.021605,"+"_I_","_DFN_",",.02)=VDT
 ..S VFDFDA(2.021605,"+"_I_","_DFN_",",.03)=DUZ
 .D UPDATE^DIE(,"VFDFDA",,"VFDERR")
 I $D(VFDERR) Q "-1^Error Encountered Adding"
 Q 1
 ;
GET1(DFN,SORT) ; Get active PATIENT CHARACTERISTICS for a patient
 N I,X,VFD,VFDERR,VFDLIST
 D LIST^DIC(2.021605,","_DFN_",","5","PI",,,,,,,"VFD","VFDERR")
 I $D(VFDERR) S RET(1)="-1^"_$G(VFDERR("DIERR",1,"TEXT",1)) Q
 ; VFD("DILIST",I,0) = "2^3^1"
 ;   IEN in multiple ^ IEN in 21602.5 ^ Active
 F I=1:1:+VFD("DILIST",0) D
 .N IEN,TEMP S TEMP=VFD("DILIST",I,0),IEN=$P(TEMP,U,2)
 .Q:'$P(TEMP,U,3)
 .D GETS^DIQ(21602.5,IEN,".01;1.02;1.06;5",,"TEMP","VFDERR")
 .Q:'TEMP(21602.5,IEN_",",5)
 .S VFDLIST(IEN)=IEN_U_TEMP(21602.5,IEN_",",.01)_U_TEMP(21602.5,IEN_",",1.02)_U_+TEMP
 .S VFDLIST("P",TEMP(21602.5,IEN_",",1.06))=IEN
 ;VFDLIST(IEN)=IEN ^ NAME ^ ABBR ^ IEN in Patient Multiple
 ;VFDLIST("P",PRIORITY) = IEN
 I SORT D
 .S X=0 F I=1:1 S X=$O(VFDLIST(X)) Q:'X  S RET(I)=VFDLIST(X)
 I 'SORT D
 .S X=0 F I=1:1 S X=$O(VFDLIST("P",X)) Q:'X  S RET(I)=VFDLIST(VFDLIST("P",X))
 Q
 ;
GETALL(SORT) ; Get all active PATIENT CHARACTERISTICS
 N I,X,INDEX,VFD,VFDERR,VFDSCR
 S VFDSCR="I $$GET1^DIQ(21602.5,Y,5)"
 D LIST^DIC(21602.5,,"1.02;1.06","P",,,,,VFDSCR,,"VFD","VFDERR")
 I $D(VFDERR) S RET(1)="-1^"_$G(VFDERR("DIERR",1,"TEXT",1)) Q
 F I=1:1:+VFD("DILIST",0) D
 .S VFD("I",+VFD("DILIST",I,0))=I
 .S VFD("P",$P(VFD("DILIST",I,0),U,4))=I
 S INDEX=$S(SORT:"I",1:"P")
 S X=0 F I=1:1 S X=$O(VFD(INDEX,X)) Q:'X  D
 .S RET(I)=$P(VFD("DILIST",VFD(INDEX,X),0),U,1,3)
 Q
 ;
INDEX(CHAR,LIST) ;
 ; CHAR(I) = IEN
 ; CHAR("IEN",IEN,I) = ""
 ; LIST(I) = IEN ^ NAME ^ ABBREVIATION
 ; LIST("IEN",IEN,I) = ""
 ;
 N I F I=1:1 Q:'$D(CHAR(I))&'$D(LIST(I))  D
 .I $D(CHAR(I)) S CHAR("IEN",CHAR(I),I)=""
 .I $D(LIST(I)) S LIST("IEN",+LIST(I),I)=""
 Q
 ;
