VFDORDSR ;DSS/SMP - DATA SEGREGATION REPORTING ; 07/23/2014
 ;;2013.0;DSS,INC VXVISTA OPEN SOURCE;;01 Jan 2014;Build 4
 ;Copyright 1995-2014,Document Storage Systems Inc. All Rights Reserved
 ;
 Q
 ;
 ; NYS ID Number - Alternate ID, Default MRN
 ; Case Number - Alternate ID, division specific.  So the report will
 ;               have to pull the Case# for the division that the
 ;               patient is currently assigned
 ; Patient Name
 ; Facility Name (of the EOC being written on)
 ; Instance Name (i.e., Lab Order)
 ; Event (Add/Update/Delete)
 ; Event Date and Time
 ; Employee ID Number
 ; Justification
 ; Ckey (whatever unique identifier there is attached to this in case
 ;       this needed to be traced). 
 ;
RPT1 ; Tasked Job
 ;<ignore>
 ; **** Scott's Comment - PLEASE IGNORE ****
 ; Runs for all sites and prints site specific reports to site specific
 ; printers that are defined by a parameter.  Then compiles all the
 ; data and sends it to Central Office printer.
 ;</ignore>
 ;
 N EDATE,RET,RPT,SDATE,SITE
 S EDATE=$$NOW^XLFDT,SDATE=$$FMADD^XLFDT(EDATE,-1,0,0,0)
 D ALLSITE1,EN,OUT(.RPT)
 Q
 ;
RPT2 ; Ad-Hoc
 ; Ad-Hoc Report that asks them for a site or sites and a date range
 N EDATE,PATID,PROVID,RET,RPT,SDATE,SITE
 Q:'$$ASKTYPE(.PATID,.PROVID)
 D ASKSITE Q:'$D(SITE)
 S SDATE=$$ASKDATE(1) Q:SDATE=-1
 S EDATE=$$ASKDATE(2,SDATE) Q:EDATE=-1
 D EN,OUT(.RPT)
 Q
 ;
EN ; COMMON ENTRY POINT
 S RPT(1)="DATA SEGREGATION REPORT"_$$RJ^XLFSTR($$FMTE^XLFDT($$NOW^XLFDT),57)
 S RPT(2)=$$REPEAT^XLFSTR("-",80),RPT(3)=" ",CNT=4
 D GETJUST(.RET,.SITE,SDATE,EDATE)
 D GETDATA(.RPT,.RET)
 Q:$D(RPT(4))
 S RPT(4)="Nothing to report.",RPT(5)=" "
 Q
 ;
 ;=====================================================================
 ;                P R I V A T E  S U B R O U T I N E S
 ;=====================================================================
 ;
ALLSITE() ; ASK IF THEY WANT ALL SITES
 N X,Y,DIR,DTOUT,DUOUT,DIRUT,DIROUT,VFD,VFDERR
 S DIR(0)="Y",DIR("B")="NO"
 S DIR("A")="Would you like to run this report for all sites"
 W ! D ^DIR W !
 I $D(DIRUT) Q -1
 I Y=1 D ALLSITE1 Q 1
 Q 0
 ;
ALLSITE1 ; ADD ALL SITES
 N VFD,VFDERR,X
 D LIST^DIC(4,,".01","P",,,,,,,"VFD","VFDER")
 S X=0 F  S X=$O(VFD("DILIST",X)) Q:'X  S SITE(+VFD("DILIST",X,0))=""
 Q
 ;
ASKTYPE(PAT,PROV) ;
 N I,X,Y,DIR,DTOUT,DUOUT,DIRUT,DIROUT
 S DIR(0)="FOA",DIR("?")="^D HELP^VFDORDSR(1)"
 S DIR("A")="Enter a type of ALTERNATE IDENTIFIER for the patients:  "
 F I=1:1 D  Q:X=""
 .D ^DIR W ! Q:X=""
 .S:X'="MRN" PAT(I)=X
 .S DIR("A")="Enter another:  "
 I $D(DTOUT)!($D(DUOUT)) K PAT Q 0
 K DIR,DTOUT,DUOUT,DIRUT,DIROUT W !!!!
 S DIR(0)="FOA",DIR("?")="^D HELP^VFDORDSR(2)"
 S DIR("A")="Enter a type of IDENTIFIER for the employee:  "
 F I=1:1 D  Q:X=""
 .D ^DIR W ! Q:X=""
 .S PROV(I)=X
 .S DIR("A")="Enter another:  "
 I $D(DTOUT)!($D(DUOUT)) K PROV Q 0
 Q 1
 ;
HELP(TYPE) ;
 N X,I,LNCNT,MSG,TEMP
 S MSG("DIHELP",1)="Current ALTERNATE IDENTIFIER types on the system: "
 S MSG("DIHELP",2)="  ",LNCNT=3
 I TYPE=1 D
 .S X="" F  S X=$O(^DPT("AVFDAI",X)) Q:X=""  D
 ..I X'="MRN" S MSG("DIHELP",LNCNT)=" "_X,LNCNT=LNCNT+1
 I TYPE=2 D
 .S X="" F  S X=$O(^VA(200,"AVFDT",X)) Q:X=""  S TEMP(X)=""
 .S TEMP("NPI")="",TEMP("DEA")=""
 .S X="" F  S X=$O(TEMP(X)) Q:X=""  S MSG("DIHELP",LNCNT)=" "_X,LNCNT=LNCNT+1
 I '$D(MSG("DIHELP",3)) K MSG D 
 .I TYPE=1 S MSG("DIHELP",1)="No non-MRN ALTERNATE IDENTIFIERS found for PATIENTS"
 .I TYPE=2 S MSG("DIHELP",1)="No IDENTIFERS found for EMPLOYEES"
 D MSG^DIALOG("WH",,80,,"MSG")
 Q
 ;
ASKDATE(LINE,MIN) ; ASK FOR START/END DATE
 ;;Start Date:  
 ;;End Date:  
 ;
 N X,Y,DIR,DTOUT,DUOUT,DIRUT,DIROUT
 S DIR(0)="DA^"_$G(MIN)_"::EXT"
 S DIR("A")=$P($T(ASKDATE+LINE),";;",2)
 D ^DIR W !
 I $D(DIRUT) Q -1
 I Y'["." D
 .I LINE=1 S Y=Y-.000001
 .I LINE=2 S Y=Y+.999999
 Q Y
 ;
ASKSITE ; ASK FOR A SITE
 N X,Y,DIC,DTOUT,DUOUT
 Q:$$ALLSITE
 S DIC=4,DIC(0)="AENOQ"
 F  D  Q:X=""
 .D ^DIC W ! Q:X=""
 .I $D(DUOUT)!($D(DTOUT)) K SITE S X="" Q
 .S SITE(+Y)=""
 Q
 ;
COMMON(OUT,JUST,PROV,DFN) ;
 N X,Y,Z
 S OUT(CNT)="JUSTIFICATION ID:  "_$$LJ^XLFSTR($P(JUST,U,3),21)
 S OUT(CNT)=OUT(CNT)_"START DATE/TIME:  "_$P(JUST,U,11),CNT=CNT+1
 S OUT(CNT)="FACILITY:  "_$$LJ^XLFSTR($$GET1^DIQ(44,$P(JUST,U,8),3,),31)
 S OUT(CNT)=OUT(CNT)_"END DATE/TIME:  "_$P(JUST,U,13),CNT=CNT+1
 S OUT(CNT)="PATIENT:  "_$$LJ^XLFSTR($P(JUST,U,7),30)
 S CNT=$$PATID(.OUT,.PATID,DFN,$$GET1^DIQ(44,$P(JUST,U,8),3,"I"),CNT)
 S OUT(CNT)="PROVIDER:  "_$$LJ^XLFSTR($P(JUST,U,5),30),CNT=CNT+1
 S CNT=$$PROVID(.OUT,.PROVID,$$GET1^DIQ(44,$P(JUST,U,8),3,"I"),CNT)
 S OUT(CNT)="FACILITY:  "_$$GET1^DIQ(44,$P(JUST,U,8),3,),CNT=CNT+1
 S OUT(CNT)=" ",CNT=CNT+1,OUT(CNT)="JUSTIFICATION:  ",CNT=CNT+1
 F X=1:1 Q:'$D(JUST(X))  S OUT(CNT)=JUST(X),CNT=CNT+1
 S OUT(CNT)=" ",CNT=CNT+1
 Q
 ;
PATID(OUT,PATID,DFN,LOC,CNT) ;
 N X,Y,Z
 S X=$$GET^XPAR("SYS","VFD PATIENT ID LABEL",1,"Q") S:X="" X="MRN"
 S Y=$$ID^VFDDFN(DFN,,"MRN"),Z=$$RJ^XLFSTR(X,15)_":  "_Y
 I $L(Z)<40 S OUT(CNT)=OUT(CNT)_Z
 E  S CNT=CNT+1 S OUT(CNT)=X_":  "_Y
 S CNT=CNT+1
 I $D(PATID) F X=1:1 Q:'$D(PATID(X))  D
 .S Y=$$ID^VFDDFN(DFN,LOC,PATID(X)) I $P(Y,U)=-1 Q
 .S OUT(CNT)=PATID(X)_":  "_Y,CNT=CNT+1
 Q CNT
 ;
PROVID(OUT,PROVID,LOC,CNT) ;
 N X,Y,Z,RET
 D ID^VFDCDUZ(.RET,PROV,"DNSTvV") ; RPC: VFDC USER ID
 I $D(PROVID) F X=1:1 Q:'$D(PROVID(X))  D
 .I PROVID(X)="SSN",$D(RET(1)) S OUT(CNT)="PROVIDER "_PROVID(X)_":  "_$P(RET(1),U,2),CNT=CNT+1
 .I PROVID(X)="NPI",$D(RET(2)) S OUT(CNT)="PROVIDER "_PROVID(X)_":  "_$P(RET(2),U,2),CNT=CNT+1
 .I PROVID(X)="DEA",$D(RET(3)) S OUT(CNT)="PROVIDER "_PROVID(X)_":  "_$P(RET(3),U,2),CNT=CNT+1
 .I PROVID(X)="VA",$D(RET(4)) S OUT(CNT)="PROVIDER "_PROVID(X)_":  "_$P(RET(4),U,2),CNT=CNT+1
 .I PROVID(X)="TAX",$D(RET(5)) S OUT(CNT)="PROVIDER "_PROVID(X)_":  "_$P(RET(5),U,2),CNT=CNT+1
 .I PROVID(X)="VPID",$D(RET(6)) S OUT(CNT)="PROVIDER "_PROVID(X)_":  "_$P(RET(6),U,2),CNT=CNT+1
 Q CNT
 ;
 ;
DT(X) ;
 Q $$FMTE^XLFDT(X)
 ;
GETDATA(RPT,DATA) ; Gather Information
 N X,Y,Z,I,LOC,JUST
 K ^TMP("VFDORDSR",$J)
 S LOC=0 F  S LOC=$O(DATA(LOC)) Q:'LOC  D
 .F I=1:1 Q:'$D(DATA(LOC,I))  D
 ..N PROV,DFN,SDT,EDT
 ..K X M X=DATA(LOC,I)
 ..S PROV=$P(X,U,4),DFN=$P(X,U,6),SDT=$P(X,U,10),EDT=$P(X,U,12)
 ..D ORDER(DFN,PROV,SDT,EDT,$NA(DATA(LOC,I)))
 ..D TIU(DFN,PROV,SDT,EDT,$NA(DATA(LOC,I)),$P(DATA(LOC,I),U,8))
 ..D GMR(DFN,PROV,SDT,EDT,$NA(DATA(LOC,I)))
 ..D ORQ(DFN,PROV,SDT,EDT,$NA(DATA(LOC,I)))
 ..D GMRV(DFN,PROV,SDT,EDT,$NA(DATA(LOC,I)))
 ..D RPT(.RPT,$NA(DATA(LOC,I)),PROV,DFN)
 K ^TMP("VFDORDSR",$J)
 Q
 ;
GETJUST(RET,SITE,START,END) ; Return list of entries that match
 ;  Data pulled from VFD JUSTIFICATION file (21620) 
 ;
 ; FIELD     FIELD
 ; NUMBER    NAME
 ; 
 ; .01       ID NUMBER (RNJ14,0), [0;1]
 ; .02       PROVIDER (RP200'), [0;2]
 ; .03       PATIENT (RP2'), [0;3]
 ; .04       LOCATION (RP44'), [0;4]
 ; .05       START DATE/TIME (D), [0;5]
 ; .06       END DATE/TIME (D), [0;6]
 ; 1         JUSTIFICATION (Multiple-21620.0001), [1;0]
 ;           .01  JUSTIFICATION (Wx), [0;1]
 ; 
 ;  1   IEN
 ;  2   IX(1)
 ;  3   ID Number
 ;  4   Provider (Internal)
 ;  5   Provider (External)
 ;  6   Patient (Internal)
 ;  7   Patient (External)
 ;  8   Location (Internal)
 ;  9   Location (External)
 ;  10  Start Date/Time (Internal)
 ;  11  Start Date/Time (External)
 ;  12  End Date/Time (Internal)
 ;  13  End Date/Time (External)
 ;
 N X,Y,FLDS,NA,SCRN,VFD,VFDERR
 S FLDS=".01;.02IE;.03IE;.04IE;.05IE;.06IE"
 S SCRN="I $P(^(0),U,5)>"_START_",$P(^(0),U,5)<"_END
 D LIST^DIC(21620,,FLDS,"P",,,,,SCRN,,"VFD","VFDERR") Q:$D(VFDERR)
 S NA=$NA(VFD("DILIST")),X=0
 F  S X=$O(@NA@(X)) Q:'X  D
 .N I,JUST,LOC S Y=@NA@(X,0),LOC=$$GET1^DIQ(44,$P(Y,U,8),3,"I") Q:'LOC
 .Q:'$D(SITE(LOC))
 .S I=1+$G(RET("B",LOC))
 .S RET(LOC,I)=Y,RET("B",LOC)=I
 .S Y=$$GET1^DIQ(21620,+Y,1,,"JUST","VFDERR") Q:$D(VFDERR)
 .M RET(LOC,I)=JUST
 Q
 ;
ORDER(DFN,PROV,SDT,EDT,NA) ; Get Orders
 ; RPC - ORWORR AGET - D AGET^ORWORR(.RET,DFN,1,1,0,0,,0)
 ; RPC - ORWORR GET4LST(.RET,$$NOW^XLFDT,.LIST)
 ;
 N X,Y,Z,I,ORRET
 K ^TMP("ORR",$J)
 S SDT=$E(SDT,1,12),EDT=$E(EDT,1,12)
 D AGET^ORWORR(.ORRET,DFN,1,1,0,0,,0)
 ; AGET(REF,DFN,FILTER,GROUPS,DTFROM,DTTHRU,EVENT,ORRECIP)
 F I=1:1:@ORRET@(.1) S X=@ORRET@(I) D
 .N OR,J S J=1 D ORFIND(.OR,$P(X,U),PROV,SDT,EDT) Q:'$D(OR)
 .I $$ORDER1(+X,6),$G(OR(6)) D
 ..S @NA@("OR",+X,J)="Created:        "_$$DT(OR(6)),J=J+1
 .I $$ORDER1(+X,2),$G(OR(2)) D
 ..S @NA@("OR",+X,J)="Signed:         "_$$DT(OR(2)),J=J+1
 .I $$ORDER1(+X,7),$G(OR(7)) D
 ..S @NA@("OR",+X,J)="Discontinued:   "_$$DT(OR(7)),J=J+1
 .I $$ORDER1(+X,3),$G(OR(3)) D
 ..S @NA@("OR",+X,J)="Released:       "_$$DT(OR(3)),J=J+1
 .I $$ORDER1(+X,4),$G(OR(4)) D
 ..S @NA@("OR",+X,J)="Flagged:        "_$$DT(OR(4)),J=J+1
 .I $$ORDER1(+X,5),$G(OR(5)) D
 ..S @NA@("OR",+X,J)="Unflagged:      "_$$DT(OR(5)),J=J+1
 .I $$ORDER1(+X,8),$G(OR(8)) D
 ..S @NA@("OR",+X,J)="Completed:      "_$$DT(OR(8)),J=J+1
 .I $$ORDER1(+X,1),$$ORDER1(+X,6),$G(OR(1)),'$G(OR(6)) D
 ..S @NA@("OR",+X,J)="New Action:     "_$$DT(OR(1)),J=J+1
 K ^TMP("ORR",$J)
 Q
 ;
ORDER1(ORNUM,ACTION) ;
 I $D(^TMP("VFDORDSR",$J,ORNUM,ACTION)) Q 0
 S ^TMP("VFDORDSR",$J,ORNUM,ACTION)=""
 Q 1
 
ORFIND(ARR,OR,PROV,SDT,EDT) ;
 ; Returns ARR(1) if NEW ENTRY IN ORDER ACTION MULTIPLE
 ; Returns ARR(2) if SIGNED
 ; Returns ARR(3) if RELEASED
 ; Returns ARR(4) if FLAGGED
 ; Returns ARR(5) if UNFLAGGED
 ; Returns ARR(6) if NEW ORDER
 ; Returns ARR(7) if DISCONTINUED
 ; Returns ARR(8) if COMPLETED
 ;
 N I,X,Y,FLDS,IENS,NA,ORDATA,VFDERR
 S X=0_U,IENS=$P($P(OR,U),";",2)_","_+OR_","
 S FLDS=".01;3;5;6;16;17;33;34;36;37"
 D GETS^DIQ(100.008,IENS,FLDS,"I","ORDATA","VFDERR") Q:$D(VFDERR)
 S NA=$NA(ORDATA(100.008,IENS))
 ; NEW ENTRY IN ORDER ACTION MULTIPLE - return 1
 ; .01 - DATE/TIME ORDERED
 ; 3   - PROVIDER
 I $$FILTER(PROV,SDT,EDT,.01,3,NA) S ARR(1)=@NA@(.01,"I")
 ; SIGNED ORDER - return 2
 ; 5 - SIGNED BY
 ; 6 - D/T SIGNED
 I $$FILTER(PROV,SDT,EDT,6,5,NA) S ARR(2)=@NA@(6,"I")
 ; RELEASED ORDER - return 3
 ; 16 - RELEASED D/T
 ; 17 - RELEASING PERSON
 I $$FILTER(PROV,SDT,EDT,16,17,NA) S ARR(3)=@NA@(16,"I")
 ; FLAGGED ORDER - return 4
 ; 33 - DATE/TIME FLAGGED
 ; 34 - FLAGGED BY
 I $$FILTER(PROV,SDT,EDT,33,34,NA) S ARR(4)=@NA@(33,"I")
 ; UNFLAGGED ORDER - return 5
 ; 36 - DATE/TIME UNFLAGGED
 ; 37 - UNFLAGGED BY
 I $$FILTER(PROV,SDT,EDT,36,37,NA) S ARR(5)=@NA@(36,"I")
 K ORDATA S FLDS="3;4;62;63;66;67",IENS=+OR_","
 D GETS^DIQ(100,IENS,FLDS,"I","ORDATA","VFDERR") Q:$D(VFDERR)
 S NA=$NA(ORDATA(100,IENS))
 ; NEW ORDER - return 6
 ; 3 - WHO ENTERED
 ; 4 - WHEN ENTERED
 I $$FILTER(PROV,SDT,EDT,4,3,NA) S ARR(6)=@NA@(4,"I")
 ; DISCONTINUED ORDER - return 7
 ; 62 - DC'ed BY
 ; 63 - DC DATE/TIME
 I $$FILTER(PROV,SDT,EDT,63,62,NA) S ARR(7)=@NA@(63,"I")
 ; COMPLETED ORDER - return 8
 ; 66 - COMPLETED
 ; 67 - COMPLETED BY
 I $$FILTER(PROV,SDT,EDT,66,67,NA) S ARR(8)=@NA@(66,"I")
 Q
 ;
FILTER(PROV,SDT,EDT,DTFLD,NPFLD,NA) ;
 ; PROV  - pointer to file 200
 ; SDT   - start date/time
 ; EDT   - end date/tim
 ; DTFLD - field number for the date/time
 ; NPFLD - field number for the person (file 200)
 ; NA    - array where data can be found ex. NA = ORDATA(100.008,iens)
 N X,Y
 S X=$G(@NA@(DTFLD,"I"))
 S Y=$G(@NA@(NPFLD,"I"))
 I $$DTCOMP(X,SDT,EDT),$$PROVCOMP(Y,PROV) Q 1
 Q 0
 ;
PROVCOMP(OR,PROV) ; Check provider
 Q OR=PROV
 ;
DTCOMP(OR,SDT,EDT) ; Check date vs Start and End Dates
 I OR>=SDT,OR<=EDT Q 1
 Q 0
 ;
RPT(OUT,NA,PROV,DFN) ;
 N X,Y,Z M X=@NA
 D COMMON(.RPT,.X,PROV,DFN)
 D RPT0(1),RPT0(2),RPT0(3),RPT0(4),RPT0(5)
 I '$D(DATA(LOC,I,"OR")),'$D(DATA(LOC,I,"TIU")),'$D(DATA(LOC,I,"GMR")),'$D(DATA(LOC,I,"ORQ")),'$D(DATA(LOC,I,"GMRV")) D
 .S RPT(CNT)="No reportable actions found.",CNT=CNT+1
 .S RPT(CNT)=" ",CNT=CNT+1
 S RPT(CNT)=$$REPEAT^XLFSTR("-",80),CNT=CNT+1
 Q
 ;
RPT0(LINE) ;
 ;;OR;ORDER ENTRY
 ;;TIU;TIU DOCUMENT
 ;;GMR;ALLERGY ENTRY
 ;;ORQ;PROBLEM ENTRY
 ;;GMRV;VITALS ENTRY
 ;
 N X,LBL,TITLE
 S X=$T(RPT0+LINE),LBL=$P(X,";",3),TITLE=$P(X,";",4)
 S X=0 F  S X=$O(@NA@(LBL,X)) Q:'X  D
 .S OUT(CNT)=TITLE_":  "_X,CNT=CNT+1
 .S Y=0 F  S Y=$O(@NA@(LBL,X,Y)) Q:'Y  D
 ..S OUT(CNT)=@NA@(LBL,X,Y),CNT=CNT+1
 .S OUT(CNT)=" ",CNT=CNT+1
 Q
 ;
TIU(DFN,PROV,SDT,EDT,NA,TIULOC) ; Notes
 N TIUIEN,ENTDT,REFDT,SGNDT
 S TIUIEN=0 F  S TIUIEN=$O(^TIU(8925,TIUIEN)) Q:'TIUIEN  D
 .I $P($G(^TIU(8925,TIUIEN,0)),"^",2)=DFN
 .I $P($G(^TIU(8925,TIUIEN,12)),"^",11)=TIULOC  D
 ..S ENTDT=$P($G(^TIU(8925,TIUIEN,12)),"^") I $$DTCOMP(ENTDT,SDT,EDT) D
 ...I PROV=$P($G(^TIU(8925,TIUIEN,12)),"^",2) S @NA@("TIU",TIUIEN,1)="Entered:   "_$$DT(ENTDT)
 ..S REFDT=$P($G(^TIU(8925,TIUIEN,13)),"^") I $$DTCOMP(REFDT,SDT,EDT) D
 ...I PROV=$P($G(^TIU(8925,TIUIEN,13)),"^",2) S @NA@("TIU",TIUIEN,2)="Reference:   "_$$DT(REFDT)
 ..S SGNDT=$P($G(^TIU(8925,TIUIEN,15)),"^") I $$DTCOMP(SGNDT,SDT,EDT) D
 ...I PROV=$P($G(^TIU(8925,TIUIEN,15)),"^",2) S @NA@("TIU",TIUIEN,3)="Signed:   "_$$DT(SGNDT)
 ;
 ;
 ; CONSULTS
 ; --------
 ; ORQQCN LIST
 ; ORQQCN DETAIL
 ; ORQQCN MED RESULTS
 ;
 Q
 ;
GMR(DFN,PROV,SDT,EDT,NA) ;Allergies
 N IEN,GMDATE,ERRDATE,X,Y,I,ERIEN
 S GMDATE="" F  S GMDATE=$O(^GMR(120.8,"AODT",GMDATE)) Q:GMDATE<1  D
 .I $$DTCOMP(GMDATE,SDT,EDT) S IEN=0 F  S IEN=$O(^GMR(120.8,"AODT",GMDATE,IEN)) Q:'IEN  D
 ..I $P($G(^GMR(120.8,IEN,0)),U)=DFN,$P($G(^GMR(120.8,IEN,0)),U,5)=PROV
 ..S @NA@("GMR",IEN,1)="Created:        "_$$DT(GMDATE)
 S ERIEN="" F  S ERIEN=$O(^GMR(120.8,ERIEN))  Q:ERIEN=""  D
 .S ERRDATE=$P($G(^GMR(120.8,ERIEN,"ER")),U,2) I $$DTCOMP(ERRDATE,SDT,EDT)  D
 ..I $P($G(^GMR(120.8,ERIEN,"ER")),U,3)=PROV S @NA@("GMR",ERIEN,2)="Entered in Error:        "_$$DT(ERRDATE)
 Q
 ;
ORQ(DFN,PROV,SDT,EDT,NA) ;Problems
 N PIEN,PROBDT,PROBID,X,Y,I
 S PROBDT="" F  S PROBDT=$O(^DIA(9000011,"C",PROBDT)) Q:'PROBDT  D 
 .I $$DTCOMP(PROBDT,SDT,EDT) D
 ..S PIEN="" F  S PIEN=$O(^DIA(9000011,"C",PROBDT,PIEN)) Q:'PIEN  D
 ...Q:$P($G(^DIA(9000011,PIEN,0)),"^",4)'=PROV 
 ...S PROBID=$P($G(^DIA(9000011,PIEN,0)),"^")
 ...I $P($G(^AUPNPROB(PROBID,0)),"^",2)=DFN S @NA@("ORQ",PROBID,1)="Modified:        "_$$DT(PROBDT)
 Q
 ;
GMRV(DFN,PROV,SDT,EDT,NA) ;Vitals
 N I,X,Y,GIEN,GMDT,GMIEN,GMVDT,GID
 S GIEN=0 F  S GIEN=$O(^GMR(120.5,GIEN)) Q:'GIEN  D
 .S GMDT=$P($G(^GMR(120.5,GIEN,0)),U,4) I $$DTCOMP(GMDT,SDT,EDT) D
 ..I $P($G(^GMR(120.5,GIEN,0)),U,2)=DFN,$P($G(^GMR(120.5,GIEN,0)),U,6)=PROV
 ..S @NA@("GMRV",GIEN,1)="Entered:    "_$$DT(GMDT)
 S GMIEN=0 F  S GMIEN=$O(^DIA(120.5,GMIEN)) Q:'GMIEN  D
 .S GMVDT=$P($G(^DIA(120.5,GMIEN,0)),U,2) I $$DTCOMP(GMVDT,SDT,EDT) D
 ..S GID=$P($G(^DIA(120.5,GMIEN,0)),U) I $P($G(^GMR(120.5,GID,0)),U,2)=DFN,$P($G(^GMR(120.5,GID,0)),"^",6)=PROV
 ..S @NA@("GMRV",GID,2)="Entry marked as error:    "_$$DT(GMVDT)
 Q
 ;
 ;
OUT(RPT) ;
 N I,X,Y,DIR,DTOUT,DUOUT,DIRUT,DIROUT
 W @IOF F I=1:1 Q:'$D(RPT(I))  W RPT(I),!
 S DIR(0)="E" W ! D ^DIR W !!
 Q
