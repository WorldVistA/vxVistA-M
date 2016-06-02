VFDPSOMA ;DSS/SMP - VFD PSO MED ADMIN ; 04/24/2015 16:20
 ;;15.0;DSS,INC VXVISTA OPEN SOURCE;**42**;28 Jan 2013;Build 5
 ;Copyright 1995-2015,Document Storage Systems Inc. All Rights Reserved
 ;
 ;--------------------------------------
 ; ICR # | Supported Reference
 ;-------|------------------------------
 ;  2051 |^DIC: $$FIND1
 ;  2336 |^XPAREDIT: GETENT, BLDLST
 ; 10026 |^DIR
 Q
 ;
SAVE(RET,INPUT) ; RPC - [VFD PSO MED ADMIN SAVE]
 ; INPUT(n) - req where INPUT(n) = tag ^ value for n=1,2,3,...
 ; 
 ; CODE  REQ VALUE
 ; ----- --- -----------------------------------------------------------
 ; ORD    Y  Order file (#100) IEN
 ; LOC    Y  Hospital Location file (#44) IEN
 ; DATE   Y  Date/Time of Administration.
 ; WHEN   N  Date/Time of Visit. Defaults to NOW
 ; END    N  End Date/Time of Administration. Defaults to NOW
 ; QTY    N  Quantity Administered
 ; WHO    N  Who Administered - 1 = Patient; 2 = Provider
 ; READ   N  Monitored Re-Administraion Reason
 ; MISS   N  Dose Missed Reason
 ; HELD   N  Dose Held Reason
 ; COM    N  Comments
 ; MULT   N  Multiple Admin Reason
 ;
 ; OUTPUT: 
 ;   RET = IEN
 ;   RET = -1^Error message
 ;
 N I,X,DATA,FLAG,MAP,VFDERR,VFDFDA,VFDIEN,VST
 S X=$P($T(VAR+1),";;",2)
 F I=1:1:$L(X,U) N @$P(X,U,I) S @$P(X,U,I)=""
 D INIT(X)
 S VST=$$DATA I VST["-1" Q
 S DATA(.02)=VST
 D MAP(.MAP,.DATA)
 F X=.01,.02,1.01,1.02 I '$D(DATA(X)) S FLAG=1
 I $G(FLAG) S RET="-1^Insufficient Input" Q
 S X=0 F  S X=$O(DATA(X)) Q:'X  S VFDFDA(21653.79,"+1,",X)=DATA(X)
 D UPDATE^DIE(,"VFDFDA","VFDIEN","VFDERR")
 I $D(VFDERR) S RET="-1^Error Filing Data" Q
 S RET=$G(VFDIEN(1))
 Q
 ;
DATA() ; Validate Data
 N X,Y,Z,DFN,DFNR,PREF
 I 'ORD Q "-1^No Order number sent"
 I '$D(^OR(100,ORD)) Q "-1^Invalid Order number"
 S PREF=$G(^OR(100,ORD,4))
 S DFNR=$P(^OR(100,ORD,0),U,2) I DFNR["DPT" S DFN=+DFNR
 I '$D(DFN)!(DFN="") Q "-1^No DFN for order"
 I PREF["P" Q "-1^Order in PENDING status."
 I 'DATE Q "-1^No Administration Date"
 I LOC="" Q "-1^Invalid data for Location"
 I 'WHEN S WHEN=$E($$NOW^XLFDT,1,12)
 I 'END S END=$E($$NOW^XLFDT,1,12)
 ;
PCEDATA ; Create Visit
 N OK,PKG,VFDRA,VISITIEN,VFDSCR
 S VFDRA("ENCOUNTER",1,"ENC D/T")=WHEN
 S VFDRA("ENCOUNTER",1,"PATIENT")=DFN
 S VFDRA("ENCOUNTER",1,"HOS LOC")=LOC
 S VFDRA("ENCOUNTER",1,"SERVICE CATEGORY")="X" ;Ancillary Package Daily Data
 S VFDRA("ENCOUNTER",1,"ENCOUNTER TYPE")="A" ;Ancillary
 S VFDRA("ENCOUNTER",1,"CHECKOUT D/T")=END
 S PKG=$O(^DIC(9.4,"C","VFD","")),VFDSCR="VFD PSO MED ADMIN"
 S OK=$$DATA2PCE^PXAPI("VFDRA",PKG,VFDSCR,.VISITIEN)
 I '$D(VISITIEN)!(VISITIEN="") Q "-1^No visit returned from DATA2PCE"
 I '$D(^AUPNVSIT(VISITIEN,0)) Q "-1^No Visit record"
 Q VISITIEN
 ;
INIT(TXT) ;
 N X,Y
 S X=0 F  S X=$O(INPUT(X)) Q:X=""  S Y=INPUT(X),@$P(Y,U)=$P(Y,U,2)
 Q
 ;
MAP(MAP,DATA) ; Map the TAGS and Fields
 ;;.01^ORD
 ;;.02^VST
 ;;1.01^DATE
 ;;1.02^QTY
 ;;1.03^WHO
 ;;2^READ
 ;;3.02^MISS
 ;;4.02^HELD
 ;;6^COM
 ;;7^MULT
 ;;
 N I,TXT
 F I=1:1 S TXT=$P($T(MAP+I),";",3,99) Q:TXT=""  D
 .S MAP($P(TXT,U,2))=$P(TXT,U)
 .I $G(@$P(TXT,U,2))'="" S DATA($P(TXT,U))=@$P(TXT,U,2)
 S DATA(.03)=DUZ
 I '$D(DATA(1.02)) S DATA(1.02)=0
 I $D(DATA(3.02)) S DATA(3.01)=1
 I $D(DATA(4.02)) S DATA(4.01)=1
 Q
 ;
VAR ;
 ;;COM^DATE^END^HELD^LOC^MISS^ORD^QTY^READ^WHEN^WHO
 ;
 Q
 ;
 ;=====================================================================
 ;
 ;=====================================================================
ERR ; Option:  VFD PSO MED ADMIN ERROR
 N X,Y,Z,ERR,VFD,VFDERR,VFDFDA
 S VFD=$$DIC Q:VFD=-1  D DISPLAY(VFD)
 Q:'$$VERIFY("Is this the correct entry?  ")
 S ERR=$$DIR Q:ERR=""
 S VFDFDA(21653.79,VFD_",",5.01)=$$NOW^XLFDT
 S VFDFDA(21653.79,VFD_",",5.02)=DUZ
 S VFDFDA(21653.79,VFD_",",5.03)=ERR
 Q:'$$VERIFY("Are you sure you want to mark this entry as ENTERED IN ERROR?  ")
 D FILE^DIE(,"VFDFDA","VFDERR")
 I $D(VFDERR) W "Error updating",!
 E  W !,"Record marked ENTERED IN ERROR.",!!
 Q
 ;
DIC() ; Lookup entry in 21653.79
 N X,Y,DIC,DTOUT,DUOUT
 S DIC=21653.79,DIC(0)="AEQMZ",DIC("S")="I $G(^(5))="""""
 W ! D ^DIC W !
 I $D(DUOUT)!$D(DTOUT) Q -1
 Q +Y
 ;
DIR() ; Read Reason Entered in Error
 N X,Y,DIR,DTOUT,DUOUT,DIRUT,DIROUT
 S DIR(0)="FAO^3;150",DIR("A")="Reason Entered in Error:  "
 D ^DIR W !
 I $D(DIRUT) Q ""
 Q X
 ;
DISPLAY(VFD) ; Display info about entry
 N X,Y,Z,VFDATA,VFDERR
 D GETS^DIQ(21653.79,VFD_",","**","IE","VFDATA","VFDERR")
 D PARSE(.VFDATA,VFD)
 W !,"ORDER:  "_$$LJ(VFDATA(.01),36)_"VISIT D/T:  "_VFDATA(.02),!
 W " PATIENT:"_$$LJ(VFDATA("PAT"),35)_"ADMIN D/T:  "_VFDATA(1.01),!
 W " USER:  "_$$LJ(VFDATA(.03),36),!
 W "ADMINISTERED BY:  "_$$LJ($G(VFDATA(1.03)),14)
 W "QUANTITY ADMINISTERED:  "_$G(VFDATA(1.02)),!
 I $D(VFDATA(2)) W !,"MONITORED RE-ADMIN REASON:",!,VFDATA(2),!
 I $D(VFDATA(3.01))  W !,"DOSE MISSED REASON:",!,VFDATA(3.02),!
 I $D(VFDATA(4.01)) W !,"DOSE HELD REASON:",!,VFDATA(4.02),!
 Q
 ;
LJ(STR,NUM) ;
 Q $$LJ^XLFSTR(STR,NUM)
 ;
PARSE(VFDATA,IEN) ;
 N X,TEMP
 M TEMP=VFDATA K VFDATA
 S X="TEMP" F  S X=$Q(@X) Q:X=""  I $QS(X,4)="E",@X'="" S VFDATA($QS(X,3))=@X
 S VFDATA("PAT")=$$GET1^DIQ(9000010,TEMP(21653.79,IEN_",",.02,"I"),.05)
 Q
 ;
VERIFY(A) ;
 N X,Y,DIR,DTOUT,DUOUT,DIRUT,DIROUT
 S DIR(0)="YA",DIR("A")=A,DIR("B")="NO"
 D ^DIR W !
 I $D(DIRUT) Q ""
 Q +Y
 ;
ADMIN(VFDADMIN,VFDADMOR,VFDADMDT) ; Get admin(s) for ORDER from VFD PSO MED ADMIN (#21653.79)
 ; Called from RPC VFD PSO GET ADMIN
 ;
 ; Input
 ;   VFDADMOR = IEN of ORDER file #100
 ; Output
 ;   VFDADMIN(1)=1^<Date/Time>^<User IEN>
 ;   VFDADMIN(n)=n^. . .
 ;   The array will be returned in reverse chronological order (most recent first)
 ;
 N CNT  ; Count of matches
 N X,Y  ; FileMan scratch
 N RETURN,ERRMSG  ; FileMan results, errors
 N ADTSCR  ; Screen for FileMan
 ;
 K VFDADMIN S VFDADMIN(1)=0  ; Default to nothing returned
 ;  Order (#100) file IEN must be numeric
 I '$G(VFDADMOR) S VFDADMIN(1)="-1^BLANK OR INVALID ORDER NUMBER" Q
 ;  Default to today's date if not passed in
 S:'$G(VFDADMDT) VFDADMDT=$P($$NOW^XLFDT(),".")
 ;  Set up screen
 S ADTSCR="I $P($P(^VFD(21653.79,Y,1),""^""),""."")=VFDADMDT"
 ;
 ;  Use FM to find match(es). If error, return to calling process
 D FIND^DIC(21653.79,"","@;.01;.03;1.01","P",VFDADMOR,,,ADTSCR,,"RETURN","ERRMSG")
 I $G(ERRMSG("DIERR",1,"TEXT",1))]"" S VFDADMIN="-1^"_ERRMSG("DIERR",1,"TEXT",1) Q
 ;  Build RPC return array from FileMan array
 S CNT=0 F  S CNT=$O(RETURN("DILIST",CNT)) Q:'CNT  D
 .Q:$P($G(RETURN("DILIST",CNT,0)),"^",2)'=VFDADMOR
 .S VFDADMIN(CNT)=CNT_"^"_$P($G(RETURN("DILIST",CNT,0)),"^",4)_"^"_$P($G(RETURN("DILIST",CNT,0)),"^",3),VFDADMIN=CNT
 Q
 ;
TESTING ;
 ;12345678901234567890123456789012345678901234567890123456789012345678901234567890
 ;ORDER:  123456789                           VISIT D/T:  APR 27, 2015@11:56  
 ; PATIENT:  12345678901234567890123456789    ADMIN D/T:  APR 27, 2015@11:30
 ;    USER:  12345678901234567890123456789    
 ; ADMINISTERD BY:  PATIENTX      QUATITY ADMINISTERRED: 
 ;
 ;
 ;
 ;
 ;
 ;
