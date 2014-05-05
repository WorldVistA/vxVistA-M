TIUSRV ; SLC/JER - Silent server functions ; 3/28/06 3:01pm
 ;;1.0;TEXT INTEGRATION UTILITIES;**1,19,28,87,61,100,109,113,112,184,211**;Jun 20, 1997;Build 26
RPC(TIUY,TIUDA,REASSIGN) ; RPC for DT
 N VALMAR,TIUGDATA,TIUGWHOL K ^TMP("TIUAUDIT",$J)
 S TIUY=$NA(^TMP("TIUAUDIT",$J))
 D GET(TIUDA,1,+$G(REASSIGN))
 K ^TMP("VALM VIDEO",$J)
 Q
GET(TIUDA,HUSH,REASSIGN) ; Build List
 N TIUI,TIUL,TIUREC,TIUDADD,X,TIUCPF,ONBROWSE
 S (TIUDADD,TIUI,VALMCNT)=0,HUSH=+$G(HUSH)
 N DA,DIC,DIQ,DR,TIUNAME K ^TMP("TIUAUDIT",$J)
 I '$D(TIUPRM0) D SETPARM^TIULE
 I '$D(IOINORM) S X="IOINORM;IOIHI;IORVON;IORVOFF;IOUON;IOUOFF;IOBON;IOBOFF" D ENDR^%ZISS
 S:'$D(VALMAR) VALMAR="^TMP(""TIUAUDIT"",$J)"
 S VALMEVL=+$G(VALMEVL)
 I '$D(^TIU(8925,+TIUDA,0)) S VALMQUIT=1 Q
 ; if the document has an OnBrowse Event, execute it
 S ONBROWSE=$$ONBROWSE^TIULC1(+$G(^TIU(8925,+TIUDA,0)))
 I $L(ONBROWSE) D LOADSUPP(ONBROWSE,TIUDA,.VALMCNT)
 ;Set a flag to indicate whether or not a Title is a member of the
 ;Clinical Procedures Class (1=Yes and 0=No)
 S TIUCPF=+$$ISA^TIULX(+$G(^TIU(8925,TIUDA,0)),+$$CLASS^TIUCP)
 S DIC=8925,DIQ="TIUREC(",DA=TIUDA
 S DR=".01;.02;.05;.07:.1;1201;1202;1204;1208;1212;1301;1302;1305;1306;1501;1502;1505;1507;1508;1511;1601:1602;1610:1612;1701;89261"
 ;If the document is a member of the Clinical Procedures Class, include the
 ;Procedure Summary Code field and the Date/Time Performed field
 I TIUCPF S DR=DR_";70201;70202"
 D EN^DIQ1
 S TIUI="" F  S TIUI=$O(TIUREC(8925,+TIUDA,TIUI)) Q:+TIUI'>0  D
 . I $G(TIUREC(8925,+TIUDA,TIUI))']"" S TIUREC(8925,+TIUDA,TIUI)="None"
 . E  S TIUREC(8925,+TIUDA,TIUI)=$$UP^XLFSTR(TIUREC(8925,+TIUDA,TIUI))
 I $D(TIUREC)>9 D
 . D SOURCE(.TIUREC,HUSH,.VALMCNT,TIUCPF)
 . I '+$G(REASSIGN) D PROBLEM(TIUDA,.VALMCNT) D:$$ISPRFDOC^TIUPRF(TIUDA) LKDETAIL^TIUPRF3(TIUDA,.VALMCNT) D EDIT(TIUDA,.VALMCNT)
 . D REASSIGN^TIUSRV1(TIUDA,+$G(REASSIGN),.VALMCNT) Q:+$G(REASSIGN)
 . D IDLINK^TIUSRV1(TIUDA,.VALMCNT)
 . D SIGN(.TIUREC,.VALMCNT)
 . I +$O(^TIU(8925.7,"B",TIUDA,0)) D XTRASIGN(TIUDA,.VALMCNT)
 . I $D(^TIU(8925,+TIUDA,16)) D PRIVACY(.TIUREC,.VALMCNT)
 . D BODY(TIUDA,.VALMCNT)
 S:+$G(VALMCNT)<$G(VALM("LINES")) VALMCNT=$G(VALM("LINES"))
 Q
LOADSUPP(METHOD,TIUDA,TIUL)     ; Execute OnBrowse/Load Supplementary data
 N TIUY,TIUI S TIUI=0
 X METHOD I '$D(@TIUY) Q
 S TIUL=TIUL+1
 D SET(TIUL,1,"Requesting Package Information ",$G(IORVON),$G(IORVOFF))
 S TIUL=TIUL+1 D BLANK(TIUL)
 F  S TIUI=$O(@TIUY@(TIUI)) Q:+TIUI'>0  D
 . S TIUL=+$G(TIUL)+1 D SET(TIUL,1,@TIUY@(TIUI))
 S TIUL=TIUL+1 D BLANK(TIUL) S TIUL=TIUL+1
 K @TIUY
 Q
SOURCE(TIUREC,HUSH,TIUL,TIUCPF) ; Source Info
 W:'+$G(HUSH) !!,"Opening "_TIUREC(8925,+TIUDA,.01)_" record for review..."
 S TIUL=TIUL+1
 D SET(TIUL,1,"Source Information ",$G(IORVON),$G(IORVOFF))
 D SET(TIUL+1,2," Standard Title: "_$G(TIUREC(8925,TIUDA,89261)))
 D SET(TIUL+2,2," Reference Date: "_$G(TIUREC(8925,TIUDA,1301)))
 D SET(TIUL+3,2,"     Entry Date: "_$G(TIUREC(8925,TIUDA,1201)))
 D SET(TIUL+4,2,"Expected Signer: "_$G(TIUREC(8925,TIUDA,1204)))
 D SET(TIUL+5,2,"        Urgency: "_$G(TIUREC(8925,TIUDA,.09)))
 D SET(TIUL+6,2,"     Line Count: "_$G(TIUREC(8925,TIUDA,.1)))
 D SET(TIUL+7,2,"       Division: "_$G(TIUREC(8925,TIUDA,1212)))
 D SET(TIUL+8,2,"        Subject: "_$G(TIUREC(8925,TIUDA,1701)))
 ;If the document is a member of the Clinical Procedures Class, include the
 ;Procedure Summary Code field and the Date/Time Performed field
 I $G(TIUCPF) D
 . D BLANK(TIUL+9)
 . D SET(TIUL+10,2,"Procedure Summary Code: "_$G(TIUREC(8925,TIUDA,70201)))
 . D SET(TIUL+11,2,"   Date/Time Performed: "_$G(TIUREC(8925,TIUDA,70202)))
 D SET(TIUL+1,40,"            Author: "_$G(TIUREC(8925,TIUDA,1202)))
 D SET(TIUL+2,40,"        Entered By: "_$G(TIUREC(8925,TIUDA,1302)))
 D SET(TIUL+3,40," Expected Cosigner: "_$G(TIUREC(8925,TIUDA,1208)))
 D SET(TIUL+4,40,"   Document Status: "_$G(TIUREC(8925,TIUDA,.05)))
 D SET(TIUL+5,40,"    TIU Document #: "_+$G(TIUDA))
 S TIUL=$S(+$G(TIUCPF):TIUL+11,1:TIUL+8)
 Q
PROBLEM(TIUDA,TIUL) ; Problems
 N TIUI,DR,DIC,DIQ,TIUPROB S TIUI=0
 S TIUL=TIUL+1 D BLANK(TIUL) S TIUL=TIUL+1
 D SET(TIUL,1,"Associated Problems ",$G(IORVON),$G(IORVOFF))
 I '+$O(^TIU(8925.9,"B",+TIUDA,0)) D SET(TIUL,25,"No linked problems.")
 F  S TIUI=$O(^TIU(8925.9,"B",TIUDA,TIUI)) Q:+TIUI'>0  D
 . S DA=TIUI,DR=".02;.05",DIC="^TIU(8925.9,",DIQ="TIUPROB"
 . D EN^DIQ1 Q:$D(TIUPROB)'>9
 . S TIUL=TIUL+1
 . D SET(TIUL,19,$$MIXED^TIULS($G(TIUPROB(8925.9,TIUI,.05))))
 . D SET(TIUL,60,$G(TIUPROB(8925.9,TIUI,.02)))
 Q
EDIT(TIUDA,TIUL) ; Edits
 N TIUI,DR,DIC,DIQ,TIUED S TIUI=0
 S TIUL=TIUL+1 D BLANK(TIUL) S TIUL=TIUL+1
 D SET(TIUL,1,"Edit Information ",$G(IORVON),$G(IORVOFF))
 I '+$O(^TIU(8925.5,"B",+TIUDA,0)) D SET(TIUL,22,"No edits since entry.")
 F  S TIUI=$O(^TIU(8925.5,"B",TIUDA,TIUI)) Q:+TIUI'>0  D
 . S DA=TIUI,DR=".02:03",DIC="^TIU(8925.5,",DIQ="TIUED"
 . D EN^DIQ1 Q:$D(TIUED)'>9!($G(TIUED(8925.5,TIUI,.02))']"")
 . S TIUL=TIUL+1
 . D SET(TIUL,2,"      Edit Date: "_$G(TIUED(8925.5,TIUI,.02)))
 . D SET(TIUL,44,"     Edited By: "_$G(TIUED(8925.5,TIUI,.03)))
 Q
SIGN(TIUREC,TIUL) ; Signature
 S TIUL=TIUL+1 D BLANK(TIUL) S TIUL=TIUL+1
 D SET(TIUL,1,"Signature Information ",$G(IORVON),$G(IORVOFF))
 D SET(TIUL+1,2,"    Signed Date: "_$G(TIUREC(8925,TIUDA,1501)))
 D SET(TIUL+3,2,"  Cosigned Date: "_$G(TIUREC(8925,TIUDA,1507)))
 D SET(TIUL+1,40,"         Signed By: "_$G(TIUREC(8925,TIUDA,1502)))
 D SET(TIUL+2,40,"    Signature Mode: "_$G(TIUREC(8925,TIUDA,1505)))
 D SET(TIUL+3,40,"       Cosigned By: "_$G(TIUREC(8925,TIUDA,1508)))
 D SET(TIUL+4,40,"  Cosignature Mode: "_$G(TIUREC(8925,TIUDA,1511)))
 S TIUL=TIUL+4
 Q
XTRASIGN(TIUDA,TIUL) ; Additional signers
 N TIUI,DA,DR,DIC,DIQ,TIUXTRA
 S TIUI=0
 S TIUL=TIUL+1 D BLANK(TIUL) S TIUL=TIUL+1
 D SET(TIUL,1,"Receipt Acknowledged By ",$G(IORVON),$G(IORVOFF))
 F  S TIUI=$O(^TIU(8925.7,"B",TIUDA,TIUI)) Q:+TIUI'>0  D
 . S DA=TIUI,DR=".03:.08",DIC="^TIU(8925.7,",DIQ="TIUXTRA"
 . D EN^DIQ1 Q:$D(TIUXTRA)'>9
 . S TIUL=TIUL+1
 . D SET(TIUL,2,"    Signed Date: "_$G(TIUXTRA(8925.7,DA,.04)))
 . D SET(TIUL,40,"         Signed By: "_$G(TIUXTRA(8925.7,DA,.06)))
 . S TIUL=TIUL+1
 . D SET(TIUL,2,"Expected Signer: "_$G(TIUXTRA(8925.7,DA,.03)))
 . D SET(TIUL,40,"    Signature Mode: "_$G(TIUXTRA(8925.7,DA,.08)))
 Q
PRIVACY(TIUREC,TIUL) ; Privacy Act
 S TIUL=TIUL+1 D BLANK(TIUL) S TIUL=TIUL+1
 D SET(TIUL,1,"Privacy Act Information ",$G(IORVON),$G(IORVOFF))
 S TIUL=TIUL+1
 D SET(TIUL,2,"   Amended Date: "_$G(TIUREC(8925,TIUDA,1601)))
 D SET(TIUL,40,"        Amended By: "_$G(TIUREC(8925,TIUDA,1602)))
 S TIUL=TIUL+1
 D SET(TIUL,2,"   Deleted Date: "_$G(TIUREC(8925,TIUDA,1611)))
 D SET(TIUL,40,"        Deleted By: "_$G(TIUREC(8925,TIUDA,1610)))
 S TIUL=TIUL+1
 D SET(TIUL,2,"         Reason: "_$G(TIUREC(8925,TIUDA,1612)))
 Q
BODY(TIUDA,TIUL) ; body of document
 N CANSEE
 S TIUL=TIUL+1 D BLANK(TIUL) S TIUL=TIUL+1
 D SET(TIUL,1,"Document Body ",$G(IORVON),$G(IORVOFF))
 S TIUL=TIUL+1 D BLANK(TIUL)
 S CANSEE=$$CANDO^TIULP(TIUDA,"VIEW")
 I '+CANSEE D NOSEE(CANSEE,.TIUL) Q
 I '$D(TIUGDATA) S TIUGDATA=$$IDDATA^TIURECL1(TIUDA)
 D LOADREC^TIUBR1(TIUDA,.TIUL,TIUGDATA,$G(TIUGWHOL))
 K ^TMP("TIU ADDENDUM",$J)
 Q
ISCOMP(DA) ; Evaluate whether a given record is a component
 N TIUY,TIUTYP
 S TIUTYP=+$G(^TIU(8925,DA,0))
 S TIUY=$S($P($G(^TIU(8925.1,+TIUTYP,0)),U,4)="CO":1,1:0)
 Q TIUY
NOSEE(CANSEE,TIUJ) ; When the user shouldn't see the data...
 S TIUJ=+$G(TIUJ)+1
 D SET(TIUJ,2,$P(CANSEE,U,2))
 Q
SET(TIULINE,TIUCOL,TIUTEXT,ON,OFF) ; set display info in array
 D:'$D(@VALMAR@(TIULINE,0)) BLANK(.TIULINE)
 D SET^VALM10(.TIULINE,$$SETSTR^VALM1(.TIUTEXT,@VALMAR@(TIULINE,0),.TIUCOL,$L(TIUTEXT)))
 D:$G(ON)]""!($G(OFF)]"") CNTRL^VALM10(.TIULINE,.TIUCOL,$L(TIUTEXT),$G(ON),$G(OFF))
 W:'(TIULINE#5)&'+$G(HUSH) "."
 Q
 ;
BLANK(TIULINE) ; blank line
 D SET^VALM10(.TIULINE,$J("",80))
 Q
