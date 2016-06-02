VFDCDUZ3 ;DSS/SGM - SILENT ADD NEW USER/CLONE ; 04/21/2014 23:55
 ;;2013.0;;;;Build 2
 ;
 ; cloned from: XUSERBLK
 ; This routine is not to be invoked directly except by ^VFDCDUZ
 ;
 ; ICR#  Type  Description
 ;-----  ----  --------------------------------------------------------
 ; 1519  Sup   ^XUTMDEVQ: NODEV
 ; 2051  Sup   ^DIC: $$FIND1
 ; 2053  Sup   ^DIE: UPDATE
 ;10003  Sup   ^%DT
 ;10060  Sup   Read access to all fields in file 200 using FM calls
 ;10064  Sup   ^XM: NEW
 ;10103  Sup   ^XLFDT: $$FMADD, $$HTFM, $$NOW^XLFDT
 ;             VistA references not supported by any ICR
 ;             --------------------------------------------------------
 ;             Reads of globals: ^XMB(3.7,DA,0), ^XTV(8989.3,1,3)
 ;             Routines:
 ;              Any reference to ^XUSERBLK
 ;              Any reference to ^XUSTERM*
 ;             File 8930.3
 ;              ICR# 3775 - Pri - Direct global read of
 ;                 ^USR(8930.3,"AUC") and ^USR(8930.3,DA,0)
 ;              Use of UPDATE^DIE to add a record
 ;
 ; Documentation Notes at the end of the routine
 ;
 ;---------------------------------------------------------------------
 ;                        ADD NEW PERSON RECORD
 ;---------------------------------------------------------------------
 ; This will not check for duplicates.  It will unequivocally add a new
 ; record to file 200.
 ;
ADD(VFDR,VFDIN,VFDA) ;
 ; RETURN VALUES
 ; If called as an extrinsic function, return file 200 ien or 0^msg
 ;  VFDR - return variable - VFDR = $$ADD
 ; INPUT PARAMETERS
 ;  VFDIN(j) - opt - key_term ^ value for j=1,2,3,4,...
 ;
 Q
 ;
 ;---------------------------------------------------------------------
 ;                            CLONE A USER
 ;---------------------------------------------------------------------
 ; This enhances the VistA grant access by profile option
 ; Routine XUSERBLK modified in the C2 module to look for a flag from
 ;   this routine.  XUSERBLK called any other way other than from this
 ;   routine should continue to work as FOIA VistA intended.
 ;   That flag is S VFDCDUZ=date.time
 ;
CLONE ;
 ; set specific variables that XUSERBLK was expecting
 ; NEW PERSON record must already exist
 ; VFDA(seq#) = key_word ^ value
 ;              TMDT     ^ termination date
 ;              AC       ^ unencrypted access code
 ;              VC       ^ unencrypted verify code
 ;              FAC      ^ FileMan access code
 ;                         For LBL and RUN execute in order received
 ;                         Run LBLs first, then do RUNs
 ;              RUN      ^ argument of a DO command
 ;              LBL      ^ label name in this routine for optional,
 ;                         common vxVistA enhancements
 ;
 N I,J,X,Y,Z,DA,ERR,VFDA,VNEW,VRUN
 M VFDA=VFDAA N VFDAA
 S ERR=0 D  Q:ERR  ; define variables that XUSERBLK submits to Taskman
 . N DIERR,VERR,VFDCDUZ,XUIOP,XMQUIET,XUTERMDT,XUTMP
 . I '$D(ZTQUEUED) N ZTQUEUED S ZTQUEUED=1
 . S XUIOP="",XMQUIET=1
 . S VFDCDUZ=$$NOW^XLFDT ; flag for modification in XUSERBLK
 . ; VFDFR = XUTMP - clone user (name or ien)
 . S Y=$G(VFDFR) I '$L(Y) S ERR=1 Q
 . I Y<1 S Y=$$FIND1^DIC(200,,"QX",Y,"B",,"VERR") S:$D(DIERR) Y=0
 . I Y<1 S ERR=1 Q
 . S X=$P($G(^VA(200,+Y,0)),U) I X="" S ERR=1 Q
 . S XUTMP=+Y,XUTMP(0)=X,VFDFR(0)=X
 . ; VFDTO = DA - file 200 ien of person to recieve clone attributes
 . S DA=+$G(VFDTO) I DA<1 S ERR=1 Q
 . S X=$G(^VA(200,DA,0)),X(1)=$G(^(.1)) I $P(X,U)="" S ERR=1 Q
 . S VFDTO=DA
 . ; set up XUSER(1) = DA ^ name ^ VNEW ^ 'VNEW ^ 1 ; see XUSERBLK
 . ; if no A/C or V/C, then treat New Person as new
 . ;S VNEW=$S($P(X,U,3)="":1,1:$P(X(1),U,2)="")
 . I $P(X,U,3)'="" S VNEW=0
 . E  S VNEW=1
 . S XUSER=1,XUSER(1)=DA_U_$P(X,U)_U_VNEW_U_('VNEW)_"^1"
 . ; this program will handle termination if appropriate, not XUSERBLK
 . S XUTERMDT=""
 . D CLONE^XUSERBLK
 . Q
 ;
 ; process any additional data passed in VFDA()
 ; this routine assumes that A/C and V/C is not to be auto-gen by
 ;   XUSERBLK, so if VNEW, then delete values
 D VFDA
 I VNEW D
 . I '$D(VFDA(2)) S VFDA(2)="@",VFDA(11)="@"
 . E  I '$D(VFDA(11)) S VFDA(11)="@"
 . Q
 D  ; file additional updates to the new person record
 . N DIERR,VFD,VFDER
 . M VFD(200,VFDTO_",")=VFDA
 . D FILE^DIE(,"VFD","VFDER")
 . ; what to do if error encountered??
 . Q
 ; if termination date then schedule it like XUSERBLK
 S Y=$G(VFDA(9.2)) I Y,Y'<DT,'VNEW D TERM(VFDTO,Y)
 ;
 ; run any LBL optionals, then do RUN optionals
 ; if necessary, should add error trap to prevent breaking 
 I $D(VRUN) S Z="VRUN" F  S Z=$Q(@Z) Q:Z=""  S X=@Z D
 . Q:X=""  Q:$T(@X)=""
 . I $QS(Z,1)="L" D
 . . I X="USR" S X="USR(VFDFR,VFDTO)"
 . . Q
 . N Z D @X
 . Q
 Q
 ;
 ;---------------------------------------------------------------------
 ;                SCHEDULE TERMINATION OF A SINGLE USER
 ;---------------------------------------------------------------------
TERM ; schedule termination of single user
 ; code extracted from B4^XUSERBLK
 Q:$G(DA)>.6  S DATE=$G(DATE) S:'DATE DATE=$$FMADD^XLFDT(DT,1)
 N X,RTN,VFD S VFD("ZTDTH")=DATE
 S RTN="TERM1^VFDCDUZ("_DA_")"
 S X=$$NODEV^XUTMDEVQ(RTN,,,.VFD,1)
 Q
 ;
 ;---------------------------------------------------------------------
 ;                       TERMINATE A SINGLE USER
 ;---------------------------------------------------------------------
 ; Clone of CHECK^XUSTERM1 which goes through the entire file 200
 ; Customers may not want to run the option for all users
 ;
TERM1 ;
 Q:$G(DA)<1  Q:'$D(^VA(200,DA,0))
 N I,J,X,Y,Z
 N FDA,XUAAW,XUDT,XUDT30,XUDT540,XUDT90,XUVE
 S XUDT90=$$HTFM^XLFDT($H-90,1),XUDT30=$$HTFM^XLFDT($H-30,1)
 S XUDT540=$$HTFM^XLFDT($H-540,1) ;*p332
 S XUAAW=+$P($G(^XTV(8989.3,1,3)),U,4) ;Academic Waiver
 S XUDA=DA,XUVE=0
 S XUJ=^VA(200,XUDA,0),XUDT=$P(XUJ,U,11)
 I $P(XUJ,U,3)]"",$L(XUDT),(XUDT'>DT) D
 . ; XU variables new'd since GET^XUSTERM leaves them defined
 . ;XUGRP=mail group, XUKEY=keys, XUSUR=mail surrogates, XUJ=# baskets
 . ;XUK=# mail msg, XUIN=# in-basket msg
 . ; GET+7^XUSTERM modified as error in VA code 
 . N X,Y,DA
 . N %,I,XUEMP,XUGRP,XUI,XUIN,XUK,XUKEY,XUNAM,XUNUM,XUSUR,XUTX1,XUTX2
 . D
 . . N XUJ D GET^XUSTERM K ^DISV(XUDA) Q
 . Q:XUEMP  ; XUEMP=any data to remove
 . ;
 . N ERR,XUACT
 . D ACT^XUSTERM S XUJ=^VA(200,XUDA,0) ;Get new copy of zero node
 . Q
 I $P(XUJ,U,3)]"",'$P(XUJ,U,8),$$NOSIGNON^XUSTERM1 D
 . D DISUSER^XUSTERM1(XUDA) Q
 I $P(XUJ,U,7) D AUSER^XUSTERM1(XUDA) ;*p332
END K XUEMP,XUDA,XUI,XUJ,XUK,XUACT,XUKEY,XUGRP,XUSUR,XUNAM,XUF,XUDT,XUIN,XUVE,X,DIC,XUDB,XUDC,XUDP
 Q
 ;
 ;=====================================================================
 ;                         PRIVATE SUBOUTINES
 ;=====================================================================
VFDA ; pre-process passed in values [VFDA()] to be ready to FILE^DIE
 ;;AC^FAC^TMDT^VC^LBL^RUN
 ;;2^3^9.2^11
 N I,J,X,Y,Z,FLD,LIST,TMP,VAL,VAR
 K VRUN
 S X=$P($T(VFDA+1),";",3),Z=$P($T(VFDA+2),";",3)
 F I=1:1:$L(X,U) S LIST($P(X,U,I))=$P(Z,U,I)
 S I="" F  S I=$O(VFDA(I)) Q:'I  D
 . S VAR=$P(VFDA(I),U),VAL=$P(VFDA(I),U,2,3)
 . Q:VAR=""  Q:'$D(LIST(VAR))
 . I VAR="RUN"!(VAR="LBL") D:VAL'=""  Q
 . . I VAR="RUN",VAL'[U S VAL=U_VAL
 . . S Z=$E(VAR),J=1+$O(VRUN(Z," "),-1),VRUN(Z,J)=VAL
 . . Q
 . S Y=VAL,FLD=LIST(VAR)
 . I "AC^VC"[VAR S Y=$$EN^XUSHSH(VAL)
 . I VAR="TMDT" N %DT S Y="",X=VAL D ^%DT S:Y>0 Y=$P(Y,".")
 . I Y>0 S TMP(FLD)=$P(Y,".")
 . Q
 K VFDA M VFDA=TMP
 Q
 ;
USR(FR,TO) ; copy usr class membership
 N I,J,X,Y,Z,DIERR,IEN,VFDER,VFDIEN,VFDUSR,TMP
 Q:$G(^VA(200,+$G(FR),0))=""  Q:$G(^VA(200,+$G(TO),0))=""
 S (I,IEN)=0 F  S I=$O(^USR(8930.3,"B",FR,I)) Q:'I  D
 . S X=^USR(8930.3,I,0) F J=1:1:4 S X(J)=$P(X,U,J)
 . Q:'X(2)
 . Q:$O(^USR(8930.3,"AUC",X(2),TO,0))  ; TO already a member
 . ; Q:X(4)  ;  FR has an expiration date
 . S IEN=IEN+1,Z="+"_IEN_",",VFDIEN(IEN)=""
 . S VFDUSR(8930.3,Z,.01)=TO
 . S VFDUSR(8930.3,Z,.02)=X(2)
 . S VFDUSR(8930.3,Z,.03)=DT
 . Q
 I $D(VFDUSR) D UPDATE^DIE(,"VFDUSR","VFDIEN","VFDER")
 ; what to do if DIERR?
 Q
 ;
 ;-----------------------------------------------------------------
 ;                         Documentation Notes
 ;---------------------------------------------------------------------
 ; 1. Original code allowed for the user-interactive collection of new
 ;    users (more than one).  It did not allow for silent creation of
 ;    new users.
 ; 2. These APIs will handle only a single user at a time.  It is the
 ;    calling applications responsibility to loop for the creation of
 ;    multiple New Person records.
 ; 3. If termination date passed, then queue automatic termination
 ;       S XUZT("ZTDTH")=XUTERMDT
 ;       S X=$$NODEV^XUTMDEVQ("CHECK^XUSTERM1",,,.XUZT,1)
 ; 4. Since this is one record at a time, it will not be queued but
 ;    will run in real time.  The VistA code set the variables as
 ;    described below up through XUIOP.  Then if queued, queued to
 ;    S VAR="XUIOP;XUTMP;XUTERMDT;XUSER;XUSER("
 ;    S XUZT("ZTDTH")=$H
 ;    S X=$$NODEV^XUTMDEVQ("CLONE^XUSERBLK",,VAR,.XUZT,1)
 ; 3. Key local variables in the VistA Grant Access By Profile Option
 ;     ^TMP($J)
 ;     XUTERMDT = optional termination date for entry to be created
 ;     XUTMP    = ien of New Person to be cloned
 ;     XUTMP(0) = name of New Person to be cloned
 ;     XUY      = $$ADD^XUSERNEW - ien^.01_name^(1/0) indicating new
 ;                a. if existing user is selected
 ;                   1) if not to be included then XUY=-1
 ;                   2) Clear out KEYS, FILES, SECONDARY MENUS first
 ;                      then $P(XUY,U,4)=1
 ;                b. Clone PERSON CLASS, then $P(XUY,U,5)=1
 ;     XUSER(seq#) = XUY [ien^name^new^clear out^clone person class]
 ;     XUSER       = total number of names
 ;     XMQUIET     = 1
 ;     XUIOP       = ION_";"_IOST_";"_IOM_";"_IOSL
 ; 4. Dequeue
 ; F XU1=1:1:XUSER S %=XUSER(XU1),DA=+%,XUNEW=$P(%,U,3),XUPURGE=$P(%,U,4) D C2,UPDATE("ORD",DA)
 ;    D C2 ; modify, access/verify codes, no letters
 ;      D BLDFDA (sets ^TMP($J))
 ;      D PRSNCL(USERIEN) ; create 200.05 multiple
 ;      S $P(^VA(200,DA,0),U,11)=XUTERMDT
 ;      S XUNEW if I $P(^VA(200,DA,0),U,3)']"" I $P($G(^VA(200,DA,.1)),U,2)']""
 ;                 D ACODE S @XFDA@(200,DA_",",2)=XUH
 ;                 D VCODE S @XFDA@(200,DA_",",11)=XUH2
 ;      D UPDATE^DIE("",XFDA,XIEN,"XERR") K @XFDA
 ;      I $D(^XMB(3.7,DA,0))[0 S Y=DA K XMZ D NEW^XM K XMDT,XMM,XMZ
 ;    D UPDATE("ORD",DA) ; updates effective date CPRS TAB^200.010113POI
