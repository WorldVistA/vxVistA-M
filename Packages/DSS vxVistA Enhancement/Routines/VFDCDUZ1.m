VFDCDUZ1 ;DSS/SGM - COMMON NEW PERSON FILE RPCS ;06/04/2014 16:35
 ;;2013.0;DSS,INC VXVISTA OPEN SOURCE;;07 Feb 2014;Build 2
 ;Copyright 1995-2014,Document Storage Systems Inc. All Rights Reserved
 ;
 ;DO NOT INVOKE THIS ROUTINE DIRECTLY, SEE VFDCDUZ ROUTINE
 ; ICR#  SUPPORTED DESCRIPTION
 ;-----  --------------------------------------------------------------
 ; 1625  $$GET^XUA4A72
 ; 2056  ^DIQ: $$GET1, GETS
 ; 2171  $$NS^XUAF4
 ; 2263  $$GET^XPAR
 ; 2343  ^XUSER: $$ACTIVE, $$PROVIDER
 ; 2533  $$DIV4^XUSER {controlled subscription - not enrolled}
 ; 3065  $$NAMEFMT^XLFNAME
 ;10060  All fields in file 200 can be read with FM calls
 ;10076  Global read of ^XUSEC(key,duz)
 ;10103  $$FMTE^XLFDT
 ;10104  $$UP^XLFSTR
 ;10112  $$SITE^VASITE
 ;
 ;---------------------------------------------------------------------
ACT() ; Validate that DUZ is an active user
 N I,J,X,Y,Z,VFD,VFDRET,VFDX
 S X=$$CK($G(XDUZ)) I X<0 Q X
 S X=$$ACTIVE^XUSER(XDUZ) I X="" Q $$ERR(3)
 I X=0 Q $$ERR(4)
 I X="0^DISUSER" Q $$ERR(5)
 I +X=0 S Y=X Q $$ERR(6)
 I '$D(VFDSCR) Q XDUZ
 ; process screen
 ; VFDSCR(n) = term ^ val [^val]
 ;   KEY ^ security key name
 ;  PARM ^ parameter name ^ parameter instance
 ;     M ^ return message ^ executable M code that sets $T
 ; Once a check has failed, do not process remaining array entries
 S Z="" F  S Z=$O(VFDSCR(Z)) Q:Z=""  S Y=VFDSCR(Z) D  Q:$D(VFDRET)
 . N Z,TERM,VFDP2,VFDP3
 . S TERM=$P(Y,U),VFDP2=$P(Y,U,2),VFDP3=$P(Y,U,3,99)
 . I TERM="KEY" S:'$D(^XUSEC(VFDP2,XDUZ)) VFDRET=$$ERR(7) Q
 . I TERM="PARM" D  Q
 . . S X="USR~"_VFDP2_"~"_VFDP3,X=$$GET1^VFDCXPR(,X,1)
 . . I +X=-1 S VFDRET=X
 . . Q
 . I TERM'="M" S VFDRET=$$ERR(12) Q
 . X VFDP3 E  S VFDRET=$S(VFDP2'="":VFDP2,1:$$ERR(22))
 . Q
 Q $S($D(VFDRET):VFDRET,1:XDUZ)
 ;
 ;---------------------------------------------------------------------
CK(XDUZ) ; basic check for valid DUZ
 I $G(XDUZ)="" Q $$ERR(1)
 I XDUZ'=+XDUZ Q $$ERR(2)
 I XDUZ<.5 Q $$ERR(2)
 N X,DIERR,VFDER S X=$$GET1^DIQ(200,XDUZ_",",.01,"I",,"VFDER")
 I X=""!$D(DIERR) Q $$ERR(3)
 Q 1
 ;
 ;---------------------------------------------------------------------
DIV() ; Return default division for user
 N A,I,X,Y,VFD,VFDCX
 S XDUZ=$G(XDUZ) S:XDUZ="" XDUZ=DUZ
 S X=$$DIV4^XUSER(.VFDCX,XDUZ)
 I 'X,'$G(SITE) Q $$ERR(8)
 I 'X Q $$SITE^VASITE
 S (A,I,X)=0
 F  S I=$O(VFDCX(I)) Q:'I  Q:VFDCX(I)  S A=A+1 S:VFDCX(I)="" X=I
 S Y=$S(I:I,A=1&X:X,1:0) I Y Q Y_U_$$NS^XUAF4(Y)
 I $G(SITE) Q $$SITE^VASITE
 Q $$ERR(9)
 ;
 ;---------------------------------------------------------------------
FIND1() ;
 ; return ien if and only if one match only
 N A,B,L,N,SCR
 S SCR="I 1"
 I $L(VFDTYP) S SCR="I $P($G(^VA(200,A,21600,B,0)),U,5)=VFDTYP"
 S SCR=SCR_" S N=N+1,N(N)=A"
 S L="",N=0
 F  S L=$O(^VA(200,"AVFD",L)) Q:'L!(N>1)  I L=VFDLOC!'$L(VFDLOC) D
 . S A="" F  S A=$O(^VA(200,"AVFD",L,VFDVAL,A)) Q:'A!(N>1)  D
 . . S B="" F  S B=$O(^VA(200,"AVFD",L,VFDVAL,A,B)) Q:'B!(N>1)  X SCR
 . . Q
 . Q
 Q $S(N=1:N(1),1:"")
 ;
 ;---------------------------------------------------------------------
ID ; return a list of IDs
 ;NOTES:
 ;1. FLAGS(file_200 field#) = mnemonic
 ;2. FUN=1 if ID^VFDCDUZ called as extrinsic function
 ;
 N I,J,X,Y,Z,FLDS,IENS,TMP,VFD
 S:$G(XDUZ)="" XDUZ=DUZ
 S X=$$CK^VFDCDUZ(XDUZ) I X<1 S TMP=X G IDOUT
 D IDFLAGS I FLDS="" S TMP=$$ERR(15) G IDOUT
 S IENS=XDUZ_","
 D GETS("VFD",200,FLDS,IENS) I +$G(VFD)=-1 S TMP=VFD G IDOUT
 ; move 200 data into temp array
 D IDTEMP
 ;
IDOUT ; now construct return array
 ; expects $D(VFDC)=0 at this point
 I +$G(TMP)=-1 S:FUN VFDC=TMP S:'FUN VFDC(1)=TMP Q
 I '$O(TMP(0)) S Z=$$ERR(16) S:FUN VFDC=Z S:'FUN VFDC(1)=Z Q
 S:FUN VFDC="" S (I,J)=0 F  S I=$O(TMP(I)) Q:'I  S X=TMP(I) D
 . I 'FUN S J=J+1,VFDC(J)=X
 . E  S VFDC=VFDC_X_";"
 . Q
 Q
 ;
IDFLAGS ;
 ; sets up FLAGS(field#)=mnemonic
 ; creates FLDS string
 N I,J,X,Y,Z
 S FLAGS=$G(FLAGS) S:FLAGS="" FLAGS="ADNSTVv"
 I FLAGS["A",FLAGS["a" S FLAGS=$TR(FLAGS,"a")
 ; map flags to field numbers
 F I=1:1:$L(FLAGS) S X=$E(FLAGS,I) D
 . I "Aa"[X S FLAGS(21600)="OAI"
 . I X="D" S FLAGS(53.2)="DEA"
 . I X="N" S FLAGS(41.99)="NPI"
 . I X="S" S FLAGS(9)="SSN"
 . I X="T" S FLAGS(53.92)="TAX"
 . I X="V" S FLAGS(9000)="VPID"
 . I X="v" S FLAGS(53.3)="VA"
 . Q
 S I=0 F  S I=$O(FLAGS(I)) Q:'I  K:'$$FLDCK(I) FLAGS(I)
 S FLDS="",I=0
 F  S I=$O(FLAGS(I)) Q:'I  S FLDS=FLDS_I_$S(I=21600:"*",1:"")_";"
 Q
 ;
IDTEMP ; move data from $GETS to sequential TMP(j) array
 ; if problems TMP=-1^message else $G(TMP)=""
 N I,J,X,Y,Z,TMPZ
 ; J = incrementor for return array
 S (I,J)=0
 F  S I=$O(VFD(200,IENS,I)) Q:'I  S X=VFD(200,IENS,I,"E") D
 . I $D(FLAGS(I)) S J=J+1,TMP(J)=FLAGS(I)_U_X
 . Q
 Q:'$D(FLAGS(21600))
 S (I,Y)=0 F  S I=$O(VFD(200.0216,I)) Q:I=""  D
 . ; get all alternate IDs.  Original code did not filter on expiration
 . ; date.  Should it and would it be backwardly compatible?
 . ; pieces 5-8 added 12/26/2013
 . K Z M Z=VFD(200.0216,I)
 . Q:Z(.02,"E")=""  S Y=Y+1
 . S X="OAI"_U_Z(.02,"E")_U_Z(.01,"E")_U_Z(.04,"E")
 . I FLAGS["Z" F J=.03,.05,.06,.07 S X=X_U_Z(J,"I")_";"_Z(J,"E")
 . S TMPZ(Y)=X
 . Q
 Q:'Y
 I FLAGS["a",Y>1 D  ; asked for default value only
  .; if any problems, kill off all alt ids retrieved
 . K Z S (I)=0
 . F  S I=$O(TMPZ(I)) Q:'I  S:$P(TMPZ(I),U,4)=1 Z(I)=TMPZ(I)
 . I '$D(Z) K TMPZ S TMP=$$ERR(21) Q
 . S I=$O(Z(0)) I +$O(Z(I)) K TMPZ S TMP=$$ERR(21) Q
 . K TMPZ S I=$O(Z(I)) S TMPZ(I)=Z(I)
 . Q
 S I=0 F  S I=$O(TMPZ(I)) Q:'I  S J=J+1,TMP(J)=TMPZ(I)
 Q
 ;
 ;---------------------------------------------------------------------
ID1 ;
 ; uses Parameter VFDP RX ALTERNATE ID TYPES
 ; because of way this is called, must use @VFDRSLT@()
 N I,J,X,Y,Z,TYP,VFDIX,VFDX
 I $G(VFDRSLT)="" S VFDRSLT="VFDRSLT"
 I $G(VFDDUZ)<.5 S @VFDRSLT@(1)=$$ERR(18) Q
 D ID^VFDCDUZ(.VFDX,VFDDUZ,"ADNZ")
 I +$G(VFDX)=-1 D IDSET(VFDX) Q
 ;12/26/2013 - backward compatibility - add DPS and NPI to parameter
 ;  values if necessary.  Hardcoded check for DPS and NPI should be
 ;  deprecated as all new installations should include DPS and NPI in
 ;  the parameter if it is desired.
 S TYP=$$GET^XPAR("SYS","VFDP RX ALTERNATE ID TYPES")
 F I=1:1:$L(TYP,";") S X=$P(TYP,";",I) S:$L(X) TYP(X)=""
 S:'$D(TYP("DPS")) TYP("DPS")=""
 S:'$D(TYP("NPI")) TYP("NPI")=""
 ;
 ; get DEA and NPI from VA fields first
 S I=0 F  S I=$O(VFDX(I)) Q:'I  S VAL=VFDX(I) D
 . I $P(VAL,U)="DEA" D IDSET(VAL) K VFDX(I) S VFDIX("DEA")=I
 . I $P(VAL,U)="NPI" D IDSET(VAL) K VFDX(I) S VFDIX("NPI")=I
 . Q
 ;
 ; get additional IDs from Alternate ID
 S I=0 F  S I=$O(VFDX(I)) Q:'I  S VAL=VFDX(I) I $P(VAL,U)="OAI" D
 . S Y=$P(VAL,U,5) I Y,Y<DT Q  ;              expired
 . ; check for LIC types
 . S X=+$P(VAL,U,8) I X D  Q
 . . S Z=$G(^VFD(21613.1,X,0)) Q:'$G(^(1))  ; exclude if not RX PRINT
 . . S X=$P(Z,U,2) S:'$L(X) X=$P(Z,U) ;       abbreviation or name
 . . ;                            LIC^ID^license type^expiration date
 . . S VAL="LIC"_U_$P(VAL,U,2)_U_X_U_$P($P(VAL,U,3),";",2)
 . . D IDSET(VAL)
 . . Q
 . ; check for parameterized IDs
 . S VAL=$P($P(VAL,U,6),";",2)_U_$P(VAL,U,2,4)
 . S X=$P($P(VAL,U)," ") Q:X=""  Q:'$D(TYP(X))
 . I $E(X,1,3)'="NPI" D IDSET(VAL) Q
 . ; if no VA NPI field value, get first valid Alt ID NPI
 . S $P(VAL,U)="NPI"
 . S Y=$G(VFDIX("NPI")) I Y,$P(VFDX(Y),U,2)'="" Q
 . I $P(VAL,U,2)'="" S VFDIX("NPI")=I D IDSET(VAL)
 . Q
 Q
 ;
IDSET(VAL) N L S L=1+$O(@VFDRSLT@(" "),-1),@VFDRSLT@(L)=VAL Q
 ;
 ;---------------------------------------------------------------------
LIST ; Return a list of active users only for a lookup value
 N I,X,Y,Z,VFD,VFDCNT,VFDCX,VFDLIST,VFDRET,VFDSCR,VFDX,ERR,INPUT
 S VFDRET=$NA(^TMP("VFDC",$J)) K @VFDRET
 S VFDLIST=$NA(^TMP("VFDCDUZ",$J)) K @VFDLIST
 I $G(VAL)="" S @VFDRET@(1)=$$ERR(10) G LOUT
 S Z=0 I $D(SCR) D  I $D(ERR) S @VFDRET@(1)=ERR G LOUT
 .S X=$G(SCR) I X'="" D OK Q
 .S I="" F  S I=$O(SCR(I)) Q:I=""  S X=SCR(I) D OK
 .Q
 S INPUT(1)="FILE^200"
 S INPUT(2)="FIELDS^.01;20.2;20.3;1;8;29"
 S INPUT(3)="VAL^"_VAL
 S INPUT(4)="SCREEN^I $$ACT^VFDCDUZ(,+Y,.VFDSCR,1)>0"
 D FIND^VFDCFM05(.VFDLIST,.INPUT)
 S (VFD,VFDCNT)=0
 I $D(@VFDLIST) F  S VFD=$O(@VFDLIST@(VFD)) Q:'VFD  D
 .S X=$G(@VFDLIST@(VFD,0)) Q:+X'>0  S X=$P(X,U,2),X=$$NAMEFMT^XLFNAME(X)
 .S Y=@VFDLIST@(VFD,0),Z=$P(Y,U,1,4),$P(Z,U,5)=X_U_$P(Y,U,5,7)
 .S VFDX=Z
 .S X=$$DIV^VFDCDUZ(,+VFDX,1,1) I X>0 S $P(VFDX,U,9)=$P(X,U,2)
 .S VFDCNT=VFDCNT+1,@VFDRET@(VFDCNT,0)=VFDX
 .Q
 I '$D(@VFDRET) S @VFDRET@(1)=$$ERR(11)
LOUT S VFDC=VFDRET K @VFDLIST
 Q
 ;
 ;---------------------------------------------------------------------
PER() ; Return a user's current active person classification for PCE
 N X
 S X=$$CK(USER) I X<1 Q X
 S:'$G(DATE) DATE=DT
 S X=$$GET^XUA4A72(USER,DATE)
 I X<1 S DATE=$$FMTE^XLFDT(DATE),X=$$ERR(13)
 Q X
 ;
 ;---------------------------------------------------------------------
PROV() ; Determine is active cprs provider
 N X,Y,Z,ACC,KEY,VISITOR
 S X=$$CK(XDUZ) I X<1 Q X
 S RDV=+$G(RDV),RDV=(RDV'=0)
 S KEY=$D(^XUSEC("XUORES",XDUZ))
 S ACC=($P(^VA(200,XDUZ,0),U,3)'="")
 S VISITOR=$D(^VA(200,"BB","VISITOR",XDUZ))
 S Y=$$PROVIDER^XUSER(XDUZ,RDV)
 I ACC,Y=1 Q 3
 I Y["0^TERMINATED" Q $$ERR(6)
 I KEY Q 2
 I VISITOR Q RDV=1
 Q $$ERR(14)
 ;
 ;=======================  PRIVATE SUBROUTINES  =======================
 ;
ERR(A) ; return error message
 S:A=1 A="No user DUZ value received"
 S:A=2 A="Invalid user DUZ value received: "_XDUZ
 S:A=3 A="NEW PERSON record "_XDUZ_" does not exist"
 S:A=4 A="User cannot sign-on"
 S:A=5 A="User cannot sign on, Disuser set"
 S:A=6 A="User terminated on "_$$FMTE^XLFDT($P(Y,U,3))
 S:A=7 A="User does not own security key "_Y
 S:A=8 A="User has no divisions defined"
 S:A=9 A="User has division(s), none marked as default"
 S:A=10 A="No lookup value received"
 S:A=11 A="No matches found"
 S:A=12 A="Invalid screen type received"
 S:A=13 A="Person does not have an active Person Class for "_DATE
 S:A=14 A="User is not a provider"
 S:A=15 A="Either invalid flags received, or file 200 fields do not exist"
 S:A=16 A="No data found for this record"
 S:A=17 A="More than one alternate ID found with none indicated as default"
 S:A=18 A="Missing or invalid NEW PERSON IEN"
 S:A=19 A="Invalid input parameter(s) received"
 S:A=20 A="Unexpected problems encountered trying to retrieve data"
 S:A=21 A="Unable to determine default Alternate ID"
 S:A=22 A="Input screen failed"
 Q "-1^"_$G(A)
 ;
FLDCK(F) ; validate field number in file 200 exists
 N I,J,X,Y,Z Q $$VFIELD^VFDCFM(,200,F,1)>0
 ;
GETS(VRET,VFL,VFLD,VIENS) ;
 ;  VRET - return array passed by named reference
 ;         I $G(VRET)="" S VRET="VRET"
 ;         If problems, VRET = -1^message
 ;   VFL - req - dd#
 ;  VFLD - req - valid fields string value
 ; VIENS - req - valid FM DBS IENS value
 ;
 N I,J,X,Y,Z,DIERR,FLD,FLDS,IENS,VFDER
 S:$G(VRET)="" VRET="VRET"
 I $G(VFL)'>0!($G(VFLD)'>0)!($G(VIENS)'>0) S @(@VRET_"=$$ERR(19)") Q
 D GETS^DIQ(VFL,VIENS,VFLD,"IE","VFD","VFDER")
 I $D(DIERR) D
 . K @VRET S X="-1^"_$$MSG^VFDCFM("VE",,,,"VFDER"),@(@VRET_"=X")
 . Q
 Q
 ;
OK ;  validate SCR from LIST
 S Y=$P(X,U)
 I Y?.E1L.E S Y=$$UP^XLFSTR(Y)
 I Y="KEY"!(Y="M")!(Y="PARM") S Z=1+Z,VFDSCR(Z)=X
 E  I '$D(ERR) S ERR=$$ERR(12)
 Q
