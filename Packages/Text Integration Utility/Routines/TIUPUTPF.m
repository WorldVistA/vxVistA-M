TIUPUTPF ; SLC/JER - PRF Look-up Method - ;10/9/05
 ;;1.0;TEXT INTEGRATION UTILITIES;**184**;Jun 20, 1997
 ; COPIED FROM TIUPUTPN AND THEN EDITED
LOOKUP ; Look-up code used by router/filer
 ; Required: TIUSSN, TIUVDT
 ; -- Selected flag action is set in LOOKUP & in GETCHECK^TIUPFFIX;
 ;    Action is used in post-file code to link note.
 ;    Kill it before setting it:
 K ^TMP("TIUPRFUP",$J)
 N DA,DFN,TIU,TIUDAD,TIUEDIT,TIUEDT,TIULDT,TIUXCRP,TIUTYPE
 N TIUNEW,TIUEXIST,TIUDPRM,TIUASACT
 S TIUEXIST=1
 I $S('$D(TIUSSN):1,'$D(TIUVDT):1,$G(TIUSSN)?4N:1,$G(TIUSSN)']"":1,1:0) S Y=-1 G LOOKUPX
 I TIUSSN?3N1P2N1P4N.E S TIUSSN=$TR(TIUSSN,"-/","")
 I TIUSSN["?" S Y=-1 G LOOKUPX
 S TIULOC=+$$ILOC(TIULOC)
 I '$D(^SC(+$G(TIULOC),0)) S Y=-1 G LOOKUPX
 S TIUINST=+$$DIVISION^TIULC1(TIULOC)
 S TIUEDT=$$IDATE^TIULC(TIUVDT),TIULDT=$$FMADD^XLFDT(TIUEDT,1)
 I +TIUEDT'>0 S Y=-1 Q
 S TIUTYPE=$$WHATITLE(TIUTITLE)
 I +TIUTYPE'>0 S Y=-1 Q
 ; -- Abort upload if title is not a PRF title:
 I '$$ISPFTTL^TIUPRFL(+TIUTYPE) S Y=-1 Q
 D DOCPRM^TIULC1(+TIUTYPE,.TIUDPRM)
 I $P($G(^SC(+TIULOC,0)),U,3)="W" D  I 1
 . D MAIN^TIUMOVE(.TIU,.DFN,TIUSSN,TIUEDT,TIULDT,1,"LAST",0,TIULOC)
 E  D MAIN^TIUVSIT(.TIU,.DFN,TIUSSN,TIUEDT,TIULDT,"LAST",0,TIULOC)
 I $S($D(TIU)'>9:1,+$G(DFN)'>0:1,1:0) S Y=-1 G LOOKUPX
 I $P(+$G(TIU("EDT")),".")'=$P($$IDATE^TIULC(TIUVDT),".") S Y=-1 G LOOKUPX
 I '+$$EXIST^TIUEDI3(DFN,+TIUTYPE,TIU("VSTR"),1,DUZ) S TIUEXIST=0 D
 . ; -- If refiling after filing error, get flag assignment^action
 . ;    from user:
 . I $G(PRFILERR) S TIUASACT=$$SELECT^TIUPRF1(+TIUTYPE,DFN)
 . ; -- If not, try for one available action:
 . I '$G(PRFILERR) K ^TMP("TIUPRF",$J) S TIUASACT=$$ONEACT(DFN,+TIUTYPE) K ^TMP("TIUPRF",$J)
 . I TIUASACT S ^TMP("TIUPRFUP",$J)=TIUASACT
 I 'TIUEXIST,'$G(^TMP("TIUPRFUP",$J)) S Y=-1 G LOOKUPX
 S TIUTYP(1)=1_U_TIUTYPE_U_$$PNAME^TIULC1(TIUTYPE)
 S Y=$$GETRECNW^TIUEDI3(DFN,.TIU,TIUTYP(1),.TIUNEW,.TIUDPRM)
 I +Y'>0 G LOOKUPX
 ; If record is not new, has text and can be edited, then replace
 ; existing text
 I +$G(TIUNEW)'>0 D
 . S TIUEDIT=$$CANEDIT(+Y)
 . I +TIUEDIT>0,$D(^TIU(8925,+Y,"TEXT")) D DELTEXT(+Y)
 . I +TIUEDIT'>0 S TIUDAD=+Y,Y=$$MAKEADD
 I +Y'>0 Q
 D STUFREC(Y,+$G(TIUDAD))
 I +$G(TIUDAD) D SENDADD^TIUALRT(+Y)
 K TIUHDR(.01),TIUHDR(.07),TIUHDR(1301)
LOOKUPX Q
 ;
ONEACT(DFN,TIUTYPE) ; If there is exactly one Assigment History Action
 ; available for pat DFN & Title TIUTYPE, return ASSGNIEN^ACTIEN;
 ; else return 0
 ;Count only unlinked, linkable actions
 ;An action is LINKABLE if it is not ENTERED IN ERROR (EIE) and
 ; is not taken prior to an EIE action.
 N TIUDG,TIUASSGN,ONEIEN,RESULT,ARRAYNM
 S RESULT=0,ARRAYNM="^TMP(""TIUPRFH"",$J)"
 S TIUDG=$$GETHTIU^DGPFAPI1(DFN,+TIUTYPE,ARRAYNM)
 I 'TIUDG G ONEACTX
 S TIUASSGN=+$G(@ARRAYNM@("ASSIGNIEN"))
 I $$AVAILACT^TIUPRFL(ARRAYNM,,,.ONEIEN)=1 S RESULT=TIUASSGN_U_ONEIEN
ONEACTX ;
 K ^TMP("TIUPRFH",$J)
 Q RESULT
 ;
ILOC(LOCATION) ; Get pointer to file 44
 N DIC,X,Y
 S DIC=44,DIC(0)="M",X=LOCATION D ^DIC
 Q Y
CANEDIT(DA) ; Check whether or not document is released
 Q $S(+$P($G(^TIU(8925,+DA,0)),U,5)<4:1,1:0)
MAKEADD() ; Create an addendum record
 N DIE,DR,DA,DIC,X,Y,DLAYGO,TIUATYP,TIUFPRIV S TIUFPRIV=1
 S TIUATYP=+$$WHATITLE("ADDENDUM")
 S (DIC,DLAYGO)=8925,DIC(0)="L",X=""""_"`"_TIUATYP_""""
 D ^DIC
 S DA=+Y
 I +DA>0 S DIE=DIC,DR=".04////"_$$DOCCLASS^TIULC1(TIUATYP) D ^DIE
 K TIUHDR(.01)
 Q +DA
STUFREC(DA,PARENT) ; Stuff fixed field data
 N FDA,FDARR,IENS,FLAGS,TIUMSG
 S IENS=""""_DA_",""",FDARR="FDA(8925,"_IENS_")",FLAGS="K"
 I +$G(PARENT)'>0 D
 . S @FDARR@(.02)=$G(DFN),@FDARR@(.03)=$P($G(TIU("VISIT")),U)
 . S @FDARR@(.05)=3
 . S @FDARR@(.07)=$P($G(TIU("EDT")),U)
 . S @FDARR@(.08)=$P($G(TIU("LDT")),U)
 . S @FDARR@(1201)=$$NOW^TIULC
 . S @FDARR@(1205)=$S(+$P($G(TIU("LOC")),U):$P($G(TIU("LOC")),U),1:$P($G(TIU("VLOC")),U))
 . ;S @FDARR@(1211)=$P($G(TIU("VLOC")),U)
 . S @FDARR@(1404)=$P($G(TIU("SVC")),U)
 I +$G(PARENT)>0 D
 . S @FDARR@(.02)=+$P($G(^TIU(8925,+PARENT,0)),U,2)
 . S @FDARR@(.03)=+$P($G(^TIU(8925,+PARENT,0)),U,3),@FDARR@(.05)=3
 . S @FDARR@(.06)=PARENT
 . S @FDARR@(.07)=$P($G(^TIU(8925,+PARENT,0)),U,7)
 . S @FDARR@(.08)=$P($G(^TIU(8925,+PARENT,0)),U,8)
 . S @FDARR@(1205)=$P($G(^TIU(8925,+PARENT,12)),U,5)
 . S @FDARR@(1404)=$P($G(^TIU(8925,+PARENT,14)),U,4)
 . S @FDARR@(1201)=$$NOW^TIULC
 S @FDARR@(1205)=$P($G(TIU("LOC")),U)
 S @FDARR@(1212)=$P($G(TIU("INST")),U)
 S @FDARR@(1301)=$S($G(TIUDDT)]"":$$IDATE^TIULC($G(TIUDDT)),1:"")
 I $S(@FDARR@(1301)'>0:1,$P(@FDARR@(1301),".",2)']"":1,1:0) D
 . S @FDARR@(1301)=$S($P($G(TIU("VSTR")),";",3)="H":$$NOW^XLFDT,1:$G(@FDARR@(.07)))
 S @FDARR@(1303)="U"
 D FILE^DIE(FLAGS,"FDA","TIUMSG") ; File record
 Q
DELTEXT(DA) ; Delete existing text in preparation for replacement
 N DIE,DR,X,Y
 S DIE=8925,DR="2///@" D ^DIE
 Q
WHATYPE(X) ; Identify document type
 ; Receives: X=Document Definition Name
 ;  Returns: Y=Document Definition IFN
 N DIC,Y,TIUFPRIV S TIUFPRIV=1
 S DIC=8925.1,DIC(0)="M"
 S DIC("S")="I $D(^TIU(8925.1,+Y,""HEAD""))!$D(^TIU(8295.1,+Y,""ITEM""))"
 D ^DIC K DIC("S")
WHATYPX Q Y
WHATITLE(X) ; Identify document title
 ; Receives: X=Document Definition Name
 ;  Returns: Y=Document Definition IFN
 N DIC,Y,TIUFPRIV S TIUFPRIV=1
 S DIC=8925.1,DIC(0)="M"
 S DIC("S")="I $P(^TIU(8925.1,+Y,0),U,4)=""DOC"""
 D ^DIC K DIC("S")
WHATITX Q Y
FOLLOWUP(TIUDA) ; Post-filing code for PRF
 N FDA,FDARR,IENS,FLAGS,TIUMSG,TIU,DFN,TIUTEMP
 S IENS=""""_TIUDA_",""",FDARR="FDA(8925,"_IENS_")",FLAGS="K"
 S @FDARR@(1204)=$$WHOSIGNS^TIULC1(TIUDA)
 I +$P($G(^TIU(8925,TIUDA,12)),U,9),'+$P($G(^(12)),U,8) D
 . S @FDARR@(1208)=$$WHOCOSIG^TIULC1(TIUDA)
 D FILE^DIE(FLAGS,"FDA","TIUMSG")
 I +$P($G(^TIU(8925,+TIUDA,12)),U,8),(+$P($G(^TIU(8925,+TIUDA,12)),U,4)'=+$P($G(^(12)),U,8)) D
 . S @FDARR@(1506)=1 D FILE^DIE(FLAGS,"FDA","TIUMSG")
 D RELEASE^TIUT(TIUDA,1)
 D AUDIT^TIUEDI1(TIUDA,0,$$CHKSUM^TIULC("^TIU(8925,"_+TIUDA_",""TEXT"")"))
 I $G(^TMP("TIUPRFUP",$J)) D
 . N TIUDFN S TIUDFN=$P($G(^TIU(8925,TIUDA,0)),U,2)
 . S TIUTEMP=$$LINK^TIUPRF1(TIUDA,$P($G(^TMP("TIUPRFUP",$J)),U),$P($G(^TMP("TIUPRFUP",$J)),U,2),TIUDFN)
 . K ^TMP("TIUPRFUP",$J)
 I '$D(TIU("VSTR")) D
 . N TIUD0,TIUD12,TIUVLOC,TIUHLOC,TIUEDT,TIULDT
 . S TIUD0=$G(^TIU(8925,+TIUDA,0)),TIUD12=$G(^(12))
 . S DFN=+$P(TIUD0,U,2),TIUEDT=+$P(TIUD0,U,7)
 . S TIULDT=$$FMADD^XLFDT(TIUEDT,1),TIUHLOC=+$P(TIUD12,U,5)
 . S TIUVLOC=$S(+$P(TIUD12,U,11):+$P(TIUD12,U,11),1:+TIUHLOC)
 . I $S(+DFN'>0:1,+TIUEDT'>0:1,+TIULDT'>0:1,+TIUVLOC'>0:1,1:0) Q
 . D MAIN^TIUVSIT(.TIU,DFN,"",TIUEDT,TIULDT,"LAST",0,+TIUVLOC)
 Q:'$D(TIU("VSTR"))
 D ENQ^TIUPXAP1 ; Get/file VISIT
 Q
