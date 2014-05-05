FSCRPCUD ;SLC/STAFF-NOIS RPC User Defaults ;4/16/98  15:18
 ;;1.1;NOIS;;Sep 06, 1998
 ;
USER(IN,OUT) ; from FSCRPX (RPCUserDefaults)
 N USER
 S USER=+$G(^TMP("FSCRPC",$J,"INPUT",1))
 I 'USER Q
 S ^TMP("FSCRPC",$J,"OUTPUT",1)=USER_U_$P($G(^VA(200,+USER,0)),U)
 Q
 ;
DUZ(IN,OUT) ; from FSCRPX (RPCCurrentUser)
 N PFUNC,PMOD,PPRI,PSPEC,PTASK,SPEC,PREF,NAME,PHONE
 S (PFUNC,PMOD,PPRI,PSPEC,PTASK)=0
 S SPEC=$$ACCESS^FSCU(DUZ,"SPEC")
 S PPRI=+$O(^FSC("PRI","B","ROUTINE",0))
 S NAME=$P($G(^VA(200,+DUZ,0)),U),PHONE=$P($G(^(.13)),U,2)
 I SPEC D
 .S PREF=$$FUNC^FSCEUD(DUZ) I $P(PREF,U)'="I" D
 ..I $L($P(PREF,U,2)) S PFUNC=+$O(^FSC("FUNC","B",$P(PREF,U,2),0))
 .S PREF=$$TASK^FSCEUD(DUZ) I $P(PREF,U)'="I" D
 ..I $L($P(PREF,U,2)) S PTASK=+$O(^FSC("TASK","B",$P(PREF,U,2),0))
 .S PREF=$$PRIORITY^FSCEUD(DUZ) I $P(PREF,U)'="I" D
 ..I $L($P(PREF,U,2)) S PPRI=+$O(^FSC("PRI","B",$P(PREF,U,2),0))
 .S PREF=$$MOD^FSCEUD(DUZ) I $P(PREF,U)'="I" D
 ..I $L($P(PREF,U,2)) S PMOD=+$O(^FSC("MOD","B",$P(PREF,U,2),0))
 .S PREF=$$SPEC^FSCEUD(DUZ) I $P(PREF,U)'="I" D
 ..S PSPEC=DUZ
 S ^TMP("FSCRPC",$J,"OUTPUT",1)=DUZ_U_NAME_U_SPEC_U_PFUNC_U_PPRI_U_PSPEC_U_PTASK_U_PMOD_U_PHONE
 Q
 ;
DUZTEMP(IN,OUT) ; from FSCRPX (RPCUserTempFiles)
 N CNT,NUM,ZERO
 S CNT=0
 S NUM=0 F  S NUM=$O(^FSC("LIST","C",DUZ,NUM)) Q:NUM<1  S ZERO=$G(^FSC("LIST",NUM,0)) I $L(ZERO) D
 .I $P(ZERO,U,3)'="S" Q
 .S CNT=CNT+1
 .S ^TMP("FSCRPC",$J,"OUTPUT",CNT)=NUM_U_ZERO
 Q
 ;
NEWTEMP(IN,OUT) ; from FSCRPX (RPCNewTempList)
 N CNT,DA,DIC,DIE,DLAYGO,DR,LISTNAME,NEWNAME,X,Y K DIC,DLAYGO
 S LISTNAME="TEMP-"_$P($P(^VA(200,DUZ,0),U),",")
 S LISTNAME=$E(LISTNAME,1,28)
 S NEWNAME=LISTNAME
 S CNT=0
 F  Q:'$O(^FSC("LIST","B",NEWNAME,0))  S CNT=CNT+1,NEWNAME=LISTNAME_CNT
 S (DIC,DLAYGO)=7107.1,DIC(0)="XL",X=NEWNAME
 D ^DIC K DIC,DLAYGO
 I '$P(Y,U,3) Q
 S DA=+Y,DIE=7107.1,DR="1///`"_DUZ_";2///S"
 D ^DIE
 S ^TMP("FSCRPC",$J,"OUTPUT",1)=DA_U_^FSC("LIST",DA,0)
 Q
 ;
CONTACT(IN,OUT) ; from FSCRPX (RPCContact)
 N CONTACT,PHONE,SITE
 S SITE=+$G(^TMP("FSCRPC",$J,"INPUT",1))
 S CONTACT=+$P($G(^FSC("SITE",SITE,0)),U,6)
 S PHONE=$$PH(SITE,CONTACT)
 I 'CONTACT,'$L(PHONE) Q
 S ^TMP("FSCRPC",$J,"OUTPUT",1)=CONTACT_U_PHONE
 Q
 ;
PHONE(IN,OUT) ; from FSCRPX (RPCPhone)
 N PHONE,SITE,USER
 S SITE=$P($G(^TMP("FSCRPC",$J,"INPUT",1)),U),USER=$P($G(^(1)),U,2)
 S PHONE=$$PH(SITE,USER)
 S ^TMP("FSCRPC",$J,"OUTPUT",1)=PHONE
 Q
 ;
PH(SITE,USER) ; $$(site,user) -> phone #
 N PHONE S PHONE=""
 I USER S PHONE=$P($G(^VA(200,USER,.13)),U,2)
 I $L(PHONE) Q PHONE
 I 'SITE Q ""
 S PHONE=$P($G(^FSC("SITE",SITE,0)),U,7)
 I $L(PHONE) Q PHONE
 Q $P($G(^FSC("SITE",SITE,0)),U,8)
