IBBAACCT ;OAK/ELZ - PFSS ACCOUNT API ;15-MAR-2005
 ;;2.0;INTEGRATED BILLING;**286**;21-MAR-94
 ;;Per VHA Directive 10-93-142, this routine should not be modified.
 ;
GET(IBBDFN,IBBARFN,IBBEVENT,IBBAPLR,IBBPV1,IBBPV2,IBBPR1,IBBDG1,IBBZCL,IBBDIV,IBBRAIEN,IBBSURG) ;
 ;
 N IBB,IBBIEN,IBBIENS,IBBERR,IBBVERR,IBBSUBTY,IBBVDEF,FDA,OUT,J,J1,X,Y,X1,X2,X3
 I '$G(IBBDFN)!($G(IBBEVENT)="") Q 0
 I $D(IBBPV1)<10 Q 0
 ;
 ;update account record
 S OUT=0
 I IBBARFN'="" D
 .S IBBIEN=IBBARFN
 .I IBBDFN'=$P($G(^IBBAA(375,IBBIEN,0)),U,3) S OUT=1 Q
 .;visit data
 .I $D(IBBPV1)>1 D
 ..I $G(IBBPV1(44))="" S IBBPV1(44)=$G(IBBPV2(8))
 ..I $P($G(^IBBAA(375,IBBIEN,"PV1")),U,44),$G(IBBPV1(44))'=$P($G(^IBBAA(375,IBBIEN,"PV1")),U,44),$P(^IBBAA(375,IBBIEN,0),U,2) D KAC144^IBBAADD(IBBIEN)
 ..S J=0 F  S J=$O(IBBPV1(J)) Q:'J  S $P(^IBBAA(375,IBBIEN,"PV1"),U,J)=IBBPV1(J)
 ..I $G(IBBPV1(50)) S $P(^IBBAA(375,IBBIEN,"PV1"),U,50)=IBBPV1(50)_";;;;OPP"
 .I $D(IBBPV2)>1 D
 ..S J=0 F  S J=$O(IBBPV2(J)) Q:'J  S $P(^IBBAA(375,IBBIEN,"PV2"),U,J)=IBBPV2(J)
 .;procedure
 .I $D(IBBPR1)>1 D
 ..S J=0,X="" F  S J=$O(IBBPR1(J)) Q:'J  I J'=4 S $P(X,U,J)=IBBPR1(J)
 ..S ^IBBAA(375,IBBIEN,"PR1")=X
 ..I $G(IBBPR1(4))'="" S ^IBBAA(375,IBBIEN,11)=IBBPR1(4)
 .;diagnosis
 .;if any dx sent, remove existing dx
 .I $D(IBBDG1)>1,$G(IBBDG1(1,3))=+$G(IBBDG1(1,3)) K ^IBBAA(375,IBBIEN,"DG1") D DX^IBBAACCT(.IBBDG1,IBBIEN)
 .I $G(IBBDG1(1,4)) S ^IBBAA(375,IBBIEN,12)=IBBDG1(1,4)
 .;classification
 .;if any classification sent, remove existing classification
 .I $D(IBBZCL)>1 D
 ..K ^IBBAA(375,IBBIEN,"ZCL")
 ..S (J,J1)=0 F  S J=$O(IBBZCL(J)) Q:'J  S J1=J1+1,X=J1_U_IBBZCL(J,2)_U_IBBZCL(J,3),^IBBAA(375,IBBIEN,"ZCL",J1,0)=X
 ..S ^IBBAA(375,IBBIEN,"ZCL",0)="^375.05A^"_J1_U_J1
 .;miscellaneous
 .I $G(IBBDIV) S ^IBBAA(375,IBBIEN,13)=IBBDIV
 .I $D(IBBSURG)>1 S ^IBBAA(375,IBBIEN,14)=$G(IBBSURG(1))_U_$G(IBBSURG(2))
 .I $G(IBBRAIEN) S ^IBBAA(375,IBBIEN,15)=$G(IBBRAIEN)
 .I $G(IBBSURG(1)) S $P(^IBBAA(375,IBBIEN,"PV1"),U,50)=IBBSURG(1)_";;;;SUR"
 I OUT Q 0
 ;
 ;request account reference number
 I IBBARFN="" D
 .I $G(IBBAPLR)'="" D
 ..S IBBAPLR=$E($TR(IBBAPLR,U,";"),1,25)
 ..I IBBAPLR'[";" S IBBAPLR=";"_IBBAPLR
 .L +^IBBAA(375,0):5
 .Q:'$T
 .S IBBIEN=$P(^IBBAA(375,0),U,3)+1,$P(^IBBAA(375,0),U,3)=IBBIEN
 .L -^IBBAA(375,0)
 .S ^IBBAA(375,IBBIEN,0)=IBBIEN
 .S IBBIENS=IBBIEN_","
 .S IBBERR="IBB(""DIERR"")"
 .S FDA(375,IBBIENS,.02)="G"
 .S FDA(375,IBBIENS,.03)=IBBDFN
 .S FDA(375,IBBIENS,.04)=$G(IBBAPLR)
 .D FILE^DIE("","FDA",IBBERR)
 .Q:$D(IBB("DIERR"))
 .S IBBARFN=IBBIEN
 .;visit data
 .I $D(IBBPV1)>1 D
 ..I $G(IBBPV1(44))="" S IBBPV1(44)=$G(IBBPV2(8))
 ..S J=0,X="" F  S J=$O(IBBPV1(J)) Q:'J  S $P(X,U,J)=IBBPV1(J)
 ..S ^IBBAA(375,IBBIEN,"PV1")=X
 ..I $G(IBBPV1(50)) S $P(^IBBAA(375,IBBIEN,"PV1"),U,50)=IBBPV1(50)_";;;;OPP"
 ..I $G(IBBPV1(3))="FEE BASIS" D
 ...S IBBPV1(44)=$P($G(IBBPV1(44)),".",1)
 ...S $P(^IBBAA(375,IBBIEN,"PV1"),U,3)="",$P(^("PV1"),U,44)=IBBPV1(44)
 ...S ^IBBAA(375,IBBIEN,16)=IBBPV1(3)
 .I $D(IBBPV2)>1 D
 ..S J=0,X="" F  S J=$O(IBBPV2(J)) Q:'J  S $P(X,U,J)=IBBPV2(J)
 ..S ^IBBAA(375,IBBIEN,"PV2")=X
 .;procedure
 .I $D(IBBPR1)>1 D
 ..I $D(IBBPR1(4)) S ^IBBAA(375,IBBIEN,11)=IBBPR1(4)
 ..S J=0,X="" F  S J=$O(IBBPR1(J)) Q:'J  I J'=4 S $P(X,U,J)=IBBPR1(J)
 ..S ^IBBAA(375,IBBIEN,"PR1")=X
 .;diagnosis
 .I $D(IBBDG1)>1 D
 ..I $D(IBBDG1(1,4)) S ^IBBAA(375,IBBIEN,12)=IBBDG1(1,4)
 ..I $G(IBBDG1(1,3))=+$G(IBBDG1(1,3)) D DX^IBBAACCT(.IBBDG1,IBBIEN)
 .;classification
 .I $D(IBBZCL)>1 D
 ..S (J,J1)=0 F  S J=$O(IBBZCL(J)) Q:'J  S J1=J1+1,X=J1_U_IBBZCL(J,2)_U_IBBZCL(J,3),^IBBAA(375,IBBIEN,"ZCL",J1,0)=X
 ..S ^IBBAA(375,IBBIEN,"ZCL",0)="^375.05A^"_J1_U_J1
 .;miscellaneous
 .I $G(IBBDIV) S ^IBBAA(375,IBBIEN,13)=IBBDIV
 .I $D(IBBSURG)>1 S ^IBBAA(375,IBBIEN,14)=$G(IBBSURG(1))_U_$G(IBBSURG(2))
 .I $G(IBBRAIEN) S ^IBBAA(375,IBBIEN,15)=$G(IBBRAIEN)
 .I $G(IBBSURG(1)) S $P(^IBBAA(375,IBBIEN,"PV1"),U,50)=IBBSURG(1)_";;;;SUR"
 ;
 ;exit here if lock failed or FM error (??)
 I 'IBBARFN Q +IBBARFN
 ;
 ;update event history
 I $L(IBBEVENT)=3 D EVENT^IBBAACCT(IBBARFN,IBBEVENT,"R")
 ;
 ;set xref
 S X1=IBBDFN,X2=$G(IBBPV1(3)),X3=$G(IBBPV1(44)) I X3 D
 .I X2'=+X2 S ^IBBAA(375,"AF",X1,X3,X2,IBBARFN)=""
 .I X2=+X2 S ^IBBAA(375,"AC",X1,X3,X2,IBBARFN)=""
 ;
 ;quit if test patient
 I $$TESTPAT^VADPT(IBBDFN) S $P(^IBBAA(375,IBBIEN,0),U,20)=1 Q IBBARFN
 ;
 ;call VDEF
 S IBBVDEF=0,X=IBBEVENT
 S IBBSUBTY=$S(X="A01":"PFAN",X="A03":"PFDE",X="A04":"PFOA",X="A05":"PFPA",X="A08":"PFUPI",X="A11":"PFCAN",X="A13":"PFCDE",X="A38":"PFCPA",1:"")
 I IBBSUBTY'="" S X=$T(QUEUE^VDEFQM) I X'="" S IBBVDEF=$$QUEUE^VDEFQM("ADT^"_IBBEVENT,"SUBTYPE="_IBBSUBTY_"^IEN="_IBBARFN,.IBBVERR,"PFSS OUTBOUND")
 ;
 Q +IBBARFN
 ;
DX(DG1,IEN) ;file diagnosis on subfile #375.04
 N J,IBB,IBBIEN,IBBIENS,IBBERR,FDA
 S J=0 F  S J=$O(DG1(J)) Q:'J  Q:(DG1(J,3)'=+DG1(J,3))  D
 .S IBBIEN(1)=J
 .S IBBIENS="+1,"_IEN_","
 .S IBBERR="IBB(""DIERR"")"
 .S FDA(375.04,IBBIENS,.01)=J
 .S FDA(375.04,IBBIENS,.03)=DG1(J,3)
 .S FDA(375.04,IBBIENS,.06)=$G(DG1(J,6))
 .D UPDATE^DIE("","FDA","IBBIEN",IBBERR)
 Q
 ;
EVENT(IBBARFN,IBBEVENT,IBBREAS,IBBHLMSG) ;update the event history subfile #375.099
 ;
 ;update event history
 N IBB,IBBIEN,IBBIENS,IBBERR,FDA
 Q:'$G(IBBARFN)  Q:$G(IBBEVENT)=""
 S IBBIEN(1)=""
 S IBBIENS="+1,"_IBBARFN_","
 S IBBERR="IBB(""DIERR"")"
 S FDA(375.099,IBBIENS,.01)=$$NOW^XLFDT()
 S FDA(375.099,IBBIENS,.02)=IBBEVENT
 S FDA(375.099,IBBIENS,.03)=$G(IBBREAS)
 S FDA(375.099,IBBIENS,.04)=$G(IBBHLMSG)
 D UPDATE^DIE("","FDA","IBBIEN",IBBERR)
 Q
 ;
EXTNUM(IBBDFN,IBBARFN) ;find external visit number
 N IBBIEN,IBBEXVN,IBBARRY,IBBERR
 S IBBEXVN="",IBBIEN=IBBARFN
 D GETS^DIQ(375,IBBIEN_",",".02;.03","I","IBBARRY","IBBERR")
 I $D(IBBERR("DIERR")) Q IBBEXVN
 I IBBARRY(375,IBBIEN_",",.03,"I")=IBBDFN S IBBEXVN=IBBARRY(375,IBBIEN_",",.02,"I")
 Q IBBEXVN
