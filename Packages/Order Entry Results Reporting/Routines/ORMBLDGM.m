ORMBLDGM ;SLC/MKB-Build outgoing GMR* ORM msgs ;11/18/08  11:24
 ;;3.0;ORDER ENTRY/RESULTS REPORTING;**26,68,97,190,195,280**;Dec 17, 1997;Build 85
HL7DATE(DATE) ; -- FM -> HL7 format
 Q $$FMTHL7^XLFDT(DATE)  ;**97
 ;
PTR(NAME) ; -- Returns ptr value of prompt in Dialog file
 Q $O(^ORD(101.41,"AB",$E("OR GTX "_NAME,1,63),0))
 ;
CSLT ; -- Segments for new Consult order
 N OI,WP,URG,CATG,PLACE,ATTN,DIAG,CODE,QT,I,J,USID,CTYPE,RSERV,Z
 N EARLY,X,Y ;WAT/280
 S OI=$G(ORDIALOG($$PTR("ORDERABLE ITEM"),1))
 S CTYPE=$G(ORDIALOG($$PTR("FREE TEXT OI"),1))
 S RSERV=$G(ORDIALOG($$PTR("REQUEST SERVICE"),1))
 S WP=$$PTR("WORD PROCESSING 1"),URG=+$G(ORDIALOG($$PTR("URGENCY"),1))
 S CATG=$G(ORDIALOG($$PTR("CATEGORY"),1))
 S PLACE=$G(ORDIALOG($$PTR("PLACE OF CONSULTATION"),1))
 S ATTN=$G(ORDIALOG($$PTR("PROVIDER"),1))
 S DIAG=$G(ORDIALOG($$PTR("FREE TEXT"),1))
 S CODE=$G(ORDIALOG($$PTR("CODE"),1))
 S X=$G(ORDIALOG($$PTR("EARLIEST DATE"),1)),%DT="TX" D ^%DT S:Y>0 EARLY=$$HL7DATE(Y) ;WAT/280
CS1 S QT="^^^"_$G(EARLY)_"^^"_$P($G(^ORD(101.42,+URG,0)),U,2),$P(ORMSG(4),"|",8)=QT ;WAT/280
 S $P(ORMSG(3),"|",3)=CATG S:PLACE="C" PLACE="OC"
 S USID=$$USID^ORMBLD(OI) ;S:$L(CTYPE) $P(USID,U,5)=CTYPE
 S ORMSG(5)="OBR||||"_USID_"||||||||||||||"_PLACE_"|"_ATTN,Z=5
 ; Create DG1 & ZCL segment(s) for Billing Awareness (BA) project
 D DG1^ORWDBA3($G(IFN),"Z",5)
 I RSERV'>0,$P(USID,U,6)="99CON" S RSERV=+$P(USID,U,4)
 S:RSERV Z=Z+1,ORMSG(Z)="ZSV|^^^"_+RSERV_U_$$GET1^DIQ(123.5,+RSERV_",",.01)_"^99CON|"_CTYPE
 S I=0,J=+$O(^TMP("ORWORD",$J,WP,1,0)),Z=Z+1 ; get first line
 S ORMSG(Z)="OBX|1|TX|2000.02^REASON FOR REQUEST^AS4||"_$G(^TMP("ORWORD",$J,WP,1,J,0))
 F  S J=$O(^TMP("ORWORD",$J,WP,1,J)) Q:J'>0  S I=I+1,ORMSG(Z,I)=^(+J,0)
 I $L(DIAG) D
 . N TYPE,VALUE S TYPE="TX",VALUE=DIAG
 . S:$L(CODE) TYPE="CE",VALUE=CODE_U_DIAG_"^I9C"
 . S Z=Z+1,ORMSG(Z)="OBX|2|"_TYPE_"|^PROVISIONAL DIAGNOSIS^||"_VALUE
 Q
