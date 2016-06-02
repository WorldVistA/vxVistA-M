VFDPXRM3 ;DSS/LJF - ART computed finding ;11/27/2013
 ;;1.0;VFDVX ADR;;FEB 3, 2014;Build 29
 ;Copyright 1995-2014, Document Storage Systems, Inc., All Rights Reserved
 ; Copy of routine: PXRMART ;SLC/DAN - ART computed finding ;01/11/2005
 ;
ARTCL(DFN,NGET,BDT,EDT,NFOUND,TEST,DATE,DATA,TEXT) ;
 S TEST=$$UP^XLFSTR(TEST)
 S NFOUND=0
 Q:TEST=""!(NGET=0)!("^IN:^DR:^"'[("^"_$E(TEST,1,3)_"^"))
 N WILD,ITEM,SUB,ING,INGT,TERM
 S WILD=$S(TEST["*":1,1:0),SUB=$S($E(TEST,1,2)="IN":"API",1:"APC")
 S TEST=$P(TEST,":",2)
 I 'WILD S ITEM=$S(SUB="API":+$O(^PS(50.416,"B",TEST,0)),1:TEST) Q:ITEM=0  D GETINFO Q
 S ITEM=$E(TEST,1,$L($P(TEST,"*")))
 I SUB="APC" F  S ITEM=$O(^GMR(120.8,SUB,DFN,ITEM)) Q:ITEM=""!(ITEM'[($E(TEST,1,$L($P(TEST,"*")))))!(NFOUND=NGET)  D GETINFO
 I SUB="API" S TERM=ITEM,ING=0 F  S ING=$O(^GMR(120.8,SUB,DFN,ING)) Q:'+ING  S INGT=$P(^PS(50.416,ING,0),U) I $E(INGT,1,$L(TERM))=TERM S ITEM=ING D GETINFO
 Q
 ;
GETINFO ;
 N GMRAR0,EDATE,IEN,SEVIEN
 S IEN="" F  S IEN=$O(^GMR(120.8,SUB,DFN,ITEM,IEN),-1) Q:'+IEN!(NFOUND=NGET)  D
 .S GMRAR0=^GMR(120.8,IEN,0)
 .S EDATE=$P(GMRAR0,U,4)
 .Q:EDATE<BDT!(EDATE>EDT)
 .S NFOUND=NFOUND+1,TEST(NFOUND)=1,DATE(NFOUND)=EDATE
 .S DATA(NFOUND,"REACTANT")=$P(GMRAR0,U,2)
 .S TEXT(NFOUND)="Documented reaction "_$S(SUB="API":"that includes the ingredient ",1:"to an agent in class ")_$S(SUB="APC":ITEM,1:$P(^PS(50.416,ITEM,0),U))_", reactant was: "_DATA(NFOUND,"REACTANT")_"."
 .;
 .; DSS/LJF - New fields: 013/12/05@12:24
 .S DATA(NFOUND,"OBSERVED/HISTORICAL")=$P(GMRAR0,U,6)  ;(field 6) from PATIENT ALLERGIES FILE (#120.8)
 .S DATA(NFOUND,"MECHANISM")=$P(GMRAR0,U,14)  ;(field 17) from PATIENT ALLERGIES FILE (#120.8)
 .S SEVIEN=$O(^GMR(120.85,"C",IEN,0))
 .I SEVIEN S DATA(NFOUND,"SEVERITY")=$P($G(^GMR(120.85,SEVIEN,0)),U,14)  ;(field 14.5) from ADVERSE REACTION REPORTING FILE (#120.85)
 Q
 ;
POSTART ; Post install to populate 'REMINDER COMPUTED FINDINGS FILE' with VFD-ALLERGY
 N VFDFDA,VFDIEN,VFDNAME,VERRMSG
 S VFDNAME="VFD-ALLERGY"
 S VFDIEN=$O(^PXRMD(811.4,"B",VFDNAME,0))
 I 'VFDIEN S VFDIEN="+1"
 S VFDFDA(811.4,VFDIEN_",",.01)=VFDNAME
 S VFDFDA(811.4,VFDIEN_",",.02)="VFDPXRM3"
 S VFDFDA(811.4,VFDIEN_",",.03)="ARTCL"
 S VFDFDA(811.4,VFDIEN_",",.04)="Allergy"
 S VFDFDA(811.4,VFDIEN_",",5)="M"
 S VFDFDA(811.4,VFDIEN_",",100)="L"
 D UPDATE^DIE("","VFDFDA",,"VERRMSG")
 ;
 S VFDIEN=$O(^PXRMD(811.4,"B",VFDNAME,0))
 I 'VFDIEN W !!,"** ERROR SETTING UP '"_VFDNAME_"' in 'REMINDER COMPUTED FINDINGS FILE' **" Q
 ;
 k VFDFDA,VERRMSG
 S VFDFDA(1)="Identifies any allergies that contain either the ingredient or drug"
 S VFDFDA(2)="class that you specify via the Computed Finding Parameter. Ingredients"
 S VFDFDA(3)="will be prefixed with IN: while DR: is used for drug classes. You may"
 S VFDFDA(4)="also use the * as a wildcard on the end of your selection. For example,"
 S VFDFDA(5)="to search for the ingredient aspirin you would enter IN:ASPIRIN. For"
 S VFDFDA(6)="drug class MS101 you would enter DR:MS101. For all ingredients"
 S VFDFDA(7)="beginning with ""ampi"" you would type IN:AMPI*. For all MS1 related"
 S VFDFDA(8)="drug classes you'd enter DR:MS1*."
 S VFDFDA(9)=""
 S VFDFDA(10)="Note: This computed finding does not support date reversal."
 D WP^DIE(811.4,VFDIEN_",",1,,"VFDFDA","VERRMSG")
 ;
 Q   ; END POSTART^VFDPXRM3
 ;
DETAIL(ORAY,DFN,ALLR,ID) ;RPC: VFD ORQQAL DETAIL - Replicates the VA RPC: ORQQAL LIST but adds MECHANISM Field to report
 ; RETURN DETAILED ALLERGY INFO FOR SPECIFIED ALLERGIC REACTION.
 D EN1^GMRAOR2(ALLR,"GMRACT")
 N CR,OX,OH,I S CR=$CHAR(13),I=1
 S ORAY(I)="    Causative agent: "_$P(GMRACT,U),I=I+1
 S ORAY(I)=" Nature of Reaction: "_$S($P(GMRACT,U,6)="ALLERGY":"Allergy",$P(GMRACT,U,6)="PHARMACOLOGIC":"Adverse Reaction",$P(GMRACT,U,6)="UNKNOWN":"Unknown",1:""),I=I+1 ;216
 ;
 S ORAY(I)=$$GET1^DIQ(120.8,ALLR,17,"E")
 I ORAY(I)="" K ORAY(I)
 E  S ORAY(I)="          Mechanism: "_ORAY(I),I=I+1
 ;
 S ORAY(I)=" ",I=I+1
 I $D(GMRACT("S",1)) D SYMP^ORQQAL
 I $D(GMRACT("V",1)) D CLAS^ORQQAL
 S ORAY(I)="         Originator: "_$P(GMRACT,U,2)_$S($L($P(GMRACT,U,3)):" ("_$P(GMRACT,U,3)_")",1:""),I=I+1 ;216
 S ORAY(I)="         Originated: "_$P(GMRACT,U,10),I=I+1 ;216
 I $D(GMRACT("O",1)) D OBS^ORQQAL
 S ORAY(I)="           Verified: "_$S($P(GMRACT,U,4)="VERIFIED":$P(GMRACT,U,8),1:"No"),I=I+1 ;216
 S ORAY(I)="Observed/Historical: "_$S($P(GMRACT,U,5)="OBSERVED":"Observed",$P(GMRACT,U,5)="HISTORICAL":"Historical",1:""),I=I+1
 I $D(GMRACT("C",1)) D COM^ORQQAL
 K GMRACT
 Q   ; END DETAIL^VFDPXRM3
 ;
POSTDETA ; Post Install for ALLERGY DETAIL
 N VFDDATA,VFDERR,VFDRET,VFDRPCIX,VFDOERRR,VFDOPTIX
 ; Get IEN for new RPC: VFD ORQQAL DETAIL
 D FIND^DIC(8994,,"@;.03","OPQXU","VFD ORQQAL DETAIL",,"B",,,"VFDRET","VFDERR")
 I $P($G(VFDRET("DILIST","1",0)),U,2)'="VFDPXRM3" Q
 S VFDRPCIX=+VFDRET("DILIST","1",0)
 K VFDERR,VFDRET
 ; Get IEN for OE/RR REPORT (file# 101.24)
 D FIND^DIC(101.24,,"@;.13","OPQXU","ORCV ALLERGIES DETAILS",,"B",,,"VFDRET","VFDERR")
 I $P($G(VFDRET("DILIST","1",0)),U,2)="ORQQAL DETAIL" D
 .S VFDOERRR=+VFDRET("DILIST","1",0)
 .; Add RPC IEN to field .13 of OE/RR REPORT
 .S VFDDATA(101.24,VFDOERRR_",",.13)=VFDRPCIX
 .K VFDERR
 .D FILE^DIE("","VFDDATA","VFDERR")
 ;
 ; Post Install for Option
 ; Find IEN of Option "VFDVX CPRS GUI CHART"
 K VFDERR,VFDRET
 D FIND^DIC(19,,"@;.01","OPQXU","VFDVX CPRS GUI CHART",,"B",,,"VFDRET","VFDERR")
 I $P($G(VFDRET("DILIST","1",0)),U,2)="VFDVX CPRS GUI CHART" D
 .S VFDOPTIX=+VFDRET("DILIST","1",0)
 .I $D(^DIC(19,VFDOPTIX,"RPC","B",VFDRPCIX)) Q    ; Quit if RPC already linked
 .; Add RPC to Option
 .K VFDDATA,VFDRET
 .S VFDDATA(19.05,"+1,"_VFDOPTIX_",",.01)=VFDRPCIX
 .D UPDATE^DIE("","VFDDATA","VFDRET")
 Q
 ;
POSTINST ;
 D POSTART
 D POSTDETA
 Q
