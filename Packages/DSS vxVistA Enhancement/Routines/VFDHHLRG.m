VFDHHLRG ;DSS/LM/PW - Patient registration API for single ID (MRN) - June 6, 2012
 ;;2012.1.1;VENDOR - DOCUMENT STORAGE SYS;;24 Jun 2013;Build 136
 ;Copyright 1995-2013,Document Storage Systems Inc. All Rights Reserved
 ;DSS/BUILD - VFDHHL HL7 EXCHANGE 2012.1.1 * 06/23/15 * vxdev64v2k8_VX13 * 2012.1 T22
 ;DSS/BUILD - VFDHHL HL7 EXCHANGE 2012.1.1 * 02/02/14 * vxdev64v2k8_DEVXOS * 2012.1.1T21
 ; This is used in stream with the ADT FILER and is dependant on the existence of 
 ; published SISI variables and call points
 Q
MRN ;
 ; DSS-specific new patient registration
 D:SISIALOG>2 XCPT^SISIAUTL(,"In MRN~VFDZREG")
 N DFN,X D DFN^SISIACCT
 I $G(DFN) D XCPT^SISIAUTL(,"Reg: Duplicate MRN",$G(HL("MID"))):SISIALOG Q
 S SISIRP=$G(SISIRP,$E($G(HL("ECH")," ~"),2)) ;Repetition character
 ; Replace array of identifiers with single-valued MRN
 N VFDPID3 S VFDPID3("MRN")=$P($G(@SISIPMSG@("PID",3)),SISID1)
 I $L(VFDPID3("MRN")) ;MRN
 E  D XCPT^SISIAUTL(,"REG: Missing MRN",$G(HL("MID"))):SISIALOG Q
 ; Next is based on ADT-A04 processing routine ^SISIADT4
 N RR,SISIENR,SISIFDA S RR=$NA(SISIFDA(2,"+1,"))
 ; Required fields - Replace pre-defined @RR
 S @RR@(.01)=$$JOIN^SISIADT4($$UP^XLFSTR($P($G(@SISIPMSG@("PID",5)),SISIRP)),SISID1)
 S @RR@(.02)=$E($G(@SISIPMSG@("PID",8)))
 S X=$$HL7TFM^XLFDT($G(@SISIPMSG@("PID",7)))
 S:X>0 @RR@(.03)=X
 ; End vxVistA required fields
 D UPDATE^DIE(,$NA(SISIFDA),$NA(SISIENR))
 S (DFN,SISIDFN)=$G(SISIENR(1))
 I 'DFN D XCPT^SISIAUTL(,"Registration failed",$G(HL("MID"))):SISIALOG Q
 D FILEID(DFN,.VFDPID3) ;File ALTERNATE IDENTIFIER (MRN)
 ; Save raw SSN for update.  SSN API and validation are in UPDATE code.
 N SISIPSSN S SISIPSSN=$G(@SISIPMSG@("PID",19))
 ; Process remaining fields
 D UPDATE^SISIAP05(DFN)
 ; Prior to patch SISIADT*2.0*4, ACCOUNT NUMBER pointer to PATIENT was not
 ; filed in the implementation-specific REGISTER PATIENT api context.
 S X=$$FIND1^DIC(9.7,,"X","SISIADT*2.0*4","B") I X,$$GET1^DIQ(9.7,X,.02,"I")=3 Q
 D FILE^SISIACCT(2,DFN)
 Q
FILEID(DFN,IDLIST) ;;File identifiers
 ; DFN=PATIENT IEN
 ; IDLIST=Identifiers subscripted "MRN", "SSN", etc.
 ; 
 Q:'$G(DFN)  Q:'$L($G(IDLIST("MRN")))  N RR,SISIFDA,SISIENR
 N VFDINST S VFDINST=$$INST(.IDLIST) Q:'VFDINST
 S RR=$NA(SISIFDA(2.0216,"?+1,"_DFN_","))
 S @RR@(.01)=VFDINST ;Default Institution
 S @RR@(.02)=IDLIST("MRN") ;Patient ID
 S @RR@(.04)=1 ;Default
 S @RR@(.05)="MRN"
 D:$D(SISIFDA) UPDATE^DIE(,$NA(SISIFDA),$NA(SISIENR))
 Q
INST(IDLIST) ;;Return File 4 IEN
 ; IDLIST=[By reference] Indexed list of patient ID's
 ; 
 N X
 S X=$G(HL("SFN")) I $L(X) S X=$O(^DIC(4,"SISI","ADT FILER",X,0)) Q:$L(X) X
 S X=$G(IDLIST("MRN")) Q:'$L(X) ""
 Q $O(^DIC(4,"SISI","ADT FILER",$E(X)_"MRN",0))
 ;
CDFN() ;;Called by COMPUTE DFN xecute (File 29320)
 ;
 N SISIDLST,SISIDFN,SISINST,SISIMRN
 S (SISIDLST("MRN"),SISIMRN)=$P($G(@SISIPMSG@("PID",3)),SISID1)
 Q:'$L(SISIMRN) ""
 S SISINST=$$INST(.SISIDLST) Q:'SISINST ""
 S SISIDFN=$S($L(SISIMRN):$$DFN(SISINST,SISIMRN),1:"")
 Q $S(SISIDFN:SISIDFN,1:"")  ;Use ONLY primary MRN
 ;
DFN(INST,AID) ;;Private - Called by CDFN above
 ; Lookup PATIENT by ALTERNATE ID
 ; 
 ; INST=File 4 IEN
 ; ID=Alternate ID for lookup
 ;
 N SISI,X,Y
 I $G(INST),$L(AID)
 E  Q ""
 Q $O(^DPT("AVFD",INST,AID,0))
 ; 
HISTORY ;
 ;6 June 2012 moved into the VFDHHL from VFDOSREG to be part of VFDHHL core
