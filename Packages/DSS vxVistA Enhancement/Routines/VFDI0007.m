VFDI0007 ;DSS/SGM - ENV/PRE/POST VFD*2013.0*1 ; 12/27/2013 16:50
 ;;2013.1;DSS,INC VXVISTA OPEN SOURCE;**1**;16 Aug 2013;Build 1
 ;Copyright 1995-2013,Document Storage Systems Inc. All Rights Reserved
 ;
 ; This is the entry point for the environmental check for vxVistA
 ; 2013.0 and the corresponding GUIs for vxCPRS, vxVitals, vxBCMA
 ;   VFDI0005A - executable code for vxVistA and vxVistA Updates
 ;   VFDI0005B - executable code for vxCPRS
 ;   VFDI0005C - executable code for vxBCMA
 ;   VFDI0005D - executable code for vxVitals
 ;
 ;ICR #  SUPPORTED DESCRIPTION
 ;-----  --------------------------------------------
 ;
 ;
ENV ; environmental check for VFD*2013.0*1
 Q
 ;
PRE ; pre-install for VFD*2013.0*1
 Q
 ;
PREBCMA ; pre-install for vxBCMA 2013.0
 Q
 ;
PRECPRS ; pre-install for vxCPRS 2013.0
 Q
 ;
PREGMV ; pre-install for vxVitals 2013.0
 Q
 ;
POST ; post-install for VFD*2013.0*1
 D POST1
 Q
 ;
POSTBCMA ; post-install for vxBCMA 2013.0
 Q
 ;
POSTCPRS ; post-install for vxCPRS 2013.0
 Q
 ;
POSTGMV ; post-install for vxVitals 2013.0
 Q
 ;
 ;=====================================================================
 ;
PRE1 ;
 Q
 ;
 ;=====================================================================
 ;
POST1 ; Add MEDICARE PART D to TYPE OF INSURANCE COVERAGE file if new
 N VFDIEN,VFDERR,VFDFDA,VFDMSG,VFDWP
 S VFDIEN=$$FIND1^DIC(355.2,,"OX","MEDICARE PART D",,,"VFDERR")
 I VFDIEN D MSG(1) Q
 S VFDFDA(355.2,"+1,",.01)="MEDICARE PART D"
 S VFDFDA(355.2,"+1,",.02)="MAPD"
 D UPDATE^DIE(,"VFDFDA","VFDIEN","VFDERR")
 I $D(VFDERR) D MSG(2) Q
 F I=1:1:6 S VFDWP(I)=$P($T(DESC+I),";;",2)
 D WP^DIE(355.2,VFDIEN(1)_",",10,,"VFDWP","VFDERR")
 I $D(VFDERR) D MSG(2) Q
 E  D MSG(3)
 Q
 ;
MSG(LINE) ;
 ;;MEDICARE PART D already exists in file 355.2
 ;;*** Error adding MEDICARE PART D to file 355.2 ***
 ;;MEDICARE PART D successfully added to file 355.2
 ;;
 N I,J,X,Y,Z,VFDM
 S (VFDM(1),VFDM(3))=" "
 S VFDM(2)=$P($T(MSG+LINE),";;",2)
 D MES^XPDUTL(.VFDM)
 Q
 ;
 ;---------------------------------------------------------------------
DESC ;
 ;;A federal program to subsidize the costs of prescription drugs for 
 ;;Medicare beneficiaries in the United States. It was enacted as part of
 ;;the Medicare Modernization Act of 2003 (MMA) and went into effect on
 ;;January 1, 2006. Also called the Medicare Prescription Drug Benefit.
 ;;Coverage is available only through insurance companies and HMOs and is
 ;;voluntary.
 ;;
 Q
 ;
