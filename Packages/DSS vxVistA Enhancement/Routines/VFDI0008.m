VFDI0008 ;DSS/SGM - ENV/PRE/POST VFD*2011.1.3*19 ; 12/30/2013 09:30
 ;;2011.1.3;DSS,INC VXVISTA OPEN SOURCE;**19**;16 Aug 2013;Build 5
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
ENV ; environmental check for VFD*2011.1.3*19
 Q
 ;
PRE ; pre-install for VFD*2011.1.3*19
 Q
 ;
POST ; post-install for VFD*2011.1.3*19
 D POST1
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
 N VFDIEN,VFDIENS,VFDERR,VFDFDA
 S VFDIEN=$$FIND1^DIC(101.24,,"OX","ORRPW ADT INS",,,"VFDERR")
 I 'VFDIEN D MSG(1) Q
 S VFDIEN(1)=$$FIND1^DIC(101.243,","_VFDIEN_",","OX","Type of Coverage",,,"VFDERR")
 I VFDIEN(1) D MSG(2) Q
 S VFDFDA(101.243,"+1,"_VFDIEN_",",.01)="Type of Coverage"
 S VFDFDA(101.243,"+1,"_VFDIEN_",",.03)=17
 S VFDFDA(101.243,"+1,"_VFDIEN_",",.04)=0
 S VFDFDA(101.243,"+1,"_VFDIEN_",",.06)=20
 S VFDFDA(101.243,"+1,"_VFDIEN_",",.07)=1
 D UPDATE^DIE(,"VFDFDA","VFDIENS","VFDERR")
 I $D(VFDERR) D MSG(3) Q
 E  D MSG(4)
 Q
 ;
MSG(LINE) ;
 ;;*** ERROR:  Cannot find report ORRPW ADT INS ***
 ;;Type of Coverage column header already exists
 ;;*** ERROR:  Failed to add Type of Coverage column ***
 ;;Type of Coverage column successfully added
 ;;
 N I,J,X,Y,Z,VFDM
 S (VFDM(1),VFDM(3))=" "
 S VFDM(2)=$P($T(MSG+LINE),";;",2)
 D MES^XPDUTL(.VFDM)
 Q
 ;
