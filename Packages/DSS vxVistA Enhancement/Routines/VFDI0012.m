VFDI0012 ;DSS/SGM - ENV/PRE/POST VFD*2013.0*9 ; 04/04/2014 14:30
 ;;2013.1;DSS,INC VXVISTA OPEN SOURCE;**9**;16 Aug 2013;Build 4
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
ENV ; environmental check for VFD*2013.0*9
 Q
 ;
PRE ; pre-install for VFD*2013.0*9
 N VFD,VFDERR
 D DELIX^DDMOD(14.7,.01,1,"K","VFD","VFDERR")
 Q
 ;
POST ; post-install for VFD*2013.0*9
 N DIK
 S DIK="^%ZIS(14.7,",DIK(1)=".01^B"
 D ENALL^DIK
 Q
