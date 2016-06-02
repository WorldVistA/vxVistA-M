VFDI0009 ;DSS/SGM - ENV/PRE/POST VFD*2011.1.3*20 ; 12/30/2013 09:30
 ;;2011.1.3;DSS,INC VXVISTA OPEN SOURCE;**20**;16 Aug 2013;Build 4
 ;Copyright 1995-2013,Document Storage Systems Inc. All Rights Reserved
 ;
 ;
 ;ICR #  SUPPORTED DESCRIPTION
 ;-----  --------------------------------------------
 ;
 ;
ENV ; environmental check for VFD*2011.1.3*20
 Q
 ;
PRE ; pre-install for VFD*2011.1.3*20
 Q
 ;
POST ; post-install for VFD*2011.1.3*20
 N VFDATA,VFDMES
 S VFDATA("VFDVX CPRS GUI CHART","VFD CPRS TABS SHOW")=""
 D ADDRPC^VFDI0000(.VFDMES,,,.VFDATA)
 Q
 ;
 ;=====================================================================
 ;
PRE1 ;
 Q
 ;
 ;=====================================================================
 ;
