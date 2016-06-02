VFDI0024 ;DSS/SGM - ENV/PRE/POST VFD*2011.1.3*24 ; 02/19/2014 10:15
 ;;2011.1.3;DSS,INC VXVISTA OPEN SOURCE;**24**;16 Aug 2013;Build 1
 ;Copyright 1995-2013,Document Storage Systems Inc. All Rights Reserved
 ;
 ;
 ;ICR #  SUPPORTED DESCRIPTION
 ;-----  --------------------------------------------
 ;
 ;
ENV ; environmental check for VFD*2011.1.3*24
 Q
 ;
PRE ; pre-install for VFD*2011.1.3*24
 Q
 ;
POST ; post-install for VFD*2011.1.3*24
 S ^%ZOSF("TEST")="I X?1(1""%"",1A).ANP,$D(^$ROUTINE(X))"
 Q
 ;
 ;=====================================================================
 ;
PRE1 ;
 Q
 ;
 ;=====================================================================
 ;
