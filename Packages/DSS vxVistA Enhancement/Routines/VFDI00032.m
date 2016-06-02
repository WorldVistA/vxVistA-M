VFDI00032 ;DSS/RJS - VXVISTA 15.0 ENV/PRE/POST ; 10/06/2015 18:50
 ;;15.0;DSS,INC VXVISTA OPEN SOURCE;;06 Oct 2015;Build 1
 ;Copyright 1995-2015,Document Storage Systems Inc. All Rights Reserved
 ;
 ;This is the only routine that should be invoked from KIDS via the
 ;environmental routine field or the pre and post install routine
 ;fields in the KIDS BUILD file for vxVistA 15.0
 ;
 ;The environmental entry point is the first line of this routine.
 ;
 ;Any other routines needed in support of the install process will use
 ;the routine name format of VFDI0003x since we are allowing the
 ;use of up to 16 character routine names.
 ;
 ;---------------------------------------------------------------------
MIN ;;15.0
MAX ;;15.0
 ;---------------------------------------------------------------------
 ;   entry points for env,pre,post 
 ; environmental check
 ; check for min/max vxvista version
 N MAX,MIN
 S MIN=$P($T(MIN),";",3),MAX=$P($T(MAX),";",3)
 I $T(VERCK^VFDI0000)="" S XPDABORT=1
 E  I $$VERCK^VFDI0000(,MIN,MAX,1)'>0 S XPDABORT=1
 Q
 ;
 ;---------------------------------------------------------------------
 ;   entry points for pre,post
PRE ; pre-install entry point
 Q
 ;
 ;---------------------------------------------------------------------
POST ; post-install entry point
 ;---------------------------------------------------------------------
 D GET^VFDPSJFX ;  Cleanup bad entries in File #55
 Q
