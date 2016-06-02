VFDORDS ;DSS/SMP - DATA SEGREGATION ;Feb 13, 2014
 ;;2011.1.3;DSS,INC VXVISTA OPEN SOURCE;**25**;;Build 5
 ;Copyright 1995-2014,Document Storage Systems Inc. All Rights Reserved
 ;
 Q
 ;
AUTH(VFDRET,LOC) ;  RPC [VFD IS AUTHORIZED]
 ;Input Parameter:
 ;   LOC - req - patient location
 ;               pointer to file 44
 ;Return Value: 
 ;   VFDRET - boolean
 ;            0 - provider is not authorized (default)
 ;            1 - provider is authorized
 ;
 S VFDRET=0 ; default to FALSE
 D AUTH^VFDORDS1
 Q
 ;
ADDJUST(VFDRET,DFN,LOC,JUST) ; RPC [VFD CREATE JUSTIFICATION]
 ;Input Parameters:
 ;   DFN - req - patient ien - pointer to file 2
 ;   LOC - req - patient location - pointer to file 44
 ;  JUST - req - WP field
 ;
 ;Return Value: 
 ;   VFDRET - 1^IEN or -1^Error
 S VFDRET="-1^Failed to create JUSTIFICATION"
 D ADDJUST^VFDORDS1
 Q
 ;
ENDJUST(VFDRET,JUST) ; RPC [VFD END JUSTIFICATION]
 ;Input Parameter:
 ;  JUST - req - justification ien - pointer to file 21620
 ;
 ;Return Value: 
 ;   VFDRET - 1^IEN or -1^Error
 S VFDRET=1_U_JUST
 D ENDJUST^VFDORDS1
 Q
 ;
GETDIV(VFDRET,DFN) ; Return an array of the user's authorized divisions
 D GETDIV^VFDORDS1(.VFDRET,DFN)
 Q
