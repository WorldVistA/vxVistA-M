VFDDUZ1 ;DSS/LM - Utilities supporting NEW PERSON LOOKUP ;12/31/2013 11:55
 ;;2013.0;DSS,INC VXVISTA OPEN SOURCE;;07 Feb 2014;Build 2
 ;Copyright 1995-2014,Document Storage Systems Inc. All Rights Reserved
 ;
 ; This routine is only invoked via the ^VFDDUZ routine
 ; deprecated and removed from vxVistA - 12/31/2013/sgm
 ;
 Q
 ;
FIND1(VFDVAL,VFDTYP,VFDLOC) Q $$FIND1^VFDCDUZ(.VFDVAL,.VFDTYP,.VFDLOC)
