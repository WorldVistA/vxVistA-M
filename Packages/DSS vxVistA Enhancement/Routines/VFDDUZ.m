VFDDUZ ;DSS/LM - Utilities supporting NEW PERSON LOOKUP ;12/31/2013 11:55
 ;;2013.0;DSS,INC VXVISTA OPEN SOURCE;;07 Feb 2014;Build 2
 ;Copyright 1995-2014,Document Storage Systems Inc. All Rights Reserved
 ;
 ;***********************************
 ;*******     PLEASE NOTE     *******
 ;***********************************
 ; This routine deprecated 12/31/2013/sgm
 ; Code moved to VFDCDUZ where it should have been in the first place
 ; As of this date, %RFIND found on the following references to this
 ; routine:  VFDORP1
 ;
 Q
 ;
 ;=====================================================================
 ;                   NEW PERSON LOOKUP ON AVFD INDEX
 ;=====================================================================
FIND1(VFDVAL,VFDTYP,VFDLOC) Q $$FIND1^VFDCDUZ(.VFDVAL,.VFDTYP,.VFDLOC)
 ;
 ;=====================================================================
 ;RPC: VFD USER ID               RETURN USER DATA INCLUDING DPS AND NPI
 ;=====================================================================
ID(VFDRSLT,VFDDUZ) D ID1^VFDCDUZ(.VFDRSLT,.VFDDUZ) Q
