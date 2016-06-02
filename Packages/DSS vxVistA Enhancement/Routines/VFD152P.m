VFD152P ;DSS/JCH VFD*15.0*2 POST-INSTALL ; 09/13/15 2:21pm
 ;;15.0;DSS,INC VXVISTA OPEN SOURCE;;13 Sep 2015;Build 5
 ;Copyright 1995-2015,Document Storage Systems Inc. All Rights Reserved
 Q
 ; 
ADDRPC ; Add VFD PSO GET ADMIN remote procedure to VFDVX CPRS GUI CHART option
 N OPTIEN,RPCIEN,ERR,OUT1,OUT2
 S OPTIEN=$$FIND1^DIC(19,"","","VFDVX CPRS GUI CHART",,,"ERR")
 I 'OPTIEN D EN^DDIOL("Unable to find VFDVX CPRS GUI CHART option") Q
 S RPCIEN=$$FIND1^DIC(8994,"","","VFD PSO GET ADMIN",,,"ERR")
 I 'RPCIEN D EN^DDIOL("Unable to find VFD PSO GET ADMIN remote procedure") Q
 ;
 S FDA(19.05,"?+1,"_OPTIEN_",",.01)=RPCIEN
 D UPDATE^DIE("","FDA","OUT1","OUT2")
 I '$G(OUT1(1)) D EN^DDIOL("Unable to add VFD PSO GET ADMIN Remote Procedure to VFDVX GUI CHART option")
 Q
