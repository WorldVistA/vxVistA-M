VFDI0011 ;DSS/SGM - ENV/PRE/POST VFD*2011.1.3*25 ; 02/14/2014 13:45
 ;;2011.1.3;DSS,INC VXVISTA OPEN SOURCE;**25**;16 Aug 2013;Build 5
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
ENV ; environmental check for VFD*2011.1.3*25
 Q
 ;
PRE ; pre-install for VFD*2011.1.3*25
 Q
 ;
POST ; post-install
 N I,J,X,Y,RPC,SUP,VFDATA,VFDMES,VXS
 ; add RPCs to menu context(s) if they are missing
 ;   DSIC rpcs will not exist in vxVistA OS
 F I=1:1:1 S X=$P($T(@("T"_I)),";",3) D
 . F J=1:1 S Y=$P($T(@("T"_I_"+"_J)),";;",2,99) Q:Y=""  D
 . . S SUP=$P(Y,";")="S",RPC=$P(Y,";")
 . . I RPC'="" S VFDATA(X,RPC)=""
 . . Q
 . Q
 D ADDRPC^VFDI0000(.VFDMES,,,.VFDATA)
 Q
 ;
 ;-----------------------  PRIVATE SUBROUTINES  -----------------------
 ; T# line label ;;<name of OPTION to add RPCs to>
 ; T#+offset ;;<<S>;name of RPC to add
 ;     ;S should only be set if this RPC is for supported accounts only
 ; 
T1 ;;VFDVX CPRS GUI CHART
 ;;VFD IS AUTHORIZED
 ;;VFD CREATE JUSTIFICATION
 ;;VFD END JUSTIFICATION
 ;
