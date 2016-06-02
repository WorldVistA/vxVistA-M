VFDI0021 ;DSS/SMP - ENV/PRE/POST VFD*2013.1*45 ; 02/06/2015 16:30
 ;;2013.1;DSS,INC VXVISTA OPEN SOURCE;**45**;29 May 2015;Build 16
 ;Copyright 1995-2015,Document Storage Systems Inc. All Rights Reserved
 ;
 ;
 ;ICR # SUPPORTED DESCRIPTION
 ;----- --------------------------------------------
 ;
 ;
ENV ; environmental check for VFD*2013.1*45
 Q
 ;
PRE ; pre-install for VFD*2013.1*45
 Q
 ;
POST ; post-install for VFD*2013.1*45
 D ADDRPC
 Q
 ;
ADDRPC ;
 N I,J,X,Y,RPC,SUP,VFDATA,VFDMES,VXS
 ; add RPCs to menu context(s) if they are missing
 ; DSIC rpcs will not exist in vxVistA OS
 F I=1:1 S X=$P($T(@("T"_I)),";",3) Q:X=""  D
 . F J=1:1 S Y=$P($T(@("T"_I_"+"_J)),";;",2,99) Q:Y=""  D
 . . S SUP=$P(Y,";")="S",RPC=$P(Y,";")
 . . I RPC'="" S VFDATA(X,RPC)=""
 . . Q
 . Q
 D ADDRPC^VFDI0000(.VFDMES,,,.VFDATA)
 Q
 ;
 ;----------------------- PRIVATE SUBROUTINES -----------------------
 ; T# line label ;;<name of OPTION to add RPCs to>
 ; T#+offset ;;<<S>;name of RPC to add
 ; ;S should only be set if this RPC is for supported accounts only
 ; 
T1 ;;VFDVX CPRS GUI CHART
 ;;VFDPSO IS DRUG EPT
 ;;VFDPSORL RELEASE
 ;
