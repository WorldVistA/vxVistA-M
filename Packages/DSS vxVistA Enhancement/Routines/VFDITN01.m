VFDITN01 ;DSS/KT - ENV/PRE/POST VFD*2013.1.3*20 ; 01/13/2015
 ;;2013.1.3;DSS,INC VXVISTA OPEN SOURCE;**20**;08 Jan 2015;Build 3
 ;Copyright 1995-2014,Document Storage Systems Inc. All Rights Reserved
 ;
 ;
 ;ICR #  SUPPORTED DESCRIPTION
 ;-----  --------------------------------------------
 ;
 ;
ENV ; environmental check
 Q
 ;
PRE ; pre-install
 Q
 ;
POST ; post-install for TN load - kt 3150113
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
T1 ;;VFDVX GMV V/M GUI
 ;;VFDC XPAR GET VALUE
 ;
