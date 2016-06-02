VFDI0026 ;DSS/SMP - ENV/PRE/POST VXVISTA 15.0 Overlay ; 07/7/15 14:25
 ;;15.0;DSS,INC VXVISTA OPEN SOURCE;;16 Aug 2013;Build 29
 ;Copyright 1995-2013,Document Storage Systems Inc. All Rights Reserved
 ;
ENV ; environmental check for 15.0 Overlay
 I '$$PATCH^XPDUTL("PX*1.0*201") S XPDQUIT=1
 Q
 ;
V ;;15.0;;;;
 Q
 ;ICR #  SUPPORTED DESCRIPTION
 ;-----  --------------------------------------------
 ;
 ;
 ;
PRE ; pre-install for 15.0 Overlay
 K XUMF,VFDIDUZ S XUMF=1 I $G(DUZ(0))'="@" M VFDIDUZ=DUZ S DUZ(0)="@"
 Q
 ;
POST ; post-install for 15.0 Overlay
 D POST1,POST2,POST3,POST4
 K XUMF I $D(VFDIDUZ) K DUZ M DUZ=VFDIDUZ K VFDIDUZ
 Q
 ;
 ;=====================================================================
 ;                        P R E - I N S T A L L
 ;=====================================================================
 ;
 ;
 ;=====================================================================
 ;                       P O S T - I N S T A L L
 ;=====================================================================
 ;
POST1 ; update the vxVistA Version Parameter
 N I,J,X,Y,Z,VER
 I $G(^%ZOSF("ZVX"))="" S ^%ZOSF("ZVX")="VXOS"
 S VER=$P($T(V),";",3) I VER>0 D CHG^VFDCXPR(,"SYS~VFD VXVISTA VERSION~1~"_VER)
 Q
 ;
POST2 ; post-install - Register RPC
 N I,J,X,Y,RPC,SUP,VFDATA,VFDMES,VXS
 ; add RPCs to menu context(s) if they are missing
 ;   DSIC rpcs will not exist in vxVistA OS
 F I=1:1 S X=$P($T(@("T"_I)),";",3) Q:X=""  D
 . F J=1:1 S Y=$P($T(@("T"_I_"+"_J)),";;",2,99) Q:Y=""  D
 . . S SUP=$P(Y,";")="S",RPC=$P(Y,";")
 . . I RPC'="" S VFDATA(X,RPC)=""
 . . Q
 . Q
 D ADDRPC^VFDI0000(.VFDMES,,,.VFDATA)
 Q
 ;
POST3 ; DGPM Post-Install
 N X,DGPMDA
 D POST^VFDDGPM
 S DGPMDA=0
 F  S DGPMDA=$O(^DGPM(DGPMDA)) Q:'DGPMDA  S X=$G(^(DGPMDA,0)) D:X
 .I $P(X,U,2)=3 D 3^VFDDGPM2
 Q
 ;
POST4 ; Post install for VFDVX ADR 1.0
 D POSTINST^VFDPXRM3
 Q
 ;
 ;
 ;-----------------------  PRIVATE SUBROUTINES  -----------------------
 ; T# line label ;;<name of OPTION to add RPCs to>
 ; T#+offset ;;<<S>;name of RPC to add
 ;     ;S should only be set if this RPC is for supported accounts only
 ; 
T1 ;;OR BCMA ORDER COM
 ;;VFDC XPAR GETWP
 Q
