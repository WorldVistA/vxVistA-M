VFDPXIM0 ;DSS/WLC/JM - MAIN ENTRY TO VFDPXIM ROUTINES ;04/19/2011 9:20
 ;;15.0;DSS,INC VXVISTA OPEN SOURCE;**10,19**;28 Jan 2013;Build 3
 ;Copyright 1995-2015,Document Storage Systems Inc. All Rights Reserved
 ;
 ;This routine will be the main entry point into all of the VFDPXIM*
 ;routines.  Starting Mar 1, 2006 all new DSS applications should only
 ;call line labels in this routine.  As this routine will potentially
 ;have many entry points, detailed documentation for entry point will
 ;be in the VFDPXIM* routine that is invoked.
 ;
 ;All Integration Agreements for VFDPXIM*
 ;DBIA#  Supported Reference
 ;-----  ----------------------------------------------------
 ;
GETORD(VFDIM,DFN)  ;  RPC:  VFD PXIM GET IMM ORDERS
 D GETORD^VFDPXIM1(.VFDIM,DFN)
 Q
 ;
GETLOTS(VFDIM,ORN)  ; RPC:  VFD PXIM GET LOTS
 D GETLOTS^VFDPXIM1(.VFDIM,ORN)
 Q
 ;
GETIMM(VFDIM)  ; RPC:  VFD PXIM GET IMMUN
 D GETIMM^VFDPXIM1(.VFDIM)
 Q
 ;
STORLOT(VFDIM,VFDRAY) ;RPC:  VFD PXIM STORE LOT
 D STORLOT^VFDPXIM1(.VFDIM,VFDRAY)
 Q
 ;
ADMIN(VFDIM,VFDRAY) ;RPC:  VFD PXIM ADMIN
 D ADMIN^VFDPXIM2(.VFDIM,VFDRAY)
 Q
 ;
ISIMM(VFDIM,VFDORN,VFDOI) ; RPC:  VFD PXIM IS IMMUN ORDER
 I $G(VFDOI) S VFDIM=$$ISIMMOI^VFDPXIM1(+$G(VFDORN)) Q
 S VFDIM=$$ISIMM^VFDPXIM1(+$G(VFDORN))
 Q
 ;
ERROR(VFDIM,VFDARR) ; RPC: VFD PXIM ENTERED IN ERROR
 S VFDIM=$$ERROR^VFDPXIM1(.VFDARR)
 Q
 ;
ROUTES(VFDIM) ;RPC: VFD PXIM ROUTES
 D ROUTES^VFDPXIM1(.VFDIM)
 Q
 ;  
SITES(VFDIM) ;RPC: VFD PXIM SITES
 D SITES^VFDPXIM1(.VFDIM)
 Q
 ;  
SOURCES(VFDIM) ;RPC: VFD PXIM SOURCES - May not be needed but, here it is if we do!
 D SOURCES^VFDPXIM1(.VFDIM)
 Q
 ;  
VIS(VFDIM,IMM) ;RPC: VFD PXIM VIS
 D VIS^VFDPXIM1(.VFDIM,IMM)
 Q
 ;
PRODUCT(VFDIM,IMM) ; rpc - VFD PXIM PRODUCT NAMES
 D PRODUCT^VFDPXIM1(.VFDIM,IMM)
 Q
