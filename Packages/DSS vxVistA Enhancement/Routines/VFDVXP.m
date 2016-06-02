VFDVXP ;DSS/SMP - vxPatient Picture Utilities ; 05/07/2014 12:35
 ;;1.0;DSS,INC VXVISTA OPEN SOURCE;;;Build 27
 ;Copyright 1995-2015,Document Storage Systems Inc. All Rights Reserved
 ;
ID(RET,DFN,FORMAT) ; [RPC] VFDVXP BCMA ID
 ; Inputs:  DFN    - req - Patient DFN
 ;          FORMAT - req - Instance for the parameter VFDVXP BCMA ID 
 ;                         FORMAT
 ;
 N I,PARAM,VA,VADM,VAERR,VAPD,VFDERR,VFDI,VFDLOC
 S VFDLOC=$G(DUZ(2))
 I $G(FORMAT)="" S RET(1)="-1^Invalid Format" Q
 D DEM^VADPT
 S I=1,RET(I)="PNAME^"_$G(VADM(1)),I=I+1
 S RET(I)="MRN^"_$G(VA("MRN")),I=I+1
 S RET(I)="BARCODE^"_$$GET1^VFDCXPR(,"~VFDVXP BARCODE TYPE",1),I=I+1
 D GETWP^XPAR(.PARAM,"ALL","VFDVXP BCMA ID FORMAT",FORMAT,.VFDERR)
 I $G(VFDERR)>0 K RET S RET(1)="-1^Unexpected problem encountered" Q
 I '$D(PARAM(1,0)) D  Q
 .K RET S RET(1)="-1^Parameter VFDVXP BCMA ID FORMAT not set properly"
 F VFDI=1:1 Q:'$D(PARAM(VFDI,0))  X PARAM(VFDI,0)
 Q
 ;
