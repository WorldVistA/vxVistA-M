VFDPSB ;DSS/LM/SMP - vxBCMA to vxPAMS interface ; 10/14/2015
 ;;15.0;DSS,INC VXVISTA OPEN SOURCE;**20**;01 Dec 2009;Build 2
 ;Copyright 1995-2015,Document Storage Systems Inc. All Rights Reserved
 ;
 Q
 ;
 ;=====================================================================
 ;                 R E M O T E   P R O C E D U R E S
 ;=====================================================================
 ;
EVENT(VFDRSLT,VFDIEN,VFDSCAN,VFDTAB) ;{REMOTE PROCEDURE} VFD BCMA EVENT
 ; INPUT:
 ;   VFDIEN  = [Required] BCMA MEDICATION LOG internal entry number
 ;   VFDSCAN = [Required] Scan code of medication associated with the
 ;             log entry
 ;   VFDTAB  = [Required for unit dose or PB] "UDTAB" = unit dose.
 ;             Else, IV/IVPB ward stock scan.
 ;
 ; OUTPUT:
 ;   VFDRSLT = Return value by reference: 
 ;             0=Success or "-1^Text" for error
 ;
 D EVENT^VFDPSB1(.VFDRSLT,.VFDIEN,.VFDSCAN,.VFDTAB)
 Q
 ;
SCAN(VFDRSLT,INPUT) ; RPC [VFD BCMA NDC SCAN]
 ; INPUT(n) - req where INPUT(n) = tag ^ value for n=1,2,3,...
 ; 
 ; CODE  REQ VALUE
 ; ----- --- -----------------------------------------------------------
 ; DFN    Y  Patient file (#2) IEN
 ; REC    Y  Record data in the form of:
 ;              DRUG IEN ^ DT ^ SCAN
 ;
 ;              DRUG IEN ^ DT ^ SCAN ^ DRUG IEN ^ DT ^ SCAN ....
 ;
 ;            Could be one or more
 ; Outputs:
 ;      1  Success
 ;      0  Failure
 ;
 D SCAN^VFDPSB3(.VFDRSLT,.INPUT)
 Q
 ;
INJSITE(VFDRSLT,VFDOR) ; RPC [VFD BCMA INJ SITE REQ]
 ; Inputs:  VFDOR - req - Order File IEN
 ;
 ; Outputs:
 ;      1  Success
 ;      0  Failure
 ;     -1^Error
 ;
 D INJSITE^VFDPSB3(.VFDRSLT,VFDOR)
 Q
 ;
 ;=====================================================================
 ;                           F U N C T I O N S
 ;=====================================================================
NDC(VFDDIEN,VFDSCAN) ;[Public]   Match SYNONYM or DRUG to NDC
 ; VFDDIEN=[Required] File 50 (DRUG) IEN
 ; VFDSCAN=[Required] Raw scan code (See EVENT^VFDPSB documentation)
 Q $$NDC^VFDPSB1(VFDDIEN,VFDSCAN)
