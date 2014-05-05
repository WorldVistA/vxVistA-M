VFDCVT ;DSS/WLC - Quick Docs for VFDC VISIT APIs ;06/07/2006 13:26
 ;;2011.1.2;DSS,INC VXVISTA OPEN SOURCE;;28 Jan 2013;Build 153
 ;Copyright 1995-2013,Document Storage Systems Inc. All Rights Reserved
 ;
 ;DBIA# Supported Reference
 ;----- --------------------------------
 ;10005 DT^DICRW
 ;10086 HOME^%ZIS
 ;10104 $$CJ^XLFSTR
 ;
 N I,X,Y,Z
 D DT^DICRW,HOME^%ZIS W @IOF
 S Z="",$P(Z,"-",80)=""
 F I=1:1 S X=$T(T+I) Q:X=""  S X=$P($T(T+I),";",3,99) D
 .I X'?1"VFDCVT".E W !,"  "_X Q
 .W !,Z,!,$$CJ^XLFSTR("Routine "_X,80),!,Z
 .Q
 D ^VFDCVT00  ; continuation of documentation
 Q
T ;
 ;;VFDCVT:
 ;;displays brief documentation for the various VFDCVT* calls to the screen.
 ;;
 ;;VFDCVT00:
 ;;displays brief documentation for the various VFDCVT* calls to the screen.
 ;;
 ;;VFDCVT0:
 ;;
 ;;     Driver call for APPT^VFDCVT1 described below.
 ;;       
 ;; VST(RETX,DFN,BEG,END,ZLOC,CAT,SCR)  RPC:  VFDC GET VISITS ONLY
 ;;  
 ;;     Driver call for VS^VFDCVT1 described below.
 ;;     
 ;; VSIT(RETV,DATA,SCR)  RPC:  VFDC GET VISITS/APPOINTMENT
 ;;  
 ;;     Driver call for VSIT^VFDCVT1 described below.
 ;;     
 ;;   
 ;;   
 ;;         
 ;;VFDCVT1:
 ;;
 ;;  SCCND(RET,DATA)
 ;;    this will call PCE to return Service Connected conditions for a patient.
 ;;    This is only called as an RPC.
 ;;    INPUT:
 ;;        DATA = DFN ^ appt/visit FM date/time ^ location ^ visit pointer
 ;;    OUTPUT:
 ;;        RET = ao^ec^ir^sc^mst^hnc^cv^SHAD where each piece is either 1 or ""
 ;; 
 ;; 
 ;;  APPT(RET, DATA, SCR)
 ;;     this will return a list of appointments per patient based on parameters
 ;;     defined inside DATA:
 ;;     
 ;;      DATA:
 ;;
 ;;      DFN - required - patient file ien
 ;;      BEG - optional - earliest appointment date/time [fm d/t format]
 ;;      END - optional - latest appointment
 ;;                               date/time [fm d/t format]
 ;;      ALOC - optional - clinic name or file 44 
 ;;                        ien [if not then get all]
 ;;                     preserved for backward compatibility - 8/27/2001 sgm
 ;;      FLG - optional - default = 0
 ;;          0 return active/kept/inpatient/no action taken
 ;;          1 return future appts only
 ;;          2 return both (0 & 1)
 ;;      SCR    passed by reference
 ;;             array of screened values where each is a IEN for a
 ;;             certain file.
 ;;               Where:
 ;;                 SCR(Code, IEN) = ""
 ;;                      Code = "C" for Clinic
 ;;                             "D" for Division
 ;;                             "S" for Stop Code
 ;;                     
 ;;                      IEN = 44 for "C"
 ;;                            40.8 for "D"
 ;;                            40.7 for "S"
 ;;                    
 ;;          return @RET@(#) = ext date.time^ext loc^int date.time^file 44 ptr
 ;;
 ;;  DIV(QLOC)
 ;;     Return medical center division (#40.8) pointer 
 ;;     for a hospital location (LOC).  Called as 
 ;;     Extrinsic function - not RPC.
 ;;     
 ;;         QLOC:  Location IEN to look up
 ;;         
 ;;         return Medical Center Division (#3.5)
 ;;        
 ;;  LOC(VAL)
 ;;     Convert location name to a pointer.  Kept for backward compatibility.
 ;;     Verification of IEN also performed.  Called as extrinsic function.
 ;;     
 ;;         VAL:  Location name to lookup.
 ;;         
 ;;         return IEN, otherwise NIL ("")
 ;;         
 ;;
