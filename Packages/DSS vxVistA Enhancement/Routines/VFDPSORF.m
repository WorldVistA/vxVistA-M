VFDPSORF ;DSS/RJS - VXVISTA RX REFILL COMPLETE ; 8/07/15
 ;;15.0;DSS,INC VXVISTA OPEN SOURCE;**17,57**;;Build 4
 ;Copyright 1995-2015,Document Storage Systems Inc. All Rights Reserved
 ;
 ;This routine is called to finish and release an refill prescription
 ;without the use of backdoor O/P Pharmacy.  The result of this routine
 ;is to create a ^XTMP global with pertinent information from the
 ;prescription to be used by the VFDPOAE routine to generate a printed
 ;prescription to be given to the patient upon leaving.
 ; 
 ;Direct sets to the Prescription global used in order to get around
 ;inconsistencies with the global and FILEMAN.  Specifically, an error
 ;occurs when trying to add a record through the database calls because
 ;a previous field is using a subsequently set field as part of a MUMPS
 ;style cross-reference.  Since the latter field has not been added to
 ;the file yet, the cross-reference errors out.  In addition, there are
 ;some inconsistencies with required identifiers in the file.  This
 ;program does use some FILEMAN database calls to do updates once the
 ;record is created.
 ; 
 ; ICR:
 ; 4821    PEN^PSO5241
 ; Add, edits, and delete to file #52 and 52.41 have NO ICR's
 ;
 Q
 ;DSS/RJS - BEGIN OPT REFILL RELEASE
REFILL(AXY,VFDORD,VFDSCAN) ;Finish and release an RX REFILL without backdoor pharmacy
 I '$G(VFDORD) S AXY="-1^Missing Order Number!" Q
 I '$G(VFDSCAN)!($G(VFDSCAN)'?1.N) S AXY="-1^Invalid Scan Value" Q
 N VFDPU,VFDPUI,VFDISSUE,VFDMSG,VFDPEND
 S VFDPEND=$$GET1^DIQ(100,+VFDORD,5)="ACTIVE"
 S VFDPU="I",VFDISSUE=1 D RELEASE
 S AXY=$S($G(VFDMSG)]"":VFDMSG,1:1)
 Q
RELEASE ; Finish and Release the order from PS(52.41 & OR(100)
 N %,I,X,X1,X2,Y,Z,DA,DIE,DIK,DOSE,DOSE1,DR,ERR,FDAX,ID,II,IMMUN,OR0,ORVP,PEND
 N PSOANSQ,PSOCOM,PSOCOU,PSOCOUU,PSOCS,PSODAT,PSODATE,PSODFN,PSODRG,PSORF,DLAYGO
 N PSODRUG,PSOIEN,PSOINS1,PSOL,PSOM,PSONEW,PSONRXN,PSOORG,PSOPEN,PSOPI,VFDNDC
 N PSOPRC,PSOREC0,PSORET,PSORSTAT,PSORX,PSORXN,PSOSCP,PSOSIG,PSOSITE,VFDORG
 N PSOTN,PSOY,PTRF,REC,ROUTE,SIGOK,SITE,UNITS,VFDORIT,VFDSTA,VFDXX,VFDRF,DINUM
 S X=+$G(VFDORD) N VFDORD S VFDORD=X
 S VFDORIT=$G(^OR(100,VFDORD,.1,1,0))
 I +VFDORIT S ID=+$P(^ORD(101.43,VFDORIT,0),U,2)
 S VFDMSG=""
 S (PSOINS1,PSOPI,PSOPRC,PSOSIG,PSOSITE)=0
 I VFDORD S PSOSITE=$$SITE^VFDPSUTL(VFDORD)
 I 'PSOSITE D  I 'PSOSITE Q
 . S X=$P($G(^XMB(1,1,"XUS")),U,17) S:X PSOSITE=$O(^PS(59,"D",X,""))
 . S:'PSOSITE VFDMSG="-1^Could not identify OUTPATIENT SITE"
 . Q
 I VFDORD="" S VFDMSG="-1^No Order number sent" Q
 I $G(^OR(100,VFDORD,0))="" S VFDMSG="-1^Order file entry not found" Q
 S VFDORG=$$GET1^DIQ(100,VFDORD_",",9)
 I VFDORG D EN^VFDPSORR Q:'$D(PSOORG)  ; no renew as orig script (#52) not found 
 K FDAX
 S PSODFN=+$$GET1^DIQ(100,VFDORD_",",.02,"I")  ; patient DFN
 S PSOCOM=$O(^VA(200,"B","COMMERCIAL,PHARMACY",""))
 I PSOCOM="" S VFDMSG="-1^COMMERCIAL,PHARMACY not setup in NEW PERSON (#200) File." Q
 ; DSS/rjs: Comment-out PSOSITE set here
 ;         PSOSITE has been defined in $$SITE^VFDPSUTL call above
 K PEND,ERR
 S PSOPEN=$O(^PS(52.41,"B",VFDORD,""))
 D GETS^DIQ(52.41,PSOPEN_",","**","I","PEND","ERR")
 I $D(ERR) S VFDMSG="-1^Error retrieving Pending Order" Q
 S PSODRG=+PEND(52.41,PSOPEN_",",11,"I")
 I 'PSODRG S VFDMSG="-1^Unidentified DRUG" Q  ;DSS/LM
 S PSODRUG("IEN")=PSODRG,PSODRUG("DEA")=$$GET1^DIQ(50,PSODRG_",",3)
 S PSOIEN=$G(PEND("52.41",""_PSOPEN_",","21","I"))
 I '$G(PSOIEN) S VFDMSG="-1^Unidentified RX" Q
 S PSORXN=$P(^PSRX(PSOIEN,0),"^",1)
 ;
 S PSODATE=$$NOW^XLFDT,PSODAT=$P(PSODATE,".",1)
 ;
 S VFDXX=$G(^PSRX(PSOIEN,0))
 ;DSS/RJS set the REFILL DATE fieldS
 I '$D(^PSRX(PSOIEN,1,0)) S PSORF=1
 I $D(^PSRX(PSOIEN,1,0)) S PSORF=$P(^PSRX(PSOIEN,1,0),"^",4)+1
 K DIC,DA,DR,DIE
 S DA(1)=PSOIEN,DIC="^PSRX(DA(1),"_"1,",(DA,DINUM,X)=PSORF,DIC(0)="L",DLAYGO=52 D FILE^DICN
 S $P(VFDRF,U,1)=PSODAT                        ; REFILL DATE
 S $P(VFDRF,U,2)=VFDPU                         ; MAIL/WINDOW/IN HOUSE
 S:$G(PEND(52.41,PSOPEN_",",12,"I")) $P(VFDRF,U,4)=PEND(52.41,PSOPEN_",",12,"I") ; QTY
 S:'$G(PEND(52.41,PSOPEN_",",12,"I")) $P(VFDRF,U,4)=$P(VFDXX,U,7)
 S $P(VFDRF,U,7)=+DUZ                          ; ENTERED BY
 S $P(VFDRF,U,8)=PSODATE                       ; LOGIN DATE
 S $P(VFDRF,U,9)=PSOSITE                       ; DIVISION
 S:$G(PEND(52.41,PSOPEN_",",101,"I")) $P(VFDRF,U,10)=PEND(52.41,PSOPEN_",",101,"I") ; DAYS SUPPLY
 S:'$G(PEND(52.41,PSOPEN_",",101,"I")) $P(VFDRF,U,10)=$P(VFDXX,U,8)
 S $P(VFDRF,U,11)=$$GET1^DIQ(50,PSODRG_",",404) ; UNIT PRICE OF DRUG
 S $P(VFDRF,U,17)=PEND(52.41,PSOPEN_",",5,"I")   ; PROVIDER
 ; DSS/JCH - VFD*15.0*17 Begin - Add RELEASED DATE/TIME to Refill multiple
 S $P(VFDRF,U,18)=PSODATE                      ; RELEASED DATE/TIME
 ; DSS/JCH - VFD*15.0*17 End
 S $P(VFDRF,U,8)=PSODAT                        ; DISPENSED DATE
 S VFDXX=$G(^PSRX(PSOIEN,0))
 S ^PSRX(PSOIEN,1,PSORF,0)=VFDRF
 S:($G(VFDISSUE)&$G(VFDSCAN)) $P(^PSRX(PSOIEN,1,PSORF,21600),U,1)=$G(VFDSCAN) ;DGR
 S $P(^PSRX(PSOIEN,1,PSORF,21600),U,2)=+DUZ
 S VFDNDC=$$NDC^VFDPSOR(PSODRG,VFDSCAN)
 S:$G(VFDNDC)'="" $P(^PSRX(PSOIEN,1,PSORF,1),U,3)=VFDNDC
 S:$D(^PSRX(PSOIEN,"EPH")) ^PSRX(PSOIEN,1,PSORF,"EPH")=+^PSRX(PSOIEN,"EPH")
 S $P(^PSRX(PSOIEN,3),U,1)=PSODATE              ; LAST DISPENSED DATE
 ; Store for Drug Accountability nightly processing
 S:'$D(^XTMP("PSA",+PSOSITE,+PSODRG,+PSODAT)) ^XTMP("PSA",+PSOSITE,+PSODRG,+PSODAT)=0
 S ^XTMP("PSA",+PSOSITE,+PSODRG,+PSODAT)=$G(^XTMP("PSA",+PSOSITE,+PSODRG,+PSODAT))+$P(VFDRF,U,4)
EX Q:'$G(PSOPEN)  S DA=+PSOPEN,DIK="^PS(52.41," D ^DIK
 Q
 ;
ISSUE(RET,RXN,ORD) ; RPC - VFDPSORF ABLE TO ISSUE REFILL
 ; INPUT:  RXN - Req - PRESCRIPTION File IEN in the form of '94R;0'
 ;         ORD - Req - ORDER File IEN
 ;
 ; OUTPUT: 1 if able to issue
 ;         0 if unable to issue
 ;         -1^Error otherwise
 ;
 S RXN=+$G(RXN),ORD=+$G(ORD),RET=1
 I 'RXN!('$D(^PSRX(RXN))) S RET="-1^No PRESCRIPTION found" Q
 I 'ORD!('$D(^OR(100,ORD))) S RET="-1^No ORDER found" Q
 I $$PICKUP(ORD)="W" S RET="0" Q
 I '$D(^PS(52.41,"ARF",RXN)) S RET=0 Q
 Q
 ;
 ; not written & pending refill return 1
 ; else return 0
PICKUP(ORD) ;
 N VFD,RET
 S RET=""
 S VFD=$O(^OR(100,ORD,4.5,"ID","PICKUP",""))
 I VFD S RET=$G(^OR(100,ORD,4.5,VFD,1))
 Q RET
