VFDZARAVVXU1 ;DSS/PDW - GENERATE IMMUNIZATION MESSAGE ; 18 April 2011
 ;;2012.1.1;VENDOR - DOCUMENT STORAGE SYS;;24 Jun 2013;Build 136
 ;Copyright 1995-2013,Document Storage Systems Inc. All Rights Reserved
 ;DSS/BUILD - VFDHHL HL7 EXCHANGE 2012.1.1 * 06/23/15 * vxdev64v2k8_VX13 * 2012.1 T22
 ;DSS/BUILD - VFDHHL HL7 EXCHANGE 2012.1.1 * 02/02/14 * vxdev64v2k8_DEVXOS * 2012.1.1T21
VISIT ;entry to select visit and send Immunization file to Mirth/HFS
 ;
 N DIC,VFDVSTDA,Y,D,V,VFDTMP,VFDMSG,VFDRSLT
 N VFDENC,VFDG,VFDVST,HLA
ENTEST ; no news
 K DIC,VFDVSTDA,Y,D,V,VFDTMP,VFDMSG,VFDRSLT
 K VFDENC,VFDG,VFDVST,HLA
 W !,"This will allow selection of a visit and it's immunizations"
 W !,"will be transmitted to the config server folder C:\hfs\hl7\ARRA."
 S DIC=9000010,DIC(0)="AEQM",D="C"
 D IX^DIC
 I '+Y Q
 I +Y S VFDVSTDA=+Y
 D ENCEVENT^PXAPI(VFDVSTDA)
 S VFDENC=$NA(^TMP("PXKENC",$J)),VFDG=$NA(^TMP("PXKENC",$J,VFDVSTDA,"IMM"))
 I '$D(@VFDG) D  G VISIT
 .W !,"There are no Immunizations associated with this visit.",!
 W !!,"The following information will be sent:"
 K VFDTMP,VFDMSG
 D GETS^DIQ(9000010,VFDVSTDA,".01;.05;.08","IE","VFDTMP","VFDMSG")
 M VFDVST=VFDTMP(9000010,VFDVSTDA_",")
 W !,?5,"Patient:",?20,VFDVST(.05,"E")
 W !,?5,"Date/Time:",?20,VFDVST(.01,"E")
 W !,?5,"Clinic:",?20,VFDVST(.08,"E")
 W !!,?5,"Immunization(s)"
 S VFDVIMDA=0 F  S VFDVIMDA=$O(@VFDG@(VFDVIMDA)) Q:VFDVIMDA'>0  W !,?5,$$GET1^DIQ(9000010.11,VFDVIMDA,.01)
 K DIR S DIR(0)="Y",DIR("A")="Is the above correct?" D ^DIR
 I Y'=1 G VISIT
 S VFDVSTDA=+Y
 W !,"Transmitting HL7 message"
 D VSITSEND(VFDVSTDA,.VFDMSGID) ; return message ID
 W !,"Message Sent with ID: ",+VFDMSGID
 Q
VSITSEND(VFDVSTDA,VFDMSGID) ; send message return message ID
 ; the message HL7 v 1.6 lines are put into @VFDHLTMP@
 ; here it is a local array as messages are small
 N VFDVIM,VFDVIMS
 K VFDVIM,VFDVIMS
 S VFDHLTMP="HLA(""HLS"")"
 S VFDDFN=VFDVST(.05,"I")
 D PIDSEG
 ;S VFDVIMDA=0 F  S VFDVIMDA=$O(@VFDG@(VFDVIMDA)) Q:VFDVIMDA'>0  D RXASEG
 D VIMSFIND(.VFDVIM,VFDVSTDA)
 S VFDVIMDA=0 F  S VFDVIMDA=$O(VFDVIM(VFDVIMDA)) Q:VFDVIMDA'>0  D RXASEG
 ;the HLA array has been built
 D SEND(.VFDMSGID)
 Q
 ;PID
PIDSEG ;set variables, load segment array, load array into HL7 message
 N PID,VFDPIC,VFDPID
 D PIDVARS
 D ADDHL16^VFDHHLOT(VFDHLTMP,.VFDHL16) ;HL1.6
 K SEG
 Q
 ;
PIDVARS ; set variables for segment PID
 ; @VFDHLTMP@ is to be the HL16 total array location is 'REQUIRED'
 ;Sequence  Name                             Type      Variable          Rep Req
 ;1.0.0.0   Set ID - PID "0001"              ST        VFDPID("SEQ")         
 ;2.1.0.0   ID                               ST        VFDPID("PTID")        
 ;3.1.0.0   ID                               ST        VFDPID("PATID")       Y
 ;3.4.1.0   namespace ID 300                 MPI*      VFDPID("ASGAUTH")     Y
 ;3.4.2.0   universal ID                     ST        VFDPID("ASGID")       Y
 ;3.4.3.0   universal ID type 301            ISO*      VFDPID("ASGGRP")      Y
 ;3.5.0.0   identifier type code 203         MR*       VFDPID("<L>")         Y
 ;5.1.0.0   family name                      ST        VFDPID("LNAME")       Y
 ;5.2.0.0   given name                       ST        VFDPID("FNAME")       Y
 ;7.0.0.0   Date/Time Of Birth               TS        VFDPID("PTDOB")       Y
 ;8.0.0.0   0001 Administrative Sex          0001      VFDPID("PTSEX")       Y
 ;10.1.0.0  RACE identifier                  0005      VFDPID("RACEID")      Y
 ;10.2.0.0  text                             ST        VFDPID("RACETXT")     Y
 ;10.3.0.0  name of coding system            396       VFDPID("RACESYS")     Y
 ;11.1.0.0  (sad) street address             ST        VFDPID("PTADDLINE1")  Y
 ;11.2.0.0  other designation                ST        VFDPID("PTADDLINE2")  
 ;11.3.0.0  city                             ST        VFDPID("PTADDCITY")   Y
 ;11.4.0.0  state or province                ST        VFDPID("PTADDSTATE")  Y
 ;11.5.0.0  zip or postal code               ST        VFDPID("PTADDZIP")    Y
 ;11.7.0.0  (190) address type               ST        VFDPID("PTADDTYP")    
 ;13.1.0.0  telephone any text               ST        VFDPID("PHONE#")      
 ;13.2.0.0  telecommunication use code       ST        VFDPID("PHONECD")     Y
 ;13.3.0.0  (202) tele equip type            PH*       VFDPID("PHONETYPE")   Y
 ;13.6.0.0  area/city code                   NM        VFDPID("PHONEAREA")   Y
 ;13.7.0.0  phone number                     NM        VFDPID("PHONELOCAL")  Y
 ;22.1.0.0  (189) identifier                 N*        VFDPID("ETHNICGRP")   Y ;22.2.0.0  text                             Not Hispanic or Latino*VFDPID("ETHNICTXT")                                                                      Y
 ;22.3.0.0  (396) name of coding system      HL70189*  VFDPID("ETHNICSYS")   Y
 ;29.0.0.0  Patient Death Date and Time      TS        VFDPID("PTDOD")       
 ;30.0.0.0  Patient Death Indicator          ST        VFDPID("DODIND")      
 ;Use @VAR@("HL")=VALUE to bypass FM to HL7 transform
 ;
 N VFDELM,VFDPTPID,VFDPIDSG,VFDPID,VFDSGMNT,VFDSEG,VFDE,VFDC,VFDS
 K VFDELM,VFDPTPID,VFDPIDSG,VFDPID,VFDSGMNT,VFDSEG,VFDE,VFDC,VFDS
 D EN^VFDHLPID(.VFDPIDSG,VFDDFN,,,,"S")
 D PARSE^VFDHHLOT(.VFDPTPID,VFDPIDSG,1) ;HL7.e.c.s subscripting
 F VFDELM=1,2,4,6,9,12 K VFDPTPID(VFDELM)
 F VFDELM=14:1:21 K VFDPTPID(VFDELM)
 S VFDE=11 F VFDC=2,6 K VFDPTPID(VFDE,1,VFDC)
 S VFDE=3 F VFDC=2,3 K VFDPTPID(VFDE,1,VFDC)
 F I=2:1:5 K VFDPTPID(3,I)
 S VFDPTPID(3,1,4,1)="MPI"
 S VFDPTPID(3,1,4,2)=+$$SITE^VASITE_"."_VFDDFN
 S VFDPTPID(3,1,4,3)="ISO"
 S VFDPTPID(3,1,5,1)="MR"
 S VFDPTPID(10,1,3,1)="HL70005"
 S VFDPTPID(13,1,6,1)=$E($G(VFDPTPID(13,1,1,1)),1,3)
 S VFDPTPID(13,1,7,1)=$E($G(VFDPTPID(13,1,1,1)),4,10)
 K VFDPTPID(13,1,1,1)
 S VFDPTPID(22,1,1,1)="N"
 S VFDPTPID(22,1,2,1)="Not Hispanic or Latino"
 S VFDPTPID(22,1,3,1)="HL70189"
 ;step 'D SEGSET^VFDHHLOT(' skipped here as array already exists
 M VFDSEG=VFDPTPID ;move working array into position for call to build^
 D BUILD^VFDHHLOT(.VFDSGMNT,.VFDSEG,1) ; assemble array VFDSEG into one VFDSGMNT
 D SPLIT^VFDHHLOT(.VFDHL16,VFDSGMNT) ; split VFDSGMNT into 240 lengths elements
 Q
 ;RXA
RXASEG ;set variables, load segment array, load array into HL7 message
 N VFDRXA
 D RXAVARS
 D ADDHL16^VFDHHLOT(VFDHLTMP,.VFDHL16) ;HL1.6
 K SEG
 Q
 ;
RXAVARS ; set variables for segment RXA
 ; @VFDHLTMP@ is to be the HL16 total array location is 'REQUIRED'
 ;Sequence  Name                             Type      Variable          Rep Req
 ;1.0.0.0   Give Sub-ID Counter              0*        VFDRXA("GVNSUBIDCNT") Y
 ;2.0.0.0   Administration Sub-ID Counter    1*        VFDRXA("ADMSUBIDCNT") Y
 ;3.0.0.0   Date/Time Start of Administrat   TS        VFDRXA("ADMDTMBEG")   Y
 ;4.0.0.0   Date/Time End of Administratio   TS        VFDRXA("ADMDTMEND")   Y
 ;5.1.0.0   (292) identifier                 292       VFDRXA("ADMCDID")     Y
 ;5.2.0.0   text                             ST        VFDRXA("ADMCDTXT")    Y
 ;5.3.0.0   (396) name of coding system      ST        VFDRXA("ADMCDSYS")    Y
 ;6.0.0.0   Administered Amount              NM        VFDRXA("AMOUNT")      Y
 ;7.1.0.0   identifier                       ST        VFDRXA("UNITID")      Y
 ;7.2.0.0   text                             ST        VFDRXA("UNITTXT")     Y
 ;7.3.0.0   (396) name of coding system      ST        VFDRXA("UNITSYS")     Y
 ;15.0.0.0  Substance Lot Number             ST        VFDRXA("LOTNUM")      Y
 ;17.1.0.0  identifier                       ST        VFDRXA("MFGID")       Y
 ;17.2.0.0  text                             ST        VFDRXA("MFGTXT")      Y
 ;17.3.0.0  (396) coding system              MVX*      VFDRXA("MFGSYS")      Y
 ;21.0.0.0  (323) Action Code-RXA            A*        VFDRXA("ACTCD")       
 ;Use @VAR@("HL")=VALUE to bypass FM to HL7 transform
 ;
 ;arrays used
 ;VFDVIM     V Immunizations
 ;VFDVFD     VFD Immunization
 ;VFDROW     Row from VX HL7x tables
 ;Find related file IENS
 ;S VFDVIMDA ; passed in partition
 N VFDTMP,VFDMSG,VFDVIM,VFDVSTDA,VFDVST,VFDVFDDA,VFDSEG,VFDRXA
 N VFDTABIS,VFDROWDA,VFDROW,VFDROWIS,VFDMFG,SEG
 N VFGD227,VFD227DA,VF227GIS,VFD292,VFD292DA,VFDV292IS
TEST ;
 K VFDTMP,VFDMSG,VFDVIM,VFDVSTDA,VFDVST,VFDVFDDA,VFDSEG,VFDRXA
 K VFDTABIS,VFDROWDA,VFDROW,VFDROWIS,VFDMFG,SEG,VFDMFG
 K VFDMFG,VFDMFGDA,VFDMFGIS,VFDVAX,VFDVAXDA,VFD292IS
 ;pull V IMMU data
 D GETS^DIQ(9000010.11,VFDVIMDA,".01;.02;.03;21600.01;21600.02;21600.03","I","VFDTMP","VFDMSG")
 ;VFDTMP(9000010.11,"27,",.01,"I")=19
 M VFDVIM=VFDTMP(9000010.11,VFDVIMDA_",")
 ;S VFDRXA("GVNSUBIDCNT")="" ; static '0'
 ;S VFDRXA("ADMSUBIDCNT")="" ; static '1'
 S VFDVSTDA=VFDVIM(.03,"I")
 S VFDRXA("ADMDTMBEG")=$$GET1^DIQ(9000010,VFDVSTDA,.01,"I")
 S VFDRXA("ADMDTMEND")=VFDRXA("ADMDTMBEG")
 ;pull values from VX0292 vacines table with IEN of IM file
 S VFDIMDA=VFDVIM(.01,"I")
 D TABPATH^VFDHHLOT(.VFD292IS,"ARRA","VX0292") ;VFDTBLIS="21625.0106^174,4,"
 S VFD292IS=","_$P(VFD292IS,U,2)
 K VFDMSG,VFDTMP,VFD292
 S (VFDFILE,VFDIENS,VFDFLGS,VFDVAL,VFDINDX,VFDSCR,VFDMSG)=""
 S VFDMSG="VFDMSG",VFDVAL=VFDIMDA,VFDTMP="VFDTMP"
 S VFDFILE=21625.0106,VFDIENS=VFD292IS,VFDVAL=VFDIMDA,VFDINDX="D"
 S VFD292DA=$$FIND1^DIC(VFDFILE,VFDIENS,VFDFLGS,VFDVAL,"D",VFDSCR,VFDMSG)
 ;N VFDHLVAL S VFDHLVAL=$$TRANS^VFDHHLOT("ARRA","VX0292",VFDIMDA,"HL")
 ;D TABVALDA^VFDHHLOT(.VFD292DA,"ARRA","VX0292",VFDHLVAL) ;ZW VFDIMDA,VFDHLVAL,VFD292DA
 I 'VFD292DA S VFD292(.02)=VFDVAL_" not found VX0292" G NO292IMM
 S VFD292IS=VFD292DA_VFD292IS
 D GETS^DIQ(21625.0106,VFD292IS,".01;.02;.05",,VFDTMP,VFDMSG)
 M VFD292=VFDTMP(21625.0106,VFD292IS)
NO292IMM ; immunization not mapped
 S VFDRXA("ADMCDID")=VFDIMDA
 S VFDRXA("ADMCDTXT")=$G(VFD292(.02))
 S VFDRXA("ADMCDSYS")=$G(VFD292(.05))
UNIT D UNITS
 I '+VFDVIM(21600.01,"I") G NODOSE
 I +VFDVIM(21600.01,"I")=999 G NODOSE
 K VFDTMP,VFDMSG,VFDVFD
 S VFDTMP="VFDTMP",VFDMSG="VFDMSG"
 S VFDVFDDA=$G(VFDVIM(21600.03,"I"))
 D GETS^DIQ(21630.01,VFDVFDDA,".01;.02;.03;.04;1;1.1",,VFDTMP,VFDMSG)
 M VFDVFD=VFDTMP(21630.01,VFDVFDDA_",")
 S VFDRXA("LOTNUM")=VFDVFD(.02)
 S VFDRXA("MFGTXT")=VFDVFD(.03)
 ;use MFGTXT, VFDVFD(.03) to lookup in VX0227 table
 D TABPATH^VFDHHLOT(.VFD227IS,"ARRA","VX0227") ;VFDTBLIS="21625.0106^174,4,"
 S VFD227IS=","_$P(VFD227IS,U,2)
 S (VFDFILE,VFDIENS,VFDFLGS,VFDVAL,VFDINDX,VFDSCR,VFDMSG)=""
 S VFDFILE=21625.0106,VFDIENS=VFD227IS,VFDINDX="C"
 K VFDMSG,VFDTMP,VFD227
 S VFDMSG="VFDMSG",VFDTMP="VFDTMP"
 S VFDVAL=VFDVFD(.03)
 S VFD227DA=$$FIND1^DIC(VFDFILE,VFDIENS,VFDFLGS,.VFDVAL,.VFDINDX,.VFDSCR,VFDMSG)
 I 'VFD227DA S VFD227(.01)=VFDVAL_" not found: VX0227"
 S VFD227IS=VFD227DA_VFD227IS
 D GETS^DIQ(21625.0106,VFD227IS,".01;.02;.05",,VFDTMP,VFDMSG)
 M VFD227=VFDTMP(21625.0106,VFD227IS)
NO227MFG ;
 S VFDRXA("MFGID")=$G(VFD227(.01))
 S VFDRXA("MFGSYS")=$G(VFD227(.05))
 ;S VFDRXA("ACTCD")=""
NODOSE ; if no dose no units, no manufacturer
 K VFDSEG
 D SEGSET^VFDHHLOT("ARRA","RXA",.VFDSEG)
 N VFDSGMNT
 D BUILD^VFDHHLOT(.VFDSGMNT,.VFDSEG) ; assemble array VFDSEG into one VFDSGMNT
 D SPLIT^VFDHHLOT(.VFDHL16,VFDSGMNT) ; split VFDSGMNT into 240 length elements
 Q
UNITS ; per RXA.6 set RXA.7 items
 I +VFDVIM(21600.01,"I"),(+VFDVIM(21600.01,"I")'=999) D  I 1
 . S VFDRXA("AMOUNT")=VFDVIM(21600.01,"I")
 . ;all ARRA is ml^milliliter^ISO+
 . S VFDRXA("UNITID")="ml"
 . S VFDRXA("UNITTXT")="milliliter"
 . S VFDRXA("UNITSYS")="ISO+"
 E  S VFDRXA("AMOUNT")=999
 Q
SEND(VFDRSLT) ; generate HL message
 N VFDFILE,VFDIENS,VFDFLGS,VFDVAL,VFDINDX,VFDSCR,VFDMSG
 S VFD101NM="VFDH ARRA VXU-V04 SERVER"
 S (VFDFILE,VFDIENS,VFDFLGS,VFDVAL,VFDINDX,VFDSCR,VFDMSG)=""
 S VFDFILE=101,VFDVAL=VFD101NM,VFDMSG="VFDMSG"
 S VFD101DA=$$FIND1^DIC(VFDFILE,VFDIENS,VFDFLGS,.VFDVAL,.VFDINDX,.VFDSCR,VFDMSG)
 N VFDDIR,VFDHFSNM
 D INIT^HLFNC2(VFD101DA,.HL)
 ;build HLA("HLS",I)
 K HLA("HLS",0)
 D GENERATE^HLMA(VFD101DA,"LM",1,.VFDRSLT)
 D HFSSEND(+VFDRSLT) ;send to HFS directory in XPAR
 Q
VIMSFIND(VFDVIM,VFDVSTDA) ;
 N VFDVIMS,VFDVIMDA
 S (VFDFILE,VFDIENS,VFDFLDS,VFDFLGS,VFDNUM,VFDFR,VFDPART,VFDNDX,VFDSCR,VFDIDENT,VFDTAR,VFDMSG)=""
 ;Set needed variables
 S VFDFILE=9000010.11,VFDFLDS="@;.01I",VFDFLGS="P"
 S VFDSCR="I $P(^(0),U,3)="_VFDVSTDA
 K VFDTAR,VFDMSG,VFDVALS,VFDVIMS
 D LIST^DIC(VFDFILE,VFDIENS,VFDFLDS,VFDFLGS,VFDNUM,VFDFR,VFDPART,VFDNDX,VFDSCR,VFDIDENT,"VFDTAR","VFDMSG")
 M VFDVIMS=VFDTAR("DILIST")
 S VFDVIMDA=0 F  S VFDVIMDA=$O(VFDVIMS(VFDVIMDA)) Q:VFDVIMDA'>0  S VFDVIM(+VFDVIMS(VFDVIMDA,0))=""
 Q
HFSSEND(VFDRSLT) ;call from VFDHHLD2; get MSG from  773,772; vfdrslt=MID of 773
 ;uses VFDHLRST
 K VFDMSG
 Q:'+VFDRSLT
 N VFDAPDA,VFDMSGDA,VFDIENS,VFDPRDA,VFDERR,VFDMSG
 S VFDMSGDA=$$FIND1^DIC(773,,"X",+VFDRSLT,"C")
 N VFDHFS,VFDMSH,VFDIENS,VFDTMP,VFDTMP1,VFDTMP2,VFDTMP3
 S VFDIENS=VFDMSGDA_","
 D GETS^DIQ(773,VFDIENS,200,,"VFDTMP","VFDERR")
 S VFDMSH=VFDTMP(773,VFDIENS,200,1)
 S VFDTIENS=$$GET1^DIQ(773,VFDIENS,.01,"I")_","
 K VFDTMP,VFDERR
 D GETS^DIQ(772,VFDTIENS,200,,"VFDTMP1","VFDERR")
 M VFDTMP2=VFDTMP1(772,VFDTIENS,200) ;VFDTMP2(1,0)="MSH
 ;N VFDLN,VFDMSG,VFDMID,VFDHFSNM,VFDDIR
GATHER ;restore the 1.6 storage to full segments
 N VFDEVNTY,%,VFDDIR
 S VFDLN=1
 S VFDMSG(1,0)=VFDMSH
 F %=1:1 Q:'$D(VFDTMP2(%))  S VFDLN=VFDLN+1 S VFDMSG(VFDLN,0)=VFDTMP2(%) D
 . I '$D(VFDTMP2(%+1)) Q
 . I $G(VFDTMP2(%+1))="" S %=%+1 Q
 . F  S VFDMSG(VFDLN,0)=VFDMSG(VFDLN)_VFDTMP2(%+1),%=%+1 Q:($G(VFDTMP2(%+1))="")
 K:$G(VFDMSG(VFDLN,0))="" VFDMSG(VFDLN)
 S VFDMID=$P(VFDMSH,"|",10),VFDEVNTY=$P($P(VFDMSH,"|",9),U,3)
 D NOW^%DTC
 ;S VFDHFSNM=VFDMID_".TXT"
 S VFDHFSNM=VFDEVNTY_"_"_%_".TXT"
 S VFDDIR=$$GET^XPAR("SYS","VFDHHL HOST FILE DIRECTORY")
GTF ;message is in array VFDMSG(n,0)
 S X=$$GTF^%ZISH($NA(VFDMSG(1,0)),1,VFDDIR,VFDHFSNM) ;**
 D ^%ZISC
 W !!,"File sent: ",VFDDIR,"\",VFDHFSNM,!!
 S VFDE=$O(VFDMSG(""),-1)
 F I=1:1:VFDE W !,VFDMSG(I,0)
 D E
 Q
E N DIR S DIR(0)="EO",DIR("A")="<CR> - Continue" D ^DIR
 Q
HISTORY ;
 ;3 MAY 11 bullet proof NO227MFG,NO292IMM mappings
 ;5 May 11 VFD292DA= code changed as FIND1^DIC not working
