PSBOPM1 ;BIRMINGHAM/BSR-BCMA OIT HISTORY API ;Oct 2005
 ;;3.0;BAR CODE MED ADMIN;**17**;Mar 2004;Build 1
 ;;Per VHA Directive 2004-038, this routine should not be modified.
 ;
 ; Reference/IA
 ; FILE 53.79
 ; X-REF AOIP
 ; X-REF AOIP3
 ; X-REF AOIP4
 ;
 ;
GETORD(PSBORDNM) ;
 N XA,NDE
 S PSBORD=0,XA=PSBORDNM,PSBDT="",NDE=.1
 Q:PSBORDNM="" PSBORD
 Q:'$D(^PSB(53.79,"AOIP",DFN,XA)) PSBORD
 F  S PSBDT=$O(^PSB(53.79,"AOIP",DFN,XA,PSBDT)) Q:PSBDT=""  D
 .S PSBIEN="" F  S PSBIEN=$O(^PSB(53.79,"AOIP",DFN,XA,PSBDT,PSBIEN)) Q:PSBIEN=""  D
 ..Q:$P($G(^PSB(53.79,PSBIEN,0)),U,9)="N"
 ..Q:'$D(^PSB(53.79,PSBIEN,NDE))
 ..S PSBORD=$P(^PSB(53.79,PSBIEN,NDE),U)
 ..I PSBORD S PSBORDNM=PSBORD
 ..S:'PSBORD!(PSBORD="") PSBORD=0,TMP("PSBOIS",$J,XA)=""
 Q PSBORD
 ;
FINDIENS ; USE PSBOIS,PSBADDS AND PSBSOLS TO FIND ALL IENS FOR THE RPT
 ;SEARCH FOR UNIT DOSE IENS
 I $D(TMP("PSBOIS",$J)) S XA="" F  S XA=$O(TMP("PSBOIS",$J,XA)) Q:XA=""  D
 .S PSBDT="" F  S PSBDT=$O(^PSB(53.79,"AOIP",DFN,XA,PSBDT)) Q:PSBDT=""  D
 ..Q:PSBDT>PSBSTOP
 ..Q:PSBDT<PSBSTRT
 ..S PSBIEN="" F  S PSBIEN=$O(^PSB(53.79,"AOIP",DFN,XA,PSBDT,PSBIEN)) Q:PSBIEN=""  D
 ...Q:$P(^PSB(53.79,PSBIEN,0),U,9)="N"
 ...S TMP("PSBIENS",$J,"UD",$$GET1^DIQ(53.79,PSBIEN_",",.06,"I"),PSBIEN)=""
 ;
 ;SEARCH FOR ADDITIVES
 I $D(TMP("PSBADDS",$J)) S XA="" F  S XA=$O(TMP("PSBADDS",$J,XA)) Q:XA=""  D
 .S PSBIEN="" F  S PSBIEN=$O(^PSB(53.79,"AOIP3",DFN,PSBIEN)) Q:PSBIEN=""  D
 ..S XB="" F  S XB=$O(^PSB(53.79,"AOIP3",DFN,PSBIEN,XB)) Q:XB=""  D
 ...Q:XB'=XA
 ...Q:$P(^PSB(53.79,PSBIEN,0),U,9)="N"
 ...I $P(^PSB(53.79,PSBIEN,0),"^",6)>PSBSTRT,$P(^PSB(53.79,PSBIEN,0),"^",6)<PSBSTOP D
 ....S TMP("PSBIENS",$J,"ADD",$$GET1^DIQ(53.79,PSBIEN_",",.06,"I"),PSBIEN)=""
 ....S TMP("PSBADDS",$J,XA)=1
 ;
 ;SEARCH FOR SOLUTIONS
 I $D(TMP("PSBSOLS",$J)) S XA="" F  S XA=$O(TMP("PSBSOLS",$J,XA)) Q:XA=""  D
 .S PSBIEN="" F  S PSBIEN=$O(^PSB(53.79,"AOIP4",DFN,PSBIEN)) Q:PSBIEN=""  D
 ..S XB="" F  S XB=$O(^PSB(53.79,"AOIP4",DFN,PSBIEN,XB)) Q:XB=""  D
 ...Q:XB'=XA
 ...Q:$P(^PSB(53.79,PSBIEN,0),U,9)="N"
 ...I $P(^PSB(53.79,PSBIEN,0),"^",6)>PSBSTRT,$P(^PSB(53.79,PSBIEN,0),"^",6)<PSBSTOP D
 ....S TMP("PSBIENS",$J,"SOL",$$GET1^DIQ(53.79,PSBIEN_",",.06,"I"),PSBIEN)=""
 ....S TMP("PSBSOLS",$J,XA)=1
 Q
 ;
