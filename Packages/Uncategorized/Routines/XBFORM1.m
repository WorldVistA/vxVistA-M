XBFORM1 ; IHS/ADC/GTH - sub x in output transforms  [ 02/07/97   3:02 PM ]
 ;;3.0;IHS/VA UTILITIES;;FEB 07, 1997
 ;
 ;XBV1=NEW CODE,XBLINX=original out transform
 Q
 ;
SUB(XBV1,XBLINX) ;EP extrensic to return new  output transform 
 D EN^XBNEW("XSUB^XBFORM1","XBV1;XBLINX")
 Q XBLINX
 ;
XSUB ;EP - do it
 NEW XB,XBT
 D SCAN
 I 'XBMK Q
 S XBLIN=XBLINX
 D BLDLIN1
 S XBLINX=XBLIN1
 Q
 ;
 ;----------------- SUB ROUTINES ---------------
 ;
SCAN ;EP - scan for X
 S XBVX="X"
 S XBP=" #&'()*+,'-/<=>@\_?;:[]!""",XBS=XBP
 S XBL=$L(XBVX)
 F XBI=1:1 S XB(XBI)=$F(XBLINX,XBVX,$G(XB(XBI-1))+1)-XBL Q:XB(XBI)'>0  D
 .S XB(XBI,"M")=0,XB(XBI,0)=XB(XBI)
 .I XBP[$E(XBLINX,XB(XBI)-1),XBS[$E(XBLINX,XB(XBI)+XBL) S XB(XBI,"M")=1
 .S XB("B",XB(XBI))=XBI,XB("E",XB(XBI)+XBL-1)=XBI
 .S XB(XBI,"E")=XB(XBI)+XBL-1
 .Q
 KILL XB(XBI)
CHKMK ;
 S XBMK="",XBJM=""
 F  S XBJM=$O(XB(XBJM)) Q:XBJM=""  I $G(XB(XBJM,"M")) S XBMK=1 Q
 KILL XBJM
SCANE ;
 Q
 ;
BLDLIN1 ;
 S XBLIN=XBLINX,XBV0="X"
 S XBLIN0=XBLIN,XBSUB=XBV0_":"_XBV1,XBLIN1=""
 F XBI=1:1 Q:'$D(XB(XBI))  S XBLIN1=XBLIN1_$E(XBLIN,$G(XB(XBI-1,"E"))+1,XB(XBI,0)-1)_$S(XB(XBI,"M"):XBV1,1:XBV0)
 S XBI=XBI-1 S XBLIN1=XBLIN1_$E(XBLIN,XB(XBI,"E")+1,999)
BLDLIN1E ;
 Q
 ;