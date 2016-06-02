VFDPSOU1 ;DSS/AMC - Utilities for RxPad ;08/11/14
 ;;15.0;DOCUMENT STORAGE SYS;**9**;02 Oct 2013;Build 5
 ;Copyright 1995-2014,Document Storage Systems Inc. All Rights Reserved
 ;
IT ;input transform File 52 Prescription, Field 21600.01 Precription pad #
 ; ZEXCEPT: X from Fileman
 I $G(X)="" K X Q  ;--> no X
 I X?1"^".E Q  ;^ allowed
 I $E($P($$SITE^VASITE,U,3),1,3)=111 D
 .I $L(X)'=8 K X Q
 .I X'?1N5UN2N K X Q  ;--> lower case or symbol
 .N LTR F LTR="A","E","I","O","U"," " I X[LTR K X Q
 .Q:'$D(X)
 .; I '+X K X Q  ;  VFDPSORXPAD*2013.1*1
 .I X="99999999" K X Q  ;
 E  I $L(X)>30 K X
 I $D(X),$D(^PSRX("AVFDRXP",X)) D  K X Q
 .. N RXIEN S RXIEN=$O(^(X,""))  ; **NAKED**
 .. N RXNUM S RXNUM=$P(^PSRX(RXIEN,0),U)
 .. D EN^DDIOL(" ")
 .. D EN^DDIOL("This number is a same Rx Pad Number as Rx #: "_RXNUM)
 .. D EN^DDIOL("Prescription Pad Numbers must be unique.")
 Q  ; serial number verified
 ;
 ;called by xecutable help
PAD ;help text PRESCRIPTION FILE fld #21600.01
 N PSOHLP
 D FULL^VALM1
 I $E($P($$SITE^VASITE,U,3),1,3)=111 D
 .S PSOHLP(1)="Requirements for NY State Issued Prescription Pad Number:"
 .S PSOHLP(2)=""
 .S PSOHLP(3)="Prescription Pad Numbers MUST be entered for Controlled Substances."
 .S PSOHLP(4)="Prescription Pad Numbers are unique and must be 8 characters long."
 .S PSOHLP(5)="They have letters and numbers only -- no symbols."
 .S PSOHLP(6)="Capitals only with no 'A'-'E'-'I'-'O' or 'U's."
 .S PSOHLP(7)="No 99999999, ZZZZZZZZ, 00000000 or blanks."
 .S PSOHLP(8)="First, seventh and eight digits must be numeric."
 E  D
 .S PSOHLP(1)="Prescription Pad Numbers must be 1-30 characters"
 D WRT
 ; ZEXCEPT: VALMBCK - for Listman
 S VALMBCK="R"
 Q
WRT ;Calls EN^DDIOL to write text
 ; ZEXCEPT: PSOHLP from above
 D EN^DDIOL(.PSOHLP) K PSOHLP
 Q
ZZ N DIR S DIR(0)="52,21600.01" D ^DIR Q
RXPAD(X) ;Determine if a Prescription Pad # exists for the RX IEN passed
 Q:'$G(X) 0
 Q $P($G(^PSRX(X,21600)),U,2)]""
DEACHK() ; Check if CS; also, disable check if check parameter isn't turned on.
 ; ZEXCEPT: PSODRUG pharmacy variable
 I '$$GET^XPAR("ALL","VFD PSO CS RXPAD#") Q 0  ; No RxPad number necessary (NO = 0)
 Q 2345[+$G(PSODRUG("DEA"))
 ;
POST ; KIDS EP to set default value of parameter for package for OMH to override what KIDS sends by default
 N STATION S STATION=$P($$SITE^VASITE(),U,3)
 I $E(STATION,1,3)=111 D  ; NYS OMH
 . D PUT^XPAR("PKG","VFD PSO CS RXPAD#",,1) QUIT
 . I '$$GET^XPAR("PKG","VFD PSO CS RXPAD#") D MES^XPDUTL("Couldn't set parameter for OMH. Contact support.")
 E  D
 . D PUT^XPAR("PKG","VFD PSO CS RXPAD#",,0) QUIT
 QUIT
 ;
VXRX() ;
 N RET S RET=0
 I $G(^%ZOSF("ZVX"))["VX",$$DEACHK S RET=1
 Q RET
 ;
PARAM() ; Check Parameter VFD PSO CS RXPAD#
 I $$GET^XPAR("ALL","VFD PSO CS RXPAD#") Q 1
 Q 0
