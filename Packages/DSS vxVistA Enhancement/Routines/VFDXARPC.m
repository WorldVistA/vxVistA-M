VFDXARPC ;DSS/LM - External authentication of users;12/14/13  13:23; 1/6/10 7:28am
 ;;2013.1;VXVISTA DSS,INC;**14**;March 29, 2007;Build 1
 ;Copyright 1995-2014, Document Storage Systems, Inc., All Rights Reserved
 ;
 ; This routine contains implementing code for remote procedures
 ; associated with external user authentication components.
 ; See also routine VFDXAUTH.
 ;
 Q
VALIDAV(VFDRSLT,VFDCODES,VFDATTR) ;[RPC] - Remote procedure: VFD AUTH AV CODE
 ; Wraps XUS AV CODE. See return array description at VALIDAV^XUSRB.
 ;
 ; VFDATTR=[Optional] - Attributes relevant to external authentication
 ;                      Array subscripted (1), (2), etc. in the format
 ;                      VFDATTR(n) = Attribute name^Attribute Value.
 ;                      Attribute name may be case-sensitive.
 ;
 ; Initialize challenge reply and index VFDATTR array, if present
 N VFDCHLNG,VFDXUSTE S VFDCHLNG=0 D NDX(.VFDATTR)
 ; Indexed attribute array is accessible to external authentication routines
 ; Extract State attribute to variable VFDXUSTE, for convenience
 S:$D(VFDATTR("B","STATE")) VFDXUSTE=VFDATTR("B","STATE")
 D VALIDAV^XUSRB(.VFDRSLT,.VFDCODES)
 ;I $G(VFDATTR("B","SIM"))="CHALLENGE" D  ;Simulate challenge for testing
 ;.S VFDRSLT(0)=0,VFDRSLT(1)=99,VFDRSLT(3)="Re-enter verify code below..."
 ;.K VFDCHLNG ;Override any actual challenge in sim mode
 ;.Q
 Q:'$G(VFDCHLNG)  ;No challenge
 S VFDRSLT(1)=$P(VFDCHLNG,U),VFDRSLT(3)=$P(VFDCHLNG,U,2)
 S VFDRSLT(5)=1,VFDRSLT(6)=$P(VFDCHLNG,U,3)
 Q
NDX(VFDARY) ;[Private] - Create name index on first "^"-piece of array
 ;
 ; VFDARY=[By Reference] - Array to be indexed
 ;
 K VFDARY("B") N A,S,X S S="" F  S S=$O(VFDARY(S)) Q:S=""  D
 .S X=$G(VFDARY(S)),A=$P(X,"^") Q:'$L(A)
 .S VFDARY("B",A)=$P(X,"^",2,$L(X,"^"))
 .Q
 Q
CHLNG(VFDRSLT) ;[RPC] - Remote procedure: VFD AUTH CHALLENGE
 ; ****** This remote procedure is not currently used ******
 ;
 ; If a challenge was received from external authentication for
 ; the current process ID, return data associated with the
 ; last-recorded challenge for the process.
 ;
 ; Return format:
 ;        1 or 0^Username^Fileman Date/time^Reply Message^State
 ;
 ; where the first "^"-piece is 1 if a challenge was recorded or
 ; 0 if no challenge was recorded, and the remaining pieces are
 ; optional additional information
 ;
 N VFDH S VFDH=$O(^XTMP("VFDXAUTH",$J,""),-1)
 I VFDH="" S VFDRSLT=0 Q  ;No challenge
 N VFDCHLNG S VFDCHLNG=$G(^XTMP("VFDXAUTH",$J,VFDH))
 S VFDRSLT=$P(VFDCHLNG,U,1,2)_$$HTFM^XLFDT(VFDH)_"^"_$P(VFDCHLNG,"^",3,4)
 ; [LM] Comment-out next until state attribute handling is resolved
 ; K ^XTMP("VFDXAUTH",$J,VFDH) ;Delete after returning the record
 Q
