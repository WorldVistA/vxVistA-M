VFDPSOHX ;DSS/JCH - Convert Prescription Number to alternate numeral base ;September 7, 2015
 ;;15.0;DSS,INC VXVISTA OPEN SOURCE;**12**;07 September 2015;Build 8
 ;Copyright 1995-2015,Document Storage Systems Inc. All Rights Reserved
 ;
 Q
 ;
NEXTRX(RX) ; Get next RX based on VFD PSO RX NUMBERING base system
 N MAXLEN,MAXNUM,HEXRX,DECRX
 K NEWRX
 Q:RX="" ""
 ; Convert RX # to decimal
 S DECRX=$$RXTODEC(RX) Q:DECRX="" ""
 ; Increment RX #
 S DECRX=DECRX+1
 ; Convert RX # back to original base
 S NEWRX=$$DECTBASE(DECRX)
 Q NEWRX
 ;
DECTOHEX(DECRX) ; Convert decimal to hex
 N I,D S D=$G(DECRX)
 I '(D?1.N) Q DECRX  ; Hex # passed in - already in base 16. Cache thinks 10E1017D8 is exponential notation, blows up with <MAXNUMBER>
 S DECTOHEX="" F I=1:1 S DECTOHEX=$E("0123456789ABCDEF",D#16+1)_DECTOHEX,D=D\16 Q:'D
 Q DECTOHEX
 ;
HEXTODEC(HEXRX) ; Convert RX # hex to decimal
 N I,J
 S HEXTODEC=0 F I=1:1:$L(HEXRX) S J=$F("0123456789ABCDEF0123456789abcdef",$E(HEXRX,I)) Q:'J  S HEXTODEC=HEXTODEC*16+(J-2#16)
 Q HEXTODEC
 ;
RXTODEC(INRX,DFLTBAS) ; Convert RX # to decimal, from numbering system in ^VFD(21652, if RX already on file, or in VFD PSO RX NUMBERING parameter if new
 K PSOUTRX S PSOUTRX=""
 N PSORXIEN,PSORXBAS,RESULT,TAG,EXEC
 S PSORXBAS=$G(DFLTBAS)
 I PSORXBAS="" S PSORXBAS=$$GET^XPAR("SYS","VFD PSO RX NUMBERING") S:PSORXBAS="" PSORXBAS="DEC"
 I PSORXBAS="DEC" S PSOUTRX=+INRX
 I '$G(PSOUTRX) S TAG=PSORXBAS_"TODEC" I $L($T(@TAG)) S EXEC="S PSOUTRX=$$"_TAG_"(INRX)" X EXEC
 Q PSOUTRX
 ;
DECTBASE(INRX) ; Convert RX # from decimal to base numbering system in VFD PSO RX NUMBERING parameter
 K PSOUTRX
 N PSORXIEN,PSORXBAS,TAG,EXEC
 S PSORXBAS=$$GET^XPAR("SYS","VFD PSO RX NUMBERING") S:PSORXBAS="" PSORXBAS="DEC"
 I PSORXBAS="DEC" S PSOUTRX=+INRX
 I '$G(PSOUTRX) S TAG="DECTO"_PSORXBAS I $L($T(@TAG)) S EXEC="S PSOUTRX=$$"_TAG_"(INRX)" X EXEC
 I $G(PSOUTRX)=""!($G(PSOUTRX)=0) S PSOUTRX=INRX
 Q PSOUTRX
 ;
FIL21652(INRX,BASE) ; File new RX in ^VFD(21652 to track RX numbering system for each RX
 N FDA,MSG
 S FDA(21652,"?+1,",.01)=INRX  ; Newly generated RX #
 S FDA(21652,"?+1,",1)=BASE    ; Numeral base system of new RX
 D UPDATE^DIE("","FDA","","MSG")
 Q
 ;
CNVBOUND(BASE,CODE) ; Convert PRESCRIPTION # LOWER BOUND, PRESCRIPTION # UPPER BOUND, and LAST PRESCRIPTION # to new RX BASE
 ;  Called from Value Validation Code in VFD PSO RX NUMBERING parameter definition when value is edited
 ;  Input: BASE = The new RX Base, prior to being saved.
 ;
 Q:'($D(BASE)>1)
 N SITE,OLDBASE,TAG,EXEC,RX,X,Y
 S BASE=$G(CODE)
 S OLDBASE=$$GET^XPAR("SYS","VFD PSO RX NUMBERING") S:OLDBASE="" OLDBASE="DEC"
 Q:OLDBASE=$G(BASE)  ; No change
 ; Build conversion tag reference - quit if doesn't exist
 S TAG=OLDBASE_"TO"_BASE Q:'$L($G(TAG))
 ; Build executable string that sets converted value RX=$$<oldbase>_TO_<newbase>_(RX)
 S EXEC="S RX=$$"_TAG_"(RX)"
 S SITE=0 F  S SITE=$O(^PS(59,SITE)) Q:'SITE  D
 .N FDA,RESULT D GETS^DIQ(59,SITE,"1001;1002;1002.1;2001;2002;2003","","RESULT")
 .S RX=$G(RESULT(59,SITE_",",2001)) I RX]"" X EXEC S FDA(59,SITE_",",2001)=RX  ; RX # lower bound
 .S RX=$G(RESULT(59,SITE_",",2002)) I RX]"" X EXEC S FDA(59,SITE_",",2002)=RX  ; RX # upper bound
 .S RX=$G(RESULT(59,SITE_",",2003)) I RX]"" X EXEC S FDA(59,SITE_",",2003)=RX  ; RX # last issued
 .S RX=$G(RESULT(59,SITE_",",1001)) I RX]"" X EXEC S FDA(59,SITE_",",1001)=RX  ; Narcotic RX lower bound
 .S RX=$G(RESULT(59,SITE_",",1002)) I RX]"" X EXEC S FDA(59,SITE_",",1002)=RX  ; Narcotic RX upper bound
 .S RX=$G(RESULT(59,SITE_",",1002.1)) I RX]"" X EXEC S FDA(59,SITE_",",1002.1)=RX  ; Narcotic RX last issued
 .Q:$D(FDA)'>1
 .D FILE^DIE("","FDA","MSG")
 Q
