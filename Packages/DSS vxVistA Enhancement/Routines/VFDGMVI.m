VFDGMVI ;DSS/LM - vxVistA Vitals RPCs ;March 18, 2015
 ;;2013.1;DSS,INC VXVISTA OPEN SOURCE;**2,33**;11 Jun 2013;Build 2
 ;Copyright 1995-2015,Document Storage Systems Inc. All Rights Reserved
 ;
 Q
GET(VFDRSLT,VFDFN,VFDVDA,VFDQDA,VFDRNG) ;VFD GET VITALS MEASUREMENTS
 ;
 ; VFDFN=[Required] Patient IEN
 ; VFDVDA=[Required] Vital Type IEN
 ; VFDQDA=[Optional] Qualifier IEN(s)
 ; VFDRNG=[Optional] Begin^End date (inclusive)
 ;
 ; VFDRSLT=Results array, subscripted 1, 2, 3, ...
 ;         VFDRSLT(I)=Date/time vitals taken (FM internal)^internal^external
 ;         Excludes results entered in error
 ;
 I $G(VFDFN)>0 S VFDFN=+VFDFN
 E  S VFDRSLT(1)="-1^Missing or invalid DFN" Q
 I $G(VFDVDA)>0 S VFDVDA=+VFDVDA,VFDQDA=$G(VFDQDA),VFDRNG=$TR($G(VFDRNG)," ")
 E  S VFDRSLT(1)="-1^Missing or invalid vital type IEN" Q
 I $L($G(VFDQDA)) N VFD,VFDA,VFDEND,VFDI,VFDK,VFDMLST,VFDT,VFDQI,VFDQQ,VFDQX,VFDZ
 F VFDI=1:1 S VFD=$P(VFDQDA,U,VFDI) Q:VFD=""  D
 .S VFDQX(VFD)=VFDI ;Cross-reference qualifiers
 .Q
 ; 7/9/2010 - Accommodate computed dates for beginning and end of range
 S VFDBEG=$P(VFDRNG,U),VFDEND=$P(VFDRNG,U,2)
 I VFDBEG]"",'(VFDBEG=+VFDBEG) S VFDBEG=$$VDT(VFDFN,VFDBEG)
 I VFDEND]"",'(VFDEND=+VFDEND) S VFDEND=$$VDT(VFDFN,VFDEND)
 I VFDBEG<0!(VFDEND<0) S VFDRSLT(1)="-1^Error in date specification" Q
 S:VFDEND&'(VFDEND[".") VFDEND=VFDEND_".24"
 S VFDT="",VFDK=0
 F  S VFDT=$O(^GMR(120.5,"AA",VFDFN,VFDVDA,VFDT)) Q:VFDT=""!(9999999-VFDT<VFDBEG)!(VFDK&'VFDBEG)  D
 .I VFDEND,9999999-VFDT>VFDEND Q  ;Exclude if after specified end date
 .;DSS/LM - Accommodate possible multiple measurements for same date/time
 .;S VFDA=$O(^(VFDT,0)) S VFDZ=$G(^GMR(120.5,VFDA,0))
 .;Q:'($P(VFDZ,U,3)=VFDVDA)  ;Match to Vital Type
 .;Q:$P($G(^GMR(120.5,VFDA,2)),U)  ;Exclude if "entered in error"
 .;S (VFDQI,VFDQQ)=0 F  Q:VFDQQ  S VFDQI=$O(VFDQX(VFDQI)) Q:'VFDQI  D  ;Qualifiers
 .;.Q:$D(^GMR(120.5,VFDA,5,"B",VFDQI))  S VFDQQ=1
 .;.Q
 .;Q:VFDQQ  ;Failure to match ALL specified qualifiers
 .;S VFDK=VFDK+1,VFDRSLT(VFDK)=VFDA_U_$P(VFDZ,U)_U_$P(VFDZ,U,8) ;T1 - Add VFDA
 .;S VFDRSLT(VFDK)=VFDRSLT(VFDK)_U_$$QUAL(VFDA) ; T1 - Add qualifiers list
 .S VFDA=0 F  S VFDA=$O(^GMR(120.5,"AA",VFDFN,VFDVDA,VFDT,VFDA)) Q:'VFDA  D
 ..S VFDZ=$G(^GMR(120.5,VFDA,0))
 ..Q:'($P(VFDZ,U,3)=VFDVDA)  ;Match to Vital Type
 ..Q:$P($G(^GMR(120.5,VFDA,2)),U)  ;Exclude if "entered in error"
 ..S (VFDQI,VFDQQ)=0 F  Q:VFDQQ  S VFDQI=$O(VFDQX(VFDQI)) Q:'VFDQI  D  ;Qualifiers
 ...Q:$D(^GMR(120.5,VFDA,5,"B",VFDQI))  S VFDQQ=1
 ...Q
 ..Q:VFDQQ  ;Failure to match ALL specified qualifiers
 ..S VFDK=VFDK+1,VFDRSLT(VFDK)=VFDA_U_$P(VFDZ,U)_U_$P(VFDZ,U,8) ;T1 - Add VFDA
 ..S VFDRSLT(VFDK)=VFDRSLT(VFDK)_U_$$QUAL(VFDA) ; T1 - Add qualifiers list
 .;DSS/LM - End modification 7/27/2013
 .Q
 S:'$D(VFDRSLT(1)) VFDRSLT(1)="-1^No qualifying measurements found"
 Q
QUAL(VFDA) ;[Private] List of qualifiers associated with GMRV VITAL MEASUREMENT
 ; VFDA=[Required] File 120.5 entry
 ;
 I $G(VFDA)>0,VFDA?1.N N VFDJ,VFDX,VFDY S (VFDJ,VFDX,VFDY)=""
 E  Q ""
 F  S VFDJ=$O(^GMR(120.5,VFDA,5,"B",VFDJ)) Q:VFDJ=""  D
 .S VFDX=$P($G(^GMRD(120.52,VFDJ,0)),U) Q:'$L(VFDX)
 .S:$L(VFDY) VFDY=VFDY_"," S VFDY=VFDY_VFDX
 .Q
 Q VFDY
 ;
VDT(VFDFN,VFDT) ;[Private] Date/time resolution
 ;
 ; VFDFN=[Required] Patient IEN
 ; VFDT= [Required] Fileman date.time + optional qualifiers
 ;                  '+' and '-' mean add or subtract
 ;                  DOB=patient date of birth, DOD=patient date of death,
 ;                      D=days, W=weeks, M=months, Y=years,
 ;                      h = hours, m=minutes, s=seconds
 ;
 I $G(VFDFN)>0 S VFDT=$G(VFDT) N VFDF,VFDL,VFDX
 E  Q "" ;Patient IEN is required
 I VFDT=""!(VFDT=+VFDT) Q VFDT
 ;
 S VFDL=$L(VFDT) ;VFDT is not empty and not canonic...
 ;
 S VFDF=$F(VFDT,"DOB") I VFDF D  Q $$VDT(VFDFN,VFDT) ;Patient date-of-birth
 .S VFDT=$E(VFDT,1,VFDF-4)_$$GET1^DIQ(2,VFDFN,.03,"I")_$E(VFDT,VFDF,VFDL)
 .Q
 S VFDF=$F(VFDT,"DOD") I VFDF D  Q $$VDT(VFDFN,VFDT) ;Patient date-of-death
 .S VFDT=$E(VFDT,1,VFDF-4)_$$GET1^DIQ(2,VFDFN,.351,"I")_$E(VFDT,VFDF,VFDL)
 .Q
 ;
 S VFDF=$F(VFDT,"TODAY") I VFDF D  Q $$VDT(VFDFN,VFDT) ;TODAY
 .S VFDT=$E(VFDT,1,VFDF-6)_$$DT^XLFDT_$E(VFDT,VFDF,VFDL)
 .Q
 S VFDF=$F(VFDT,"T") I VFDF D  Q $$VDT(VFDFN,VFDT) ;T (synonym of TODAY)
 .S VFDT=$E(VFDT,1,VFDF-2)_$$DT^XLFDT_$E(VFDT,VFDF,VFDL)
 .Q
 S VFDF=$F(VFDT,"NOW") I VFDF D  Q $$VDT(VFDFN,VFDT) ;NOW
 .S VFDT=$E(VFDT,1,VFDF-4)_$$NOW^XLFDT_$E(VFDT,VFDF,VFDL)
 .Q
 S VFDF=$F(VFDT,"N") I VFDF D  Q $$VDT(VFDFN,VFDT) ;N (synonym of NOW)
 .S VFDT=$E(VFDT,1,VFDF-2)_$$NOW^XLFDT_$E(VFDT,VFDF,VFDL)
 .Q
 ;
 N VFDA,VFDB,VFDC,VFDI,VFDO,VFDX,VFDY,VFDZ S VFDZ=$E(VFDT,VFDL)
 I "DWMYhms"[VFDZ D  Q VFDY
 .S VFDX="" F VFDI=VFDL-1:-1 S VFDC=$E(VFDT,VFDI) Q:"+-"[VFDC  S VFDX=VFDC_VFDX
 .S VFDO=VFDC
 .S:VFDZ="Y" VFDX=12*VFDX,VFDZ="M" ;Convert YEARS to MONTHS
 .S:VFDZ="W" VFDX=7*VFDX,VFDZ="D" ;Convert WEEKS to DAYS
 .S:VFDZ="m" VFDX=60*VFDX,VFDZ="s" ;Convert minutes to seconds
 .S VFDA=$$VDT(VFDFN,$E(VFDT,1,VFDI-1)),VFDB=VFDO_VFDX_VFDZ
 .S VFDY=$S("DWM"[VFDZ:$$DWM(VFDA,VFDB),"hms"[VFDZ:$$HMS(VFDA,VFDB),1:"")
 .Q
 Q -1 ;Unrecognized expression
 ;
DWM(VFDT,VFDOFF) ;[Private] Wrap classic Fileman ^%DT procedure
 ;VFDT=[Required] Fileman Date +/- xxx D/W/M
 ;Precision is DAYS [For month offsets, returns same day number]
 ;
 S VFDT=$G(VFDT) Q:VFDT<0 +VFDT N VFD,VFDC,VFDD S VFDC=+VFDT
 S VFDOFF=$G(VFDOFF) ;VFD*2013.1*33 - Next is special for months
 I $$UP^XLFSTR($E(VFDOFF,$L(VFDOFF)))="M" Q $$MADD(VFDT,+VFDOFF)
 N %DT,X,Y S %DT="",X="T"_VFDOFF D ^%DT ;Y=Fileman Date(TODAY+VFDOFF)
 N VFDRSLT,VFDKEEP S VFDD=$$FMDIFF^XLFDT(Y,DT) ;VFDD=VFDOFF in days
 S (VFDRSLT,VFDKEEP)=$$FMADD^XLFDT(VFDC,VFDD,0,0,0) ;VFDT+VFDOFF
 Q VFDRSLT
 ;
HMS(VFDT,VFDOFF) ;[Private] Wrap $$FMADD^XLFDT
 ;VFDT=[Required] Fileman Date[/time] +/- xxx h/m/s
 S VFDT=$G(VFDT) Q:VFDT<0 +VFDT N VFDC,VFDO S VFDC=+VFDT,VFDO=$E(VFDOFF,$L(VFDOFF))
 Q $$FMADD^XLFDT(VFDC,0,$S(VFDO="h":VFDOFF,1:0),$S(VFDO="m":VFDOFF,1:0),$S(VFDO="s":VFDOFF,1:0)) ;VFDT+VFDOFF
 ;
DMHS(VFDT,VFDOFF) ;[Private] Wrap $$SCH^XLFDT
 ;VFDT=[Required] Fileman Date[/time]
 ;VFDOFF=[Required] unsigned xxx D/M/h/s (Days, Months, hours, seconds)
 ;
 S VFDT=$G(VFDT) Q:VFDT<0 +VFDT N VFDO,VFDY S VFDO=$E(VFDOFF,$L(VFDOFF))
 ;$$SCH^XLFDT has a 99-month maximum AND a maximum future year -
 I VFDO="M",VFDOFF>99 Q $$DMHS($$SCH^XLFDT("99M",+VFDT),(VFDOFF-99)_"M")
 S VFDY=$$SCH^XLFDT($$UP^XLFSTR(VFDOFF),+VFDT) ;$$SCH^XLFDT returns DATE + .24
 Q $S(VFDY?.E1".24":$$FMADD^XLFDT($P(VFDY,"."),0,24),1:VFDY)
 ;
MADD(VFDT,VFDM) ;[Private] - Add/subtract a whole number of months
 ; to/from Fileman date VFDT
 ;
 ; VFDT=[Required] Base date
 ; VFDM=[Required] Signed whole number of months
 ;
 ; This special purpose function replaces a previous method that
 ; converted months to days, then added or subtracted days, and
 ; finally adjusting the day number.
 ;
 S VFDT=$G(VFDT),VFDM=+$G(VFDM) I VFDT=+VFDT,VFDM'=0 N D,M,VFDX,Y
 E  Q "" ;Invalid parameter
 S Y=$E(VFDT,1,3),M=$E(VFDT,4,5),D=$E(VFDT,6,7)
 N AY,AM,ADD S ADD=(VFDM>0) S:'ADD VFDM=-VFDM S AY=VFDM\12,AM=VFDM#12
 N TY,TM,TD S TY=$S(ADD:Y+AY,1:Y-AY) ;First approximation to year
 I ADD S TM=M+AM S:TM>12 TY=TY+1,TM=TM#12 S:'TM TM=12,TY=TY-1
 E  S TM=M-AM,TY=Y-AY S:TM<1 TM=TM+12,TY=TY-1 ;First approximation to month
 S TM=$TR($J(TM,2)," ",0),TY=$TR($J(TY,3)," ",0),TD=D
 N DPM S DPM="31^28^31^30^31^30^31^31^30^31^30^31" ;Ordinary year
 ; Ignore Leap Century for now - Will be 'Year 2100 problem'
 I '(TD>$P(DPM,U,TM))!(TD=29&'(TY#4)) Q TY_TM_TD ;Most cases
 N DELTA S DELTA=D-$P(DPM,U,TM) ;Necessarily positive
 Q $$FMADD^XLFDT(TY_TM_$P(DPM,U,TM),DELTA,0,0,0)
 ;
