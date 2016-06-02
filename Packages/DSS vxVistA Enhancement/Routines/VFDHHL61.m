VFDHHL61 ;DSS/FHS - LOAD REMOTES LAB TEST INTO 60 ; 7/9/10 2:59pm
 ;;2012.1.1;VENDOR - DOCUMENT STORAGE SYS;;24 Jun 2013;Build 136
 ;Copyright 1995-2013,Document Storage Systems Inc. All Rights Reserved
 ;DSS/BUILD - VFDHHL HL7 EXCHANGE 2012.1.1 * 06/23/15 * vxdev64v2k8_VX13 * 2012.1 T22
 ;DSS/BUILD - VFDHHL HL7 EXCHANGE 2012.1.1 * 02/02/14 * vxdev64v2k8_DEVXOS * 2012.1.1T21
 ;Copyright 1995-2010,Document Storage Systems Inc. All Rights Reserved
 Q
EN ;Entry point to create atomic tests
NA ;^VFD(21626.01,"FLG1","NA",IEN
 W !,"NA"
 ;
 N EXEC,NLT,LRA,LRAA,LRCHKNM,LRDNAM
 N LRDSEQ,LRERR,LRHL,LRIIEN,LRINST,LRISEQ,LRPRTO
 N LRPRT,RNLT,LRSAMP,LRSEQ,LRSPEC,LRSTR,LRSTYL,LRSUB,LRSYM,LRTST,LRTYP
 N LRUR,LRURG,LRVNM,X,Y,ZLR
 ;DSS/PDW T22 
 N VFDSLOTB,VFDSLOTE ; beginning of interface number 10,000 slot
 ;DSS/PDW T22 
 ;
 N DIG,DIH,DIU,DIV,FDA,J,LRSEQQ,LRTAG,LRVNM
 ;
 S VFDFILE=21626.01,LRISEQ=0,LRTAG="NA"
 ;
 F  S LRISEQ=+$O(^VFD(VFDFILE,"FLG1","NA",LRISEQ)) Q:LRISEQ<1  D GO(LRISEQ) ;K ^VFD(VFDFILE,"FLG1","NA",LRISEQ)
 Q
 ;
NP ;Entry point to process panel test
 W !,"NP"
 ; ^VFD(21626.01,"FLG1","NP",IEN
 N EXEC,NLT,LRA,LRAA,LRCHKNM,LRDNAM
 N LRDSEQ,LRERR,LRHL,LRIIEN,LRINST,LRISEQ
 N LRPRT,RNLT,LRSAMP,LRSEQ,LRSPEC,LRSTR,LRSTYL,LRSUB,LRSYM,LRTST,LRTYP
 N LRUR,LRURG,LRVNM,X,Y,ZLR
 N DIG,DIH,DIU,DIV,FDA,J,LRISEQQ,LRTAG,LRPTRO
 N LRUR,LRURG,LRVNM,X,Y,ZLR
 ;DSS/PDW T22 
 N VFDSLOTB,VFDSLOTE ; beginning and end of interface number 10,000 slot
 ;DSS/PDW T22
NP1 ;
 W !,"NP1"
 ;
 S VFDFILE=21626.01,LRISEQ=0,LRTAG="NP"
 F  S LRISEQ=$O(^VFD(VFDFILE,"FLG1","NP",LRISEQ)) Q:LRISEQ<1  D GO2 ;K ^VFD(VFDFILE,"FLG1","NP",LRISEQ)
 ;
 Q
GO2 ;
 W !,"GO2"
 ; USE SYNONYM KEY to filter vendors
 I $D(VFDVENSY),VFDVENSY'=$$GET1^DIQ(21626.01,LRISEQ,.19) Q
 ;
 N LRVNM
 S LRERR=1
 D ZAP(LRISEQ,LRTAG)
 S LRSEQ=$$FNDTST1(LRISEQ)
 I LRSEQ D  ;Update synonym every time through - same as Atomic test
 . S LRSYM=$P($G(^VFD(VFDFILE,LRISEQ,1)),U)
 . D:LRSYM'="" UPDAT5(LRSEQ,LRSYM)
 I 'LRSEQ D IN(LRISEQ)
 I 'LRERR W !,LRERR D ERR(LRISEQ,LRERR,LRTAG) Q
 I 'LRSEQ S LRSEQ=$$FNDTST(LRTST)
 I 'LRERR W !,LRERR D ERR(LRISEQ,LRERR,LRTAG) Q
 K FDA,ERR
 S FDA(1,VFDFILE,LRISEQ_",",.03)=LRSEQ
 D FILE^DIE("KS","FDA(1)","ERR")
GO3 ;
 W !,"GO3"
 N LRATM,LRPTR,LRSEQ,LRPSEQ,LRSTOP,LRTST
 S (LRSTOP,LRATM)=0
 S LRPSEQ=+$P($G(^VFD(VFDFILE,LRISEQ,0)),U,3)
 F  S LRATM=+$O(^VFD(VFDFILE,LRISEQ,10,LRATM)) Q:LRATM<1!(LRSTOP)  D
 . S LRPTR=+$G(^VFD(VFDFILE,LRISEQ,10,LRATM,0))
 . S LRSEQ=+$P($G(^VFD(VFDFILE,LRPTR,0)),U,3)
 . I 'LRSEQ S LRERR=1 D GO(LRPTR)
 . I 'LRERR S LRSTOP=1 Q
 . S LRTST(LRSEQ)=""
 I $O(LRTST(0)) D:'$G(LRSTOP) PAN(LRPSEQ,.LRTST,1)
 D
 . N FDA,ERR
 . I '$G(LRSTOP) S FDA(1,VFDFILE,LRISEQ_",",.16)="DONE "_LRTAG
 . I $G(LRSTOP) S FDA(1,VFDFILE,LRISEQ_",",.16)="ERR "_LRTAG
 . D FILE^DIE("KS","FDA(1)","ERR")
 Q
GO(LRISEQ) ;
 W !,"GO"
 ; USE SYNONYM KEY to filter vendors
 I $D(VFDVENSY),VFDVENSY'=$$GET1^DIQ(21626.01,LRISEQ,.19) Q
 ;
 N ERR,I
 N ANS,DO,D1,DA,DG,DIC,DIU,DIK
 N ANSXY,D0,DA,FDA,IEN,LRA,LRAA,LRCHKNM
 N LRDNAM,LRDSEQ,LRIEN,LRIIEN,LRINST,LRPRT,LRPRTO ;removed LRHL
 N LRSAMP,LRSPEC,LRTYL,LRSUB,LRSYM ;REMOVED LRSTR
 N LRTYP,LRUR,LRURG,LRVNM,LRZSAME ;removed LRTST
 N MFE,MFI,NODE,RP,SUB,X,Y,ZLR
 ;
GO1 ;
 W !,"GO1"
 D ZAP(LRISEQ,LRTAG)
 S LRERR=1
 S (LRZSAME,LRSEQ)=$$FNDTST1(LRISEQ)
 D IN(LRISEQ)
 I LRERR,LRSEQ D:$G(LRSYM)'="" UPDAT5(LRSEQ,LRSYM)
 I 'LRERR W !,LRERR D  Q
 . D ERR(LRISEQ,LRERR,LRTAG)
 ;
 I 'LRSEQ S LRSEQ=$$FNDTST(LRTST)
 I LRERR D
 . K FDA,ERR
 . S FDA(2,VFDFILE,LRISEQ_",",.03)=LRSEQ
 . S FDA(2,VFDFILE,LRISEQ_",",.16)="DONE "_LRTAG
 . D FILE^DIE("KS","FDA(2)","ERR")
 W !,LRISEQ_" - "_^VFD(VFDFILE,LRISEQ,0)
 Q
 ;
FILEIEN(SEQ,IEN60) ; Store pointer to 60 for created test
 ;SEQ=IEN=^VFD
 ;IEN60=IEN in ^LAB(60,
 W !,"FILEIEN"
 N ERR,FDA
 S FDA(1,VFDFILE,SEQ_",",.03)=IEN60
 S FDA(1,VFDFILE,SEQ_",",.16)="DONE "_LRTAG
 D FILE^DIE("KS","FDA(1)","ERR")
 Q
 ;
ERR(IEN,TXT,LRTAG) ;Set error message into file
 W !,"ERR"
 N FDA,ERR
 S FDA(1,VFDFILE,IEN_",",.2)=TXT
 S FDA(1,VFDFILE,IEN_",",.16)="ERR "_$G(LRTAG)
 D FILE^DIE("KS","FDA(1)","ERR")
 Q
 ;
EN1(LRDNAM,LRHL) ;Get IEN for dataname
 W !,"IEN"
 S LRDSEQ=""
 I LRDNAM="" Q LRDSEQ
 I LRSTYL="C" Q LRDSEQ
 I LRHL S LRDSEQ=$$DNAME(LRDNAM,LRHL)
 Q LRDSEQ
 ;
 ;
FNDTST(LRTST) ;Find or create Test in file 60
 W !,"FNDTST"
 K LRSEQ
 ;DSS/PDW T22 beginning of interface slot
 S LRSEQ=+$O(^LAB(60,"B",LRTST,VFDSLOTB-1))
 S:'LRSEQ LRSEQ=VFDSLOTB
 S:LRSEQ>VFDSLOTE LRSEQ=VFDSLOTB
 ;DSS/PDW T22
 D UPDAT1 ;create fda(1) for file 60 entry
 ;
 D DUPDAT1
 ;
 ;DSS/PDW 20131106 SET BACKPOINTER IN ^LAM
 D:$G(^LAB(60,LRSEQ,64)) 
 .N LRNLTVA S LRNLTVA=+^LAB(60,LRSEQ,64)
 .D LINK(LRSEQ,LRNLTVA,1)
 ;end DSS/PDW 20131106 SET BACKPOINTER IN ^LAM
 ;
 D UPDAT2(LRSEQ,LRAA,LRINST)
 ;
 D UPDAT3(LRSEQ,LRSPEC)
 ;
 D UPDAT4(LRSEQ,LRSAMP)
 ;
 ;/DSS/PDW MOD TO ADD SYNONYM
 D UPDAT5(LRSEQ,LRSYM) ; add synonym /PDW 
 ;/DSS/PDW END MOD TO ADD SYNONYM
 Q LRSEQ
 ;
IN(LRISEQ) ;Entry point to parse a singel entry
 ;
 D ZAP(LRISEQ,LRTAG)
VALIDATE ;Entry point to validate scratch file entry.
 W !,"VALIDATE"
 K ANSXY,LRSTR
 ;N ANSXY,LRSTR,LRCHKNM,LRVNM,LRCHKNM
 S LRISEQ=+LRISEQ,LRERR=1,(LRCHKNM,LRSTR)=""
 S (LRZSAME,LRSEQ)=$$FNDTST1(LRISEQ) ;added for those already there
 I '$L($G(^VFD(VFDFILE,LRISEQ,0))) S LRERR="O^1^No valid entry" Q
 S LRSTR=^VFD(VFDFILE,LRISEQ,0)_U_$G(^VFD(VFDFILE,LRISEQ,1))
 D GETS^DIQ(VFDFILE,LRISEQ_",","*","I","ANSXY","ERR")
 S LRVNM=$P(LRSTR,U,2) D  ;Get vendors name for possible synonym
 . I $O(^LAB(60,"B",LRVNM,0)) S LRVNM="" Q
 . N ANS
 . D CHK^DIE(60.1,.01,"E",LRVNM,.ANS)
 . I ANS="^" S LRVNM=""
 ;N LRIIEN,LRDNAM,LRSAMP,LRSUB,LRSTYL,LRTYP
 S LRIIEN=+$P(LRSTR,"^",3) I LRIIEN D  Q:'LRERR
 . S LRCHKNM=$P($G(^LAB(60,LRIIEN,0)),U)
 . I LRCHKNM="" S LRERR="0|3|Incomming IEN ["_LRIIEN_"] does not exist"
 S LRDNAM=$P(LRSTR,"^",6) I $L(LRDNAM)>30 D  Q
 . S LRERR="0|5|Dataname ["_LRDNAM_"] >30 characters"
 I LRDNAM'="",$L(LRDNAM)<2 S LRERR="O^5.1^Dataname ["_LRDNAM_"] Too Short" Q
 S LRSPEC=+$P(LRSTR,"^",4) I LRSPEC,'$D(^LAB(61,LRSPEC,0)) D  Q
 . S LRERR="0|4|Site/Spec ["_LRSPEC_"] does not exist"
 S LRSAMP=+$P(LRSTR,"^",5) I LRSPEC,'$D(^LAB(62,LRSAMP,0)) D  Q
 . S LRERR="0|6|Specimen ["_LRSAMP_"] Does not exist"
 S LRSUB=$P(LRSTR,"^",7) S:LRSUB="" LRSUB="X" I "CHMISP"'[LRSUB D  Q
 . S LRERR="0|7|Subscript ["_LRSUB_"] is unknown"
 S LRSTYL=$P(LRSTR,"^",8) S:LRSTYL="" LRSTYL="X" I "AC"'[LRSTYL D  Q
 . S LRERR="0|8|The type ["_LRSTYL_"] is not allowed"
 I LRSTYL="A",LRSUB="CH",$L(LRDNAM)<2 D  Q
 . S LRERR="0|3|Dataname is missing"
 S LRTYP=$E($P(LRSTR,"^",9)) S:LRTYP="" LRTYP="X" S LRTYP=$$UP(LRTYP)
 I "OBNI"'[LRTYP D  Q
 . S LRERR="0|9|The type ["_LRTYP_"] is not allowed"
 ;N LRSYM,ANS,LRURG,LRAALRPRT
 S LRSYM=$$UP($P(LRSTR,"^",10))
 K ANS I '$G(LRZSAME) D CHK^DIE(60.1,.01,"E",LRSYM,.ANS)
 I $G(ANS)="^" S LRERR="0|10|SYNONYM ["_LRSYM_"] Is not allowed" Q
 S LRTST=$P(LRSTR,"^",11) D  Q:'LRERR
 . N ANS
 . S LRERR=1 D CHK^DIE(60,.01,"E",LRTST,.ANS)
 . I ANS="^" S LRERR="0|11|Desired name not allowed"
 I LRVNM'="",LRVNM'=LRTST S LRVNM=""
 I $G(LRIIEN),$$UP(LRCHKNM)'=$$UP(LRTST) D  Q
 . S LRERR="0|11|IEN name ["_LRCHKNM_"] doesn't match list name ["_LRTST_"]"
 S LRURG=+$P(LRSTR,"^",12)
 I '$D(^LAB(62.05,LRURG,0)) D  Q
 . S LRERR="0|12|Urgency ["_LRURG_"] is unknown"
 S LRAA=+$P(LRSTR,"^",13)
 I '$D(^LRO(68,LRAA,0)) D  Q
 . S LRERR="0|13|Accession Area ["_LRAA_"] is unknown"
 S LRPRT=$P(LRSTR,"^",14) I LRPRT="" D  Q
 . S LRERR="0|14|No print name"
 ;N LRHL,LRPTRO,LRNLTVA
 K ANS I '$G(LRZSAME) D CHK^DIE(60,51,"E",LRPRT,.ANS)
 I $G(ANS)="^" S LRERR="0|14|Print name ["_LRPRT_"] is not allowed" Q
 S LRHL=+$G(^VFD(VFDFILE,LRISEQ,3))
 N LRUR,LRURG,LRVNM,X,Y,ZLR
 ;DSS/PDW T22
 S VFDSLOTB=LRHL*10000,VFDSLOTE=VFDSLOTB+9999 ; beginning and end of interface number 10,000 slot
 ;DSS/PDW T22
 S LRPRTO=$P($G(^VFD(VFDFILE,LRISEQ,3)),U,2)
 I LRHL<1 S LRERR="0|18|No interface specifed" Q
 ;S LRINST=+$P(LRSTR,"^",19)
 ;I LRINST,'$D(^DIC(4,LRINST,0)) D  Q
 ;. S LRERR="0|19|Institution ["_LRINST_"] does not exist"
 ;/PDW adding Nat VA Lab Code Check for TYPE="BOTH"
 I $G(ANSY(VFDFILE,LRISEQ_",",.09,"I"))="B" D  Q:$L($G(LRERR))
 .S LRNLTVA=+$G(ANSXY(VFDFILE,LRISEQ_",",.41,"I"))
 .I 'LRNLTVA S LRERR="0|20|No Nat VA Lab Code" Q
 .I LRNLTVA,'$D(^LAM(LRNLTVA,0)) S LRERR="0|21|WKLD CODE "_LRNLTVA_" IS UNKNOWN" Q
 .Q
 ;/PDW adding Nat VA Lab Code Check
 S:'$G(LRINST) LRINST=+$$DEFSITE
 Q
 ;
DNAME(NAME,LRHL) ; $$ Find existing dataname or create new and return the IEN
 W !,"DNAME"
 ;
 ;Name= requested dataname
 ;Name must be smaller than 31 characters
 ;LRHL= starting IEN number sequence*10000
 I $L(NAME)>30 Q "0^6304^Dataname ["_NAME_"] longer than 30 characters"
 S NAME=$$UP^XLFSTR(NAME)
 N IEN,DA,DIK
 S IEN=$O(^DD(63.04,"B",NAME,0)) I IEN Q IEN
 ;20 JUN 12 mod to insure clear IEN space to work in
 N LRUR,LRURG,LRVNM,X,Y,ZLR
 ;DSS/PDW T22 beginning of interface number 10,000 slot
 N VFDSLOTB,VFDSLOTE
 S VFDSLOTB=LRHL*10000,VFDSLOTE=VFDSLOTB+9999
 S IEN=$O(^DD(63.04,VFDSLOTE+1),-1)
 I IEN<VFDSLOTB S IEN=VFDSLOTB
 ;DSS/PDW T22 end
 ;end mod /pdw
 N LRUR,LRURG,LRVNM,X,Y,ZLR
 S ^DD(63.04,IEN,0)=NAME_"^F^^"_IEN_";1^K:$L(X)>50!($L(X)<1) X"
 S ^DD(63.04,IEN,3)="ANSWER MUST BE 1-50 CHARACTERS IN LENGTH"
 S ^DD(63.04,IEN,"DT")=DT
 S $P(^DD(63.04,0),U,4)=$P(^DD(63.04,0),U,4)+1
 S DA=IEN,DA(1)=63.04,DIK="^DD(63.04," D IX1^DIK
 Q IEN
 ;
 Q
 ;
UP(X) ;Change string to all upper characters
 Q $$UP^XLFSTR(X)
 ;
DEFSITE() ;Set default institution
 I $G(VFDINST) Q VFDINST
C4 S DIC=4,DIC(0)="AEQMZ" D ^DIC
 I +Y'>0  W !,"A choice must be made" G C4
 S VFDINST=+Y
 Q VFDINST
 ;
 ;
FDA1(LRHL,LRTST,LRTYP,LRSUB,LRURG,LRPRT,NLT,RNLT,LRDSEQ) ; Build FDA(1 Array
UPDAT1 ; Entry point is here
 W !,"UPDAT1"
 ; LRDSEQ=IEN from ^DD(63.04,LRDSEQ,0)=dataname
 ;DSS/PDW T22 beginning of interface slot
 I '$G(LRSEQ) D
 .S LRSEQ=$O(^LAB(60,VFDSLOTE+1),-1)
 .S:LRSEQ<VFDSLOTB LRSEQ=VFDSLOTB
 ;DSS/PDW T22 end
 ;DSS/PDW END
 K DA,LRIEN,FDA
 S (DA,LRIEN(1))=LRSEQ,LRDSEQ=0
 S FDA(1,60,"+1,",.01)=LRTST ;Test name
 S FDA(1,60,"+1,",3)=LRTYP ;Type
 S FDA(1,60,"+1,",4)=LRSUB ;Subscript
 I LRSUB="CH",LRSTYL="A" S LRDSEQ=$$DNAME(LRDNAM,LRHL)
 ;S FDA(1,60,"+1,",5)="CH;"_LRDSEQ_";1" ;Location  - Triggered by field 400
 S FDA(1,60,"+1,",17)=LRURG ; Highest Urgency
 S FDA(1,60,"+1,",51)=LRPRT ;Print Name
 N LRNLTVA
 S LRNLTVA=+$G(ANSXY(VFDFILE,LRISEQ_",",.41,"I"))
 I $G(LRPRTO) S FDA(1,60,"+1,",56)=LRPRTO ; Print Order
 I $G(LRNLTVA) S FDA(1,60,"+1,",64)=LRNLTVA ;NLT code ;/PDW 04/05/2005
 I $G(LRNLTVA) S FDA(1,60,"+1,",64.1)=LRNLTVA ;Result NLT code.
 I +$G(LRDSEQ) S FDA(1,60,"+1,",400)=LRDSEQ ;^DD(63.04,LRDSEQ,0)
 Q
 ;
DUPDAT1 ;
 W !,"DUPDAT1"
 D SAVE
 K ERR
 D UPDATE^DIE("S","FDA(1)","LRIEN","ERR")
 D MAN
 Q
 ;
UPDAT2(LRIEN,LRAA,LRINST) ;Set accession area for the test
 W !,"UPDAT2"
 N ERR,FDA,IEN,LRI
 S IEN="+1,"_LRIEN_","
 S FDA(2,60.11,IEN,.01)=LRINST
 S FDA(2,60.11,IEN,1)=LRAA
 S LRI(1)=LRINST
 D UPDATE^DIE("KS","FDA(2)","LRI","ERR")
 Q
 ;
UPDAT3(LRIEN,SPEC) ;Set site specimen
 W !,"UPDAT3"
 ; SPEC pointer to ^LAB(61
 Q:'$D(^LAB(61,+SPEC,0))
 ;DSS/PDW 20131107 DINUM the addition of specimun, vfdlrien being used
 N VFDLRIEN
 S VFDLRIEN(1)=SPEC
 ;END DSS/PDW 20131107 DINUM the addition of specimun, vfdlrien being used
 K ERR,FDA(3),IEN
 S IEN="+1,"_+LRIEN_","
 S FDA(3,60.01,IEN,.01)=SPEC
 ;DSS/PDW add fields to site specimen 20131112
 N LRSPFLDS
 N RFRNCLW,RFRNCHG,CRTCLW,CRTCHG,UNTS
 ;".51;.52;.53;.54;55 stored in node 5
 S LRSPFLDS=$G(^VFD(21626.01,LRISEQ,5))
 S DELIM="^"
 S XX=LRSPFLDS
 F YY="RFRNCLW=1","RFRNCHG=2","CRTCLW=3","CRTCLHG=4","UNTS=5" D GET(XX,DELIM,YY)
 S Z(1)=RFRNCLW,Z(2)=RFRNCHG,Z(3)=CRTCLW,Z(4)=CRTCLHG,Z(6)=UNTS
 M FDA(3,60.01,IEN)=Z
 ;end DSS/PDW add fields to site specimen 20131112
 D UPDATE^DIE("KS","FDA(3)","VFDLRIEN","ERR")
 Q
 ;
UPDAT4(LRIEN,SAMP) ;Set collection sample
 W !,"UPDAT4"
 ;SAMP pointer to ^LAB(62,
 Q:'$D(^LAB(62,+SAMP,0))
 K ERR,FDA(4),IEN
 S IEN="+1,"_+LRIEN_","
 S FDA(4,60.03,IEN,.01)=SAMP
 D UPDATE^DIE("KS","FDA(4)","","ERR")
 Q
 ;
 ;/DSS/PDW mod to add synonym to LAB TEST
 ;/DSS/PDW do not put synonym in if it's the test name 29 Apr 11
UPDAT5(LRIEN,LRSYM) ;Set SYNONYM for the test
 W !,"UPDAT5"
 Q:LRSYM=""
 I LRSYM=$$GET1^DIQ(60,LRIEN,.01) Q  ; test name .01 not to be a synonym 
 N ERR,FDA,IEN,LRI
 S IEN="?+1,"_LRIEN_"," ;
 S FDA(5,60.1,IEN,.01)=LRSYM
 S LRI(5)=LRIEN
 D UPDATE^DIE("S","FDA(5)","LRI","ERR")
 Q
 ;/DSS/PDW END MOD
SAVE ;Backup files
 W !,"SAVE"
 ;Save Lab Test Names
 I '$O(^XTMP("VFDLR","LAB",60,0)) D
 . S ^XTMP("VFDLR",0)=$$HTFM^XLFDT($H+120,1)_U_DT_U_"VFD Install Backup of File 60,101.43 and DD 63.04"
 . M ^XTMP("VFDLR","LAB",60)=^LAB(60)
 ;Save datanames
 I '$O(^XTMP("VFDLR","LAB",63.04,0)) D
 . M ^XTMP("VFDLR","LAB",63.04)=^DD(63.04)
 ;Save Orderable Items
 I '$O(^XTMP("VFDLR","LAB",101.43,0)) D
 . M ^XTMP("VFDLR","LAB",101.43)=^ORD(101.43)
 Q
FNDTST1(LRISEQ) ;Check to see if test already loaded
 W !,"FNDTST1"
 S LRSEQ=+$P($G(^VFD(VFDFILE,LRISEQ,0)),U,3)
 I LRSEQ Q LRSEQ
 N LRTSTNM
 S LRTSTNM=$P($G(^VFD(VFDFILE,LRISEQ,1)),U,2)
 ;DSS/PDW T22 beginning of interface 10000 slot
 I '$D(VFDSLOTB) N VFDSLOTB,VFDSLOTE S VFDSLOTB=LRHL*10000,VFDSLOTE=VFDSLOTB+9999
 I LRTSTNM'="" D
 .S LRSEQ=+$O(^LAB(60,"B",LRTSTNM,VFDSLOTB-1))
 .S:LRSEQ<VFDSLOB LRSEQ=VFDSLOTB
 .S:LRSEQ>VFDSLOTE LRSEQ=0
 ;DSS/PDW T22 end
 I LRSEQ D
 . N FDA,ERR
 . S FDA(1,VFDFILE,LRISEQ_",",.03)=LRSEQ
 . D FILE^DIE("KS","FDA(1)","ERR")
 Q LRSEQ
 ;
 Q
CHKNAME(IEN,NAME) ;Verify name is correct for ^LAB(60,IEN,0)
 W !,"CHKNAME"
 ;
 I '$D(^LAB(60,IEN,0)) Q 0
 I $P(^LAB(60,IEN,0),U)'=NAME Q 0
 Q 1
 ;
MAN ;
 W !,"MAN"
 K NODE,RP
 S RP(5)="",RP(12)=""
 I LRDSEQ,LRSUB="CH" D
 . S RP(5)="CH;"_LRDSEQ_";1"
 . S RP(12)="DD(63.04,"_LRDSEQ_","
 S NODE(0)=LRTST_"^^"_LRTYP_U_LRSUB_U
 S $P(NODE(0),U,5)=RP(5)
 S $P(NODE(0),U,12)=RP(12)
 S $P(NODE(0),U,16)=LRURG
 S ^LAB(60,LRSEQ,0)=NODE(0)
 S ^LAB(60,LRSEQ,.1)=LRPRT
 I $G(LRPRTO) S $P(^LAB(60,LRSEQ,.1),U,6)=LRPRTO
 I $G(LRDSEQ) S ^LAB(60,LRSEQ,.2)=LRDSEQ
 I $G(LRVNM)'="" D  ;Set venders name into synonym
 .;DSS/PDW block synonym=.01
 . I LRVNM=$P(^LAB(60,LRSEQ,0),U) Q
 .;DSS/PDW block synonym=.01
 . S ^LAB(60,LRSEQ,5,1,0)=LRVNM
 N DA,DIK
 S DA=LRSEQ,DIK="^LAB(60," D IX1^DIK
 Q
PAN(IEN,LRTST,PURG) ;Load test in panel
 W !,"PAN"
 ;IEN=Panel test IEN
 ;LRTST=LRTST(POINTER TO 60)=""
 ;PURG=1 (Remove all previous entries)
 ;PURG=0 (Add entries to previous test)
 Q:'$D(^LAB(60,IEN,0))
 Q:$P(^LAB(60,IEN,0),U,5)'=""
 I $G(PURG) K ^LAB(60,IEN,2)
 N ERR,FDA,LRIEN,TST
 S TST=0 F  S TST=$O(LRTST(TST)) Q:TST<1  D
 . K ERR,FDA,LRIEN
 . S FDA(1,60.02,"+1,"_IEN_",",.01)=TST
 . D UPDATE^DIE("KS","FDA(1)","LRIEN","ERR")
 N ERR,FDA
 S FDA(2,VFDFILE,LRISEQ_",",.16)="DONE NP"
 D FILE^DIE("KS","FDA(2)","ERR")
 Q
ZAP(LRISEQ,LRTAG) ;Purge any previous errors
 W !,"ZAP"
 ;
 K ^VFD(VFDFILE,LRISEQ,2)
 ;K ^VFD(VFDFILE,"FLG1","ERR "_LRTAG,LRISEQ)
 Q
 ;
CHKIEN(LRISEQ) ;Entry point to check data in VFD(21626.01,LRISEQ)
 W !,"CHKIEN"
 ;
 ; Return 1 if there are no errors (passed)
 ; Return 0 if errors are found  (failed)
 ;  stops checking when the first error is found.
 ; Error out = 0|error code|error message
 ;
 N ANS,ANSXY,LRAA,LRCHKNM,LRERR,LRHL,LRIIEN,LRINST
 N LRPRT,LROOT,LRSAMP,LRSEQ,LRSEQQ,LRSPEC,LRSTR,LRSTYL,LRSUB
 N LRSYM,LRTAG,LRTST,LRTYP,LRURG,R,VFDFILE,X,Y
 K LRERR,LRTESTX
 S VFDFILE=21626.01,LRERR=1
 I $P($G(^VFD(VFDFILE,LRISEQ,0)),U,3),'$O(^VFD(VFDFILE,LRISEQ,10,0)) Q 1
 I '$P($G(^VFD(VFDFILE,LRISEQ,0)),U,3) D VALIDATE I '$G(LRERR) Q LRERR
 S LROOT=LRISEQ,LRISEQQ=0
 F  S LRISEQQ=$O(^VFD(VFDFILE,LROOT,10,LRISEQQ)) Q:LRISEQQ<1  D
 . S LRISEQ=+$G(^(LRISEQQ,0))
 . Q:$P(^VFD(VFDFILE,LRISEQ,0),U,3)
 . D VALIDATE I 'LRERR S LRTESTX(LRISEQ)=LRERR
 I '$O(LRTESTX(0)) Q 1
 Q "0|Check LRTESTX Array"
 ;
Q(VAL) ; DISPLAY VAR WITH $Q
 N X,X1 S (X,X1)=VAL
 Q:'$D(@VAL)
 I $E(X,$L(X))=")" S X1=$E(X,1,$L(X)-1)
 I $D(@X)#10 W !,X,"=",@X
 F  S X=$Q(@X) Q:X'[X1  W !,X,"=",@X
 Q
 ;DSS/PDW 20131106 SET BACKPOINTER IN ^LAM
LINK(X60,X64,DOIT) ;Link the 2 files borrowed from LR7OU4
 N DIE,DA,DR,DIC,LRDATA,%,END,LRDATA
 S LRDATA="`"_X60
L2 D:$G(^LAB(60,X60,64)) DXSS
 K DIE,DA,DR,DIC S DIE="^LAB(60,",DA=X60,DR="64////^S X=X64",DLAYGO=60 D ^DIE K DLAYGO
XSS K DIE,DA,DR,DIC S DIE="^LAM(",DA=X64,DR="23///^S X=LRDATA;",DR(1,64)="23///^S X=LRDATA;",DR(2,64.023)=".01////LRDATA;",DLAYGO=64
 S DIC("V")="I +Y(0)=60" D ^DIE K DIC K DLAYGO
 Q
DXSS N DIE,DA,DR,DIC,DIK,DLAYGO
 S DA(1)=+$G(^LAB(60,X60,64)),DIK="^LAM("_DA(1)_",7,",DLAYGO=64
 S DA=$O(^LAM(DA(1),7,"B",X60_";LAB(60,",0))
 D:DA&(DA(1)) ^DIK
 Q
 ;DSS/PDW 20131106 SET BACKPOINTER IN ^LAM
GET(REC,DLM,XX) ; where XX = VAR_"="_I  ex: XX="PATNM=1"
 ; Set VAR = piece I of REC using delimiter DLM
 N Y,I S Y=$P(XX,"="),I=$P(XX,"=",2),@Y=$P(REC,DLM,I)
 Q
SET(REC,DLM,XX) ; where XX = VAR_U_I  ex: XX="1=PATNUM"
 ; Set VAR into piece I of REC using delimiter DLM
 N Y,I S I=$P(XX,"="),Y=$P(XX,"=",2)
 I Y'=+Y,Y'="" S $P(REC,DLM,I)=$G(@Y) I 1
 E  S $P(REC,DLM,I)=YOUT ;
 Q
HISTORY ;
 ; needed '$L($G(^VFD(VFDFILE,LRISEQ,0))) to pass validate
 ;29Apr11 UPDAT5 insure synonym'=TestName 
 ; 27 Mar 2012 added test for National VA LAB Code field .41
 ; 02 Apr 2012 adjusted testing for .41 for TYPE="BOTH"
 ;03 Apr 2012 GO1 removed LRSTR and LRTST from NEWing
 ;03 Apr 2012 added FNDTST1 call to VALIDATE sub
 ;05 June 2012 add code to block vendor name going into synonym if = .01
 ;20 June 2012 insure clear IEN space by $O(^(DD(63.04,"A"),-1)  $O(^LAB(60,"A"),-1) 
 ;06 Nov 2013 add creating a ^LAM( file 64 backpointer to file 60 ^LAB(
 ;06 nOV 2013 added lines GET and SET
 ;18 June 2015 T22 beginning and end of interface slot used for LRSEQ ^LAB(60 IEN
 ;and LRDSEQ Dataname IEN
T22 ; Adjust LRSEQ and LRDSEQ to be within the 10,000 slot
 ;setup by LRHL the interface Number
