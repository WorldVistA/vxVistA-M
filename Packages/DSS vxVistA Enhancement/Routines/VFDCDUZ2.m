VFDCDUZ2 ;DSS/SGM - COMMON NEW PERSON FILE RPCS ;06/04/2014 18:06
 ;;2013.0;DSS,INC VXVISTA OPEN SOURCE;;07 Feb 2014;Build 2
 ;Copyright 1995-2014,Document Storage Systems Inc. All Rights Reserved
 ;
 ;  DO NOT INVOKE THIS ROUTINE DIRECTLY, SEE VFDCDUZ ROUTINE
 ;
 ; ICR#  Use  Description [vxVistA not a subscriber to CS ICRs]
 ;-----  ---  ---------------------------------------------------------
 ; 2051  Sup  ^DIC1:   $$FIND1
 ; 2171  Sup  ^XUAF4:  $$LKUP - logic cloned
 ;                       convert ien,name,station# to ien
 ;                       ^XUAF4 screened out non-active locations
 ; 4532  CS   ^XUSNPI: $$CKKDGT - validates value of NPI number
 ;            ^XUSNPI: $$QI - return one value or delimited values.
 ;                This references parameter XUSNPI QUALIFIED IDENTIFIER
 ; 4574  Sup  ^XUPS:   $$IEN - convert VPID to duz
 ; 4758  CS   ^XUSHSH: $$EN - hash unencrypted access code
 ;       xxx  ^XUSER2: some logic cloned in this routine
 ;                     $$DEANUM - validate DEA#
 ;                     VALDEA - called from input transform 200,53.2
 ;                     VANUM - called from input transform 200,53.3
 ;                        VA code checks for only one Active VA#
 ;                        vxVistA checks for only one VA# active or not
 ;10053  Sup  ^XUSERNEW: $$ADD - add new person file entry
 ;10060  Sup  File 200 - FileMan read of all fields
 ;                       Package specific fields fully accessible
 ;                       Direct global read of indexes not allowed
 ;10090  Sup  File 4 - entire file allows for read with FileMan
 ;10096  Sup  ^%ZOSF - all nodes exported by Kernel are useable
 ;
 ;-----------------------
 ; 2343  ^XUSER
 ;10104  $$REPLACE^XLFSTR
 ;
 Q
 ;
 ;=====================================================================
FIND1(VFDIN) ;
 ; Extrinsic function returns File_200_ien or 0 or -1(if problems)
 ;
 ; Lookup done in the sequence order of the input array VFDIN.  The
 ; first find a unique record is returned and any remaining lookup
 ; values are not tried.
 ;
 ; INPUT PARAMETERS:
 ;  .VFDIN(j) = p1^p2^p3^p4 where j=1,2,3,4,...
 ;    p1: req - KEY_TERM
 ;    p2: opt - DELIMITER, for most cases p2 should be <null>.
 ;              Default to '^'
 ;              Since VFDIN(j) is delimited by ^, do not pass in 2nd-
 ;                    piece of VFDIN(j) with ^
 ;              Delimiter separates components of input value (p4)
 ;    p3: opt - ACTIVE, Boolean flag only relevant for Key-Term VFD and
 ;              location is passed.  If true, only scan across active
 ;              locations, else scan across all locations 
 ;    p4: req - LOOKUP VALUE
 ;              If lookup will use a compound index, then p2 will be
 ;              the delimiter that separates components of input value
 ;              See table below for description of lookup values
 ;
 ;    KEY  |          | Unique |
 ;    TERM |  FIELD#  | Lookup | Input lookup value
 ;    ---- | -------- | ------ | ---------------------------------------
 ;    AC   |   2      |   Y    | access code unencrypted
 ;    ACE  |   2      |   Y    | access code encrypted
 ;    CELL |    .137  |        | cell phone
 ;    DEA  |  53.2    |   Y    | DEA#
 ;    HOME |    .131  |        | home telephone
 ;    LIC  |  54.1;M  |   Y    | state licenses
 ;    NM   |    .01   |        | name string
 ;    NPI  |  41.99   |        | NPI#
 ;    OFF  |    .132  |        | office telephone
 ;    SSN  |   9      |   Y    | social security number
 ;    VA   |  53.3    |   Y    | VA#
 ;    VPID |  9000    |   Y    | VA Person Identifier (VPID)
 ;    ZIP  |    .116  |        | zip code
 ;    VFD  | 21600;M  |   Y    | vxVistA alternate identifier
 ;    HL   | named components (delimited)
 ;         | default delimiter '^', delimited string format:
 ;         | family~given~middle~suffix~prefix~deg
 ;
 ;    For LIC and VFD the input is of the form v1^v2^v3 where
 ;        LIC: v1 = license#   v2 = state      v3 = ID type
 ;        VFD: v1 = id value   v2 = location   v3 = other data
 ;    There are 4 different unique combinations searched for
 ;    depending upon that input string
 ;      v1       is unique across all of VistA
 ;      v1,v2    is unique across all of VistA
 ;      v1,v3    is unique across all of VistA
 ;      v1,v2,v3 is unique across all of VistA
 ;
 N I,J,X,Y,Z
 I $G(VFDIN)[U S X=VFDIN K VFDIN S VFDIN(1)=X
 S I=0 F  S I=$O(VFDIN(I)) Q:'I  S X=$$LKUP(VFDIN(I)) Q:X'>0
 Q X
 ;
QAID(KEY,VAL) ;
 Q
 ;
 ;=====================================================================
 ;                         PRIVATE SUBROUTINES
 ;=====================================================================
 ;
LKUP(STR) ; called from FIND1, process one input string at a time
 ; only QUIT if positive value found.  Otherwise keep searching input
 ; values until a unique value is found or you run out of lookup values
 ; EXTRINSIC FUNCTION return value:
 ;   IEN - File 200 ien uniquely matching lookup value
 ;     0 - no unique match found for lookup value
 ;    -1 - unexpected problems encountered
 ;
 ; $P($T(L+i),U,3,99) = p1^p2^p3^p4
 ;   p1 = key_name      p2 = field # which stores lookup value
 ;   p3 = specific index subscript to use in the lookup
 ;   p4 = Boolean flag indicating whether this data element will be
 ;        used where the lookup value is supposed to be unique.
 ;
L ;
 ;;AC^2^A^1
 ;;ACE^2^A^1
 ;;CELL^.137^^
 ;;DEA^53.2^PS1^1
 ;;HOME^.131^^
 ;;LIC^54.1^VFDLIC^1
 ;;NM^.01^B^^
 ;;NPI^41.99^ANPI^1
 ;;OFF^.132^^
 ;;SSN^9^SSN^1
 ;;VA^53.3^PS2^1
 ;;VFD^21600^VFD^1
 ;;VPID^9000^AVPID^1
 ;;ZIP^.116^^
 ;;HL^^^
 ;;
 ;
 N I,J,X,Y,Z,ACTL,DELIM,KEY,VAL,VFLD,VIDX,VIEN,VUNQ
 S VIEN=-1 I $G(STR)="" G LOUT
 S KEY=$P(STR,U),DELIM=$P(STR,U,2) S:DELIM="" DELIM=U
 S ACTL=$P(STR,U,3),VAL=$P(STR,U,4,99)
 I KEY=""!(VAL="") G LOUT
 ;
 I VAL[DELIM,DELIM'=U S VAL=$TR(VAL,DELIM,U)
 F I=1:1:$L(VAL,U) S VAL(I)=$P(VAL,U,I)
 S VIEN="",VFLD=""
 F I=1:1 S X=$P($T(L+I),";;",2) Q:X=""  Q:$P(X,U)=KEY
 I X'="" S VFLD=$P(X,U,2),VIDX=$P(X,U,3),VUNQ=$P(X,U,4)
 ; next two lines deprecated 6/4/2014-sgm
 ;Q:'$D(VFLD) -1
 ;Q:'VUNQ -1
 ;
 ; first process those key terms not needing a FOR loop search
 ; on file 200 whole file index
 I KEY="VPID" S VIEN=+$$IEN^XUPS($P(VAL,U)) G LOUT
 I KEY="NPI" D  S VIEN=+VIEN G LOUT
 . N CNT
 . I '$$CHKDGT^XUSNPI(VAL) Q  ;  validates NPI number
 . S CNT=0,X=$$QI^XUSNPI(VAL) ;  gets all NPIs matching input value
 . F I=1:1:$L(X,";") S Y=$P(X,";",I) I $L(Y) D
 . . I $P(Y,U)="Individual_ID" S CNT=CNT+1,CNT(CNT)=$P(Y,U,2)
 . . Q
 . I CNT=1 S VIEN=CNT(1)
 . Q
 ;
 ; logic extracted VANUM^XUSER2, VA# unique across active users
 I KEY="VA" D  S VIEN=+VIEN G LOUT
 . Q:$L(VAL)<3  Q:$L(VAL)>10  Q:'$D(^VA(200,VIDX,VAL))
 .  S (I,J,Y)=0 F  S I=$O(^VA(200,VIDX,VAL,I)) Q:'I  D
 . . S X=$P(^VA(200,I,"PS"),U,4) I $S('X:1,1:X'<DT) S J=J+1,Y=I
 . . Q
 . I J=1 S VIEN=Y
 . Q
 ;
 ; $O() on index to check for uniqueness
 I "^AC^ACE^DEA^LIC^"[(U_KEY_U),VIDX'="" D  S VIEN=+VIEN G LOUT
 . ; dea logic extracted from VALDEA^XUSER2
 . I KEY="DEA" Q:VAL'?2U7UN  Q:'$$DEANUM^XUSER2(VAL)
 . I KEY="AC" S VAL=$$EN^XUSHSH(VAL)
 . F I=1:1 Q:'$D(VAL(I))  I VAL(I)="" K VAL Q
 . Q:'$D(VAL)  S Z=$NA(^VA(200,VIDX))
 . F I=1:1 S X=$G(VAL(I)) Q:X=""  S Z=$NA(@Z@(VAL(I)))
 . S (I,J)=$O(@Z@(0)),J=$O(@Z@(J)) S:'J VIEN=I
 . Q
 ;
 I "^LIC^VFD^"[(U_KEY_U) S VIEN=+$$OTHER
 ;
LOUT ;
 I VIEN>0,$G(ACT) S Y=$$ACT^VFDCDUZ(VIEN) S:Y'>0 VIEN=0
 Q VIEN
 ;
OTHER() ;
 ; search for alternate IDs or Licenses
 N I,J,L,X,Y,Z,LOC
 I $G(^%ZOSF("ZVX"))'["VX" Q 0
 ; for altid   val(1)=id, val(2)=loc,   val(3)=other data
 ; for license val(1)=id, val(2)=state, val(3)=other data
 I $G(VAL(1))="" Q 0
 I KEY="VFD",VAL(2)'="" D  I 'VAL(2) Q 0
 . N DIERR,LOC,SCR,VERR S LOC=VAL(2)
 . I +LOC S:'$D(^DIC(4,LOC,0)) VAL(2)=0 Q
 . N DIERR,SCR,VERR,VFDX
 . ; copy logic from $$lkup^xuaf4
 . I ACTL S X=+$$LKUP^XUAF4(VFDX) I X>0 S VAL(2)=X Q
 . S SCR=$S('ACTL:"",1:"I $P(^(0),U,11)'=""I""")
 . I 'ACTL S X=$$FIND1^DIC(4,,"MX",LOC,,SCR,"VERR")
 . I X>0!$D(DIERR) S VAL(2)=$S($D(DIERR):0,1:X) Q
 . S X=$$FIND1^DIC(4,,"M",LOC,,SCR,"VERR")
 . S VAL(2)=$S($D(DIERR):0,X>0:X,1:0)
 . Q
 ;
 ; get all records
 S L=0,Z=$NA(^VA(200,VIDX,VAL(1))) ; VIDX="VFDLIC" or "VFD"
 F  S Z=$Q(@Z) Q:Z=""  Q:$QS(Z,1)'=200  Q:$QS(Z,2)'=VIDX  D
 . S Y=$QL(Z) I $S(KEY="LIC":Y'=6,1:Y'=5) Q
 . S I=$QS(Z,Y-1),J=$QS(Z,Y),L=L+1
 . S X=$S(KEY="LIC":"PS1",1:21600)
 . S Y=$G(^VA(200,I,X,J,0)) S:KEY="LIC" $P(Y,U,4)=$G(^(21600))
 . I KEY="LIC" S VFDL(L)=I_U_$P(Y,U)_U_$P(Y,U,4) ; duz^state^idtype
 . I KEY="VFD" S VFDL(L)=I_U_$P(Y,U)_U_$P(Y,U,5) ; duz^loc^other_data
 . Q
 ; if active user flag passed, then screen out all inactive users
 I ACT S I=0 D
 . F  S I=$O(VFDL(I)) Q:'I  I $$ACT^VFDCDUZ(+VFDL(I))<1 K VFDL(I)
 . Q
 I '$D(VFDL) Q 0
 ; if id only unique across VistA
 I VAL(2)="",VAL(3)="" D  Q VIEN
 . S I=$O(VFDL(0)),J=$O(VFDL(I)),VIEN=$S(J:0,1:+VFDL(I))
 . Q
 ; filter vfdl(i)
 S I=0 F  S I=$O(VFDL(I)) Q:'I  S X=VFDL(I) D
 . F J=2,3 I VAL(J)'="",$P(X,U,J)'=VAL(J) K VFDL(I)
 . Q
 I '$D(VFDL) Q 0
 S I=$O(VFDL(0)),J=$O(VFDL(I))
 Q $S(J:0,1:+VFDL(I))
 ;
 ;NOTES
 ;---------------------------------------------------------------------
 ; Index name: A       Field: 2       Field Name: ACCESS CODE
 ;    ^VA(200,"A",<encrypted access code>,DA)
 ;
 ; Index name: B       Field: .01     Field Name: NAME
 ;    ^VA(200,"B",$E(X,1,30),DA) - tranditional
 ;    ^VA(200,"B",$E(X,1,35),DA) - new style
 ;
 ; Index Name: SSN     Field: 9       Field Name: SSN
 ;    ^VA(200,"SSN",ssn,DA)
 ;
 ; Index Name: ANPI    Field: 41.99   Field Name: NPI
 ;    VA NPI
 ;
 ; Index Name: AVPID   Field: 90000   Field Name: VPID
 ;    ^VA(200,"AVPID",X,DA)
 ;
 ; Index Name:  VFD    Field: 21600   Field Name: IDENTIFIER
 ;    ^VA(200,"VFD",identifier,DA(1),DA)
 ;
 ; Index Name: VFDLIC  Field 54.1     Field Name: STATE LICENSE
 ;    ^VA(200,"VFDLIC",license#,<state_file_ptr>,DA(1),DA)
 ;
 ; Fields that should have lookup indexes on but do not:
 ;    .116   ZIP CODE
 ;    .131   HOME TEL
 ;    .132   OFFICE TEL
 ;    .137   VOICE PAGER - ?? put TITLE on field called CELL PHONE
