VFDCDUZ ;DSS/SGM - COMMON NEW PERSON FILE RPCS ;06/04/2014 18:08
 ;;2013.0;DSS,INC VXVISTA OPEN SOURCE;;07 Feb 2014;Build 2
 ;Copyright 1995-2014,Document Storage Systems Inc. All Rights Reserved
 ;
 ;this routine contains various common utilities to support access
 ;to the NEW PERSON file and/or user characteristics
 ;
 ;Documentation of Integration Agreements for all VFDCDUZ* routines
 ;DBIA#  Supported References
 ;-----  ------------------------------------------
 ; 1625  $$GET^XUA4A72
 ; 2051  $$FIND1^DIC
 ; 2056  $$GETS^DIQ
 ; 2171  $$NS^XUAF4
 ; 2343  ^XUSER: $$ACTIVE, $$PROVIDER
 ; 2533  $$DIV4^XUSER [controled subscription]
 ; 3065  $$NAMEFMT^XLFNAME
 ;10060  FM read of all fields in file 200
 ;10076  Read of ^XUSEC(key,duz)
 ;10103  $$FMTE^XLFDT
 ;10104  $$UP^XLFSTR
 ;10112  $$SITE^VASITE
 ;
 ;    DESCRIPTION OF SOME INPUT VARIABLES
 ;  DATE - default=today
 ;         Fileman date to check for active person class
 ;VFDSCR - array of additional screens to perform
 ;         VFDSCR(n) = string   where n = 0,1,2,3,4,...
 ;         Allowable formats of VFDSCR(n) = flag^val1^val2^val3^..
 ;          security key ck = KEY^security key name
 ;          kernel param ck = PARM^parameter name^parameter instance
 ;          M code = M^<return msg>^<executable M code which sets $T>
 ;   FUN - I $G(FUN) then extrinsic function, else RPC
 ;   RDV - Boolean flag to indicate whether or not to include remote
 ;         data view visitors - default to 0 to not include
 ;   SCR - same as VFDSCR
 ;  SITE - if SITE=1 and user has no divisions, then return facility's
 ;         default division
 ;  USER - same as XDUZ
 ;   VAL - lookup value for NEW PERSON file
 ;  XDUZ - pointer to NEW PERSON file, if optional default to DUZ
 ;
 ;Return values: see individual line labels for successful return val
 ;  if problems return -1^message
 ;
 ;EDIT HISTORY
 ;01/05/2006 - WLC - add division to return string for LIST
 ;03/02/2006 - SGM - reorganize, break into two routines,
 ;             add - CK as an internal supportable call for DSS apps
 ;01/04/2007 - SGM - add ID
 ;12/25/2013 - sgm - deprecated FUN as an input parameter, use $QUIT
 ;                   Reminder, $QUIT is stacked
 ;01/24/2014 - sgm - added API UNIQUEID
 ;---------------------------------------------------------------------
OUT Q:$Q VFDC Q
 ;
 ;=====================================================================
 ;                       REMOTE PROCEDURE CALLS
 ;=====================================================================
 ;
 ;---------------------------------------------------------------------
 ;VFDC ACTIVE USER                             [Confirm user is active]
 ;---------------------------------------------------------------------
ACT(VFDC,XDUZ,VFDSCR,FUN) S VFDC=$$ACT^VFDCDUZ1 G OUT
 ; RETURN: user's DUZ value or -1^msg
 ; FUN - deprecated 12/26/2013
 ;
 ;---------------------------------------------------------------------
 ;VFDC USER DEF DIV                           [User's Default Division]
 ;---------------------------------------------------------------------
DIV(VFDC,XDUZ,SITE,FUN) S VFDC=$$DIV^VFDCDUZ1 G OUT
 ; rpc does not exist in EXP
 ;Return default division for user.  If that user has only one
 ;division in file 200 DIVISION multiple, then that entry is assumed
 ;to be the default division unless it is explicitly marked as NO
 ;VFDC - return p1^p2^p3
 ;       p1=pointer to file 4  p2=institution name  p3=station number
 ; FUN - deprecated 12/26/2013
 ;
 ;---------------------------------------------------------------------
 ;                   NEW PERSON LOOKUP ON AVFD INDEX
 ;---------------------------------------------------------------------
FIND1(VFDVAL,VFDTYP,VFDLOC) ;
 N VFDC S (VFDC,VFDVAL)=$G(VFDVAL),VFDTYP=$G(VFDTYP),VFDLOC=$G(VFDLOC)
 G:VFDC="" OUT S (VFDC,VFDVAL)=$$FIND1^VFDCDUZ1()
 G OUT
 ; Extrinsic Functon returns File 200 IEN or <null> [exact match]
 ; Input Parameters:
 ; VFDVAL - req - Alternate ID value to look up
 ; VFDTYP - opt - Type of value (OTHER DATA #.05 subfield)
 ;                Default is ALL types
 ; VFDLOC - opt - LOCATION IEN (#.01 subfield - DINUM'ed to File 4)
 ;                Default is ALL locations
 ;
 ;---------------------------------------------------------------------
 ;VFDC USER ID                         [One or More User's Identifiers]
 ;---------------------------------------------------------------------
ID(VFDC,XDUZ,FLAGS,FUN) S FUN=$Q D ID^VFDCDUZ1 G OUT
 ; rpc does not exist in EXP
 ; Return all user IDs for a given user
 ; FLAGS - opt - default to AaDNSTVv
 ;   FLAGS["A" - return alternate IDS in field 21600
 ;         "a" - return default alternate ID only - either one must
 ;               be flagged as default or if there is only one entry
 ;               in alt id
 ;         "D" - return DEA#
 ;         "N" - return NPI#
 ;         "S" - return SSN
 ;         "T" - TAX ID
 ;         "v" - VA#
 ;         "V" - VPID
 ;         "Z" - added 12/26/2013 - support for ID1^VFDCDUZ
 ;               Backward compatiblity: no Z then return has 4 pieces
 ; Return VFDC(n) = p1^p2^p3^p4[^p5^p6^p7] for n=1,2,3,... where
 ;         p1 - ID mnemonic
 ;         p2 - ID value
 ;         p3 - location (valid for OAI mnemonics only)
 ;         p4 - 1 (valid for OAI only.  If 1, then default Alt ID)
 ;         If FLAGS["Z" then return also p5^p6^p7^p8 [12/26/2013]
 ;         p5=expiration date (internal)
 ;         p6 - type   p7=license state   p8=license type
 ; If called as extrinsic function, then return
 ;    -1^message if problems, or VFDC() as a ';'-delimited string
 ; If called as DO with parameters, then return
 ;    if problems, VFDC(1) = -1^message
 ;    else return VFDC(j)
 ;
 ;---------------------------------------------------------------------
 ;VFDC ACTIVE USER LIST                          [List of Active Users]
 ;---------------------------------------------------------------------
LIST(VFDC,VAL,SCR) D LIST^VFDCDUZ1 Q
 ; Return list of active users for a lookup value
 ; Return ^TMP("VFDC",$J,"DILIST",#,0) = p1^p2^p3^...^p8  where
 ;    p1 = ien                      p6 = initials
 ;    p2 = name (.01 field)         p7 = title
 ;    p3 = sig block printed name   p8 = service
 ;    p4 = sig block title          p9 = division
 ;    p5 = first m last
 ;
 ;---------------------------------------------------------------------
 ;VFDC ACTIVE PERSON CLASS                        [User's Person Class]
 ;---------------------------------------------------------------------
PER(VFDC,USER,DATE,FUN) S VFDC=$$PER^VFDCDUZ1 G OUT
 ; rpc does not exist in EXP
 ; Return user's current active person classification for PCE
 ; Return p1^p2^p3^...^p8  where
 ;    p1 = IEN to file 8932.1     p5 = Effective date
 ;    p2 = Occupation             p6 = expiration date
 ;    p3 = specialty              p7 = VA Code
 ;    p4 = sub-specialty          p8 = specialty code
 ;
 ;---------------------------------------------------------------------
 ;VFDC ACTIVE CPRS PROVIDER           [Is User an Active CPRS Provider]
 ;---------------------------------------------------------------------
PROV(VFDC,XDUZ,RDV,FUN) S VFDC=$$PROV^VFDCDUZ1 G OUT
 ; rpc does not exist in EXP
 ; Return: 3 if active user
 ;         2 if user is active via the XUORES security key
 ;         1 if user is a RDV visitor and you passed RDV=1
 ;         0 if user is a RDV visitor and you passed RDV=0
 ;        -1^message if problems or not a provider
 ; FUN deprecated 12/26/2013
 ;
 ;=====================================================================
 ;              APPLICATION PROGRAMMER INTERFACE (APIs)
 ;=====================================================================
 ;
 ;---------------------------------------------------------------------
 ;                Confirm NEW PERSON File IEN Is Valid
 ;---------------------------------------------------------------------
CK(XDUZ) Q $$CK^VFDCDUZ1(XDUZ)
 ; RETURN 1 if file 200 entry exists, else 0
 ;
 ;---------------------------------------------------------------------
 ;                         Clone A Single User
 ;---------------------------------------------------------------------
CLONE(VFDFR,VFDTO,VFDAA) D CLONE^VFDCDUZ3 Q
 ; VFDFR - req - ien/name of NEW PERSON file entry to be cloned from
 ; VFDTO - req - ien NEW PERSON file entry to receive clone attributes
 ; .VFDA - opt - additional fields to be filed to VFDTO record
 ;
 ;---------------------------------------------------------------------
 ;             One or More User's Identifiers Plus DPS/NPI
 ;---------------------------------------------------------------------
ID1(VFDRSLT,VFDDUZ) D ID1^VFDCDUZ1 Q
 ; Wraps VFDC USER ID and appends additional data for DPS and NPI
 ;   VFDRSLT - Req - Return array name or reference
 ;    VFDDUZ - Req - NEW PERSON internal entry number
 ; RETURN FORMAT :  See ID^VFDCDUZ
 ; ; ^-piece  1 will have the ID mnemonic
 ;   ^-piece  2 will have ID value
 ;   ^-pieces 3,4    see ID^VFDCDUZ
 ; The NPI value will be either:
 ;   1. Value of field 41.99
 ;   2. Value from ALTERNATE ID multiple, in that precedence order
 ; DPS value from the ALTERNATE ID
 ; Code from ID^VFDDUZ moved to this routine vxVistA 2010.1.1 
 ;
 ;---------------------------------------------------------------------
 ;                     Terminate A User (Scheduled)
 ;---------------------------------------------------------------------
TERM(DA,DATE) D TERM^VFDCDUZ3 Q
 ;   DA - req - ien to file 200
 ; DATE - opt - Fileman or human readable date to terminate a user
 ;              Default to DT+1
 ;
 ;---------------------------------------------------------------------
 ;                      Terminate A User (Direct)
 ;---------------------------------------------------------------------
TERM1(DA) D TERM1^VFDCDUZ3 Q
 ; DA - req - ien to file 200
 ;
 ;---------------------------------------------------------------------
 ;                   Find One Entry Using ID Lookups
 ;---------------------------------------------------------------------
UNIQUEID(VFDC,VFDIN,ACT) ;
 N X S VFDC=+$$FIND1^VFDCDUZ2(.VFDIN) G OUT
 ; This was initially written for INTERFACES that will be importing new
 ; entries into file 200 from an external source.  The one immutable
 ; assumption is that the external system has some form of identifier
 ; that will uniquely identify an individual across all of VistA.
 ; Of course this could be used by anyone with a unique identifier
 ; lookup need.
 ; RETURN:
 ;   If UID exists, return file_200_ien
 ;   If UID does not exist, return 0
 ;   If problems return -1
 ; INPUT PARAMETERS:
 ;   ACT - opt - Boolean, if true screen out inactive users
 ;  .VFDIN(j) = p1^p2^p3 where j=1,2,3,4,...
 ;   IF VFDIN=p1^p2^p3 then ignore all VFDIN(j) values and only
 ;      process the one input value
 ;    p1: KEY_TERM
 ;    p2: DELIMITER, for most cases p2 should be <null>.
 ;        Default is '^'; if the delimiter is '^' do not pass it in.
 ;        Delimiter separates the components of the input value passed
 ;        in p3.
 ;    p3: LOOKUP VALUE
 ;        If lookup will use a compound index, then p2 will be the
 ;        delimiter that separates the components of the input value.
 ;    Lookup done in the sequence order of the input array VFDIN.  The
 ;    first find of a unique record is returned and any remaining
 ;    lookup values are not tried.
 ; For additional documentation see FIND1^VFDCDUZ2
