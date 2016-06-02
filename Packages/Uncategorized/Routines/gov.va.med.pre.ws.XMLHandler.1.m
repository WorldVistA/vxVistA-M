 ;gov.va.med.pre.ws.XMLHandler.1
 ;(C)InterSystems, generated for class gov.va.med.pre.ws.XMLHandler.  Do NOT edit. 06/02/2016 02:21:23PM
 ;;52326962;gov.va.med.pre.ws.XMLHandler
 ;
zgetHandle(docHandle,xmlGlobalAddress)
	; call the MXMLDOM entry point
 	SET docHandle=$$EN^MXMLDOM(xmlGlobalAddress,"W")
 	IF docHandle = 0 QUIT "0^Unable to get handle to XML document."
 	IF docHandle > 0 QUIT "1^Document handle returned succesfully."
	Quit
zgetHandleForTest(docHandle,xmlGlobalAddress)
	; call the MXMLDOM entry point
 	SET docHandle=$$EN^MXMLDOM(xmlGlobalAddress,"W")
 	IF docHandle = 0 QUIT "0^Unable to get handle to XML document."
 	IF docHandle > 0 QUIT "1^Document handle returned succesfully."
	Quit
zgetHandleToXmlDoc(XMLAsStream,docHandle)
	NEW xmlGlobalAddress,rslt
	// The MXMLDOM api will read the xml in from this global
	SET xmlGlobalAddress = $NA(^TMP($JOB,"OUT XML"))
	XECUTE "KILL "_xmlGlobalAddress
	// store in global readable to MXMLDOM
	SET rslt = ##class(gov.va.med.pre.ws.XMLHandler).storeXMLInGlobal(XMLAsStream, xmlGlobalAddress)
	// store handle in parameter (passed by ref.)
	SET rslt = ##class(gov.va.med.pre.ws.XMLHandler).getHandle(.docHandle, xmlGlobalAddress)
    QUIT rslt
zparseAndStoreInGlobal(count,xml,xmlGlobalAddress)
 ;
 ;xml This xml string must be parsed into an array that is usable by
 ;    MXMLDOM API routines. 
 ;    I.E. "<ROOT><SUBROOT></SUBROOT></ROOT>" must be converted to 
 ;     		ARRAY(0)="<ROOT>"
 ;     		ARRAY(1)="<SUBROOT>"
 ;     		ARRAY(2)="</SUBROOT>"
 ;     		ARRAY(3)="</ROOT>"
 ; 
 NEW frontEnd,charIndex,exe,tmpGlobalAddress
 NEW remainingXML
 ;
 ; Piece the global address for XECUTE function
 SET tmpGlobalAddress = $PIECE(xmlGlobalAddress,")",1)
 SET remainingXML = xml
 ;
 FOR count=count:1 QUIT:remainingXML'[">"  DO
 . SET charIndex=$FIND(remainingXML,">")
 . SET frontEnd=$EXTRACT(remainingXML,1,charIndex-1)
 . SET remainingXML=$EXTRACT(remainingXML,charIndex,$LENGTH(remainingXML))
 . ;
 . ; Store valid xml fraction to global node
 . SET exe="SET "_tmpGlobalAddress_",count)=frontEnd"
 . XECUTE exe
 QUIT remainingXML
zstoreXMLInGlobal(XMLAsStream,xmlGlobalAddress)
	NEW xmlFraction,xml,count,maxString
	SET xml = ""
	// Single String length limit for cache is 32767 as of 
	// version 5.0. Set max read in length to less than 
	// string length limit.
	SET maxString = 15000
	SET count = 0
	// Iterate through stream
	While (XMLAsStream.AtEnd = 0) {
		SET xmlFraction = XMLAsStream.Read(.maxString)
        SET xml = xml_xmlFraction
        // function will return xml portion left 
        // after parsing xml. 
        SET xml = ##class(gov.va.med.pre.ws.XMLHandler).parseAndStoreInGlobal(.count, xml, xmlGlobalAddress)
    }
    QUIT 1
