LEX2070A ;ISL/KER - LEX*2.0*70 Pre/Post Install ;06/09/2010
 ;;2.0;LEXICON UTILITY;**70**;Sep 23, 1996;Build 2
 ;               
 ; Global Variables
 ;    ^DIC(81.3,          ICR   4492
 ;               
 ; External References
 ;    BMES^XPDUTL         ICR  10141
 ;               
 ; Local Variables NEWed or KILLed Elsewhere
 ;    None
 ;               
EN ; Main Entry Point
 N LEXLI,LEXRI S LEXLI=$O(^DIC(81.3,"B","LT",0)) Q:+LEXLI'>0  S LEXRI=$O(^DIC(81.3,"B","RT",0)) Q:+LEXRI'>0
 D BMES^XPDUTL(" Updating CPT Ranges for Modifiers LT and RT")
 D DR^LEX2070C(LEXLI),DR^LEX2070C(LEXRI),EN^LEX2070B,RR^LEX2070D(LEXLI),RR^LEX2070D(LEXRI)
 Q 
LR ;; Left/Right
 ;;LR;;0002T;;0002T;;01/01/2002;;01/01/2004
 ;;LR;;0005T;;0006T;;01/01/2002;;01/01/2003
 ;;LR;;0005T;;0005T;;01/01/2003;;01/01/2005
 ;;LR;;0006T;;0006T;;01/01/2003;;01/01/2005
 ;;LR;;0012T;;0013T;;01/01/2002;;01/01/2005
 ;;LR;;0014T;;0014T;;01/01/2002;;01/01/2005
 ;;LR;;0016T;;0016T;;01/01/2002;;01/01/2005
 ;;LR;;0017T;;0017T;;01/01/2002;;01/01/2005
 ;;LR;;0019T;;0020T;;01/01/2002;;01/01/2003
 ;;LR;;0037T;;0037T;;01/01/2003;;01/01/2005
 ;;LR;;0213T;;0214T;;01/01/2010
 ;;LR;;0216T;;0218T;;01/01/2010
 ;;LR;;11010;;11044;;01/01/1999
 ;;LR;;11300;;11463;;01/01/1999
 ;;LR;;11600;;11646;;01/01/1999
 ;;LR;;11730;;11765;;01/01/2003
 ;;LR;;11740;;11765;;01/01/1999;;01/01/2003
 ;;LR;;11960;;11971;;01/01/1999
 ;;LR;;14020;;14350;;01/01/1999
 ;;LR;;15050;;15050;;01/01/1999
 ;;LR;;15572;;15580;;01/01/1999;;01/01/2001
 ;;LR;;15572;;15576;;01/01/2002
 ;;LR;;15610;;15630;;01/01/1999
 ;;LR;;15732;;15732;;01/01/1999
 ;;LR;;15736;;15738;;01/01/1999
 ;;LR;;15820;;15823;;01/01/1999;;01/01/2005
 ;;LR;;15820;;15829;;01/01/2005
 ;;LR;;15825;;15825;;01/01/1999;;01/01/2005
 ;;LR;;15828;;15829;;01/01/1999;;01/01/2005
 ;;LR;;15832;;15837;;01/01/1999
 ;;LR;;15840;;15845;;01/01/1999
 ;;LR;;15878;;15879;;01/01/1999
 ;;LR;;15940;;15958;;01/01/1999
 ;;LR;;17260;;17286;;01/01/1999
 ;;LR;;19000;;19240;;01/01/1999
 ;;LR;;19290;;19350;;01/01/1999;;01/01/2005
 ;;LR;;19290;;19396;;01/01/2005
 ;;LR;;19357;;19368;;01/01/1999;;01/01/2005
 ;;LR;;19370;;19396;;01/01/1999;;01/01/2005
 ;;LR;;20100;;20100;;01/01/2005
 ;;LR;;20103;;20150;;01/01/2005
 ;;LR;;2010F;;2014F;;01/01/2005
 ;;LR;;20220;;20245;;01/01/2005
 ;;LR;;2022F;;2022F;;01/01/2005
 ;;LR;;20526;;20610;;01/01/2002;;01/01/2003
 ;;LR;;20526;;20551;;01/01/2003
 ;;LR;;20550;;20610;;01/01/1999
 ;;LR;;20600;;20612;;01/01/2003
 ;;LR;;20650;;20650;;01/01/1999
 ;;LR;;20663;;20663;;01/01/1999
 ;;LR;;20670;;20838;;01/01/1999
 ;;LR;;20931;;20931;;01/01/2005
 ;;LR;;20937;;20938;;01/01/2005
 ;;LR;;20955;;20957;;01/01/2005
 ;;LR;;20970;;20973;;01/01/2005
 ;;LR;;20982;;21010;;01/01/2005
 ;;LR;;21010;;21010;;01/01/1999;;01/01/2005
 ;;LR;;21025;;21031;;01/01/1999
 ;;LR;;21034;;21070;;01/01/2005
 ;;LR;;21034;;21070;;01/01/1999;;01/01/2003
 ;;LR;;21034;;21040;;01/01/2003;;01/01/2005
 ;;LR;;21044;;21070;;01/01/2003;;01/01/2005
 ;;LR;;21077;;21077;;01/01/1999
 ;;LR;;21081;;21081;;01/01/1999
 ;;LR;;21086;;21086;;01/01/1999
 ;;LR;;21116;;21116;;01/01/1999
 ;;LR;;21125;;21127;;01/01/1999
 ;;LR;;21198;;21198;;01/01/1999
 ;;LR;;21208;;21243;;01/01/1999
 ;;LR;;21247;;21247;;01/01/1999
 ;;LR;;21255;;21270;;01/01/1999
 ;;LR;;21280;;21296;;01/01/1999
 ;;LR;;21355;;21408;;01/01/1999
 ;;LR;;21440;;21490;;01/01/1999
 ;;LR;;21501;;21501;;01/01/1999
 ;;LR;;21600;;21616;;01/01/1999
 ;;LR;;21700;;21725;;01/01/1999
 ;;LR;;21800;;21810;;01/01/1999
 ;;LR;;22216;;22216;;01/01/2005
 ;;LR;;22226;;22226;;01/01/2005
 ;;LR;;23000;;23921;;01/01/1999;;01/01/2005
 ;;LR;;23000;;26550;;01/01/2005
 ;;LR;;23930;;24940;;01/01/1999;;01/01/2005
 ;;LR;;25000;;25931;;01/01/1999;;01/01/2005
 ;;LR;;26010;;26550;;01/01/1999;;01/01/2005
 ;;LR;;26553;;26952;;01/01/1999;;01/01/2003
 ;;LR;;26553;;26580;;01/01/2003;;01/01/2005
 ;;LR;;26553;;27079;;01/01/2005
 ;;LR;;26587;;26596;;01/01/2003;;01/01/2005
 ;;LR;;26600;;26952;;01/01/2003;;01/01/2005
 ;;LR;;26990;;27079;;01/01/1999;;01/01/2005
 ;;LR;;27086;;27156;;01/01/1999
 ;;LR;;27161;;27187;;01/01/1999;;01/01/2005
 ;;LR;;27161;;27193;;01/01/2005
 ;;LR;;27215;;27215;;01/01/1999
 ;;LR;;27220;;27280;;01/01/1999
 ;;LR;;27284;;27295;;01/01/1999;;01/01/2005
 ;;LR;;27284;;27390;;01/01/2005
 ;;LR;;27301;;27390;;01/01/1999;;01/01/2005
 ;;LR;;27393;;27393;;01/01/1999
 ;;LR;;27396;;27598;;01/01/1999;;01/01/2005
 ;;LR;;27396;;28825;;01/01/2005
 ;;LR;;27600;;27894;;01/01/1999;;01/01/2005
 ;;LR;;28001;;28825;;01/01/1999;;01/01/2005
 ;;LR;;29065;;29131;;01/01/1999
 ;;LR;;29240;;29280;;01/01/1999
 ;;LR;;29345;;29515;;01/01/1999
 ;;LR;;29530;;29705;;01/01/1999;;01/01/2005
 ;;LR;;29530;;29710;;01/01/2005
 ;;LR;;29730;;29750;;01/01/1999;;01/01/2005
 ;;LR;;29730;;29902;;01/01/2005
 ;;LR;;29800;;29898;;01/01/1999
 ;;LR;;29800;;29902;;01/01/2002;;01/01/2003
 ;;LR;;29800;;29807;;01/01/2003;;01/01/2005
 ;;LR;;29819;;29902;;01/01/2003;;01/01/2005
 ;;LR;;30100;;30118;;01/01/1999
 ;;LR;;30130;;30140;;01/01/1999
 ;;LR;;30200;;30210;;01/01/1999
 ;;LR;;3020F;;3020F;;01/01/1999
 ;;LR;;30300;;30320;;01/01/1999
 ;;LR;;30560;;30560;;01/01/1999
 ;;LR;;30901;;30930;;01/01/1999;;01/01/2005
 ;;LR;;30901;;31230;;01/01/2005
 ;;LR;;31000;;31230;;01/01/1999;;01/01/2005
 ;;LR;;31233;;31294;;01/01/1999
 ;;LR;;31595;;31595;;01/01/1999
 ;;LR;;31623;;31631;;01/01/2005
 ;;LR;;31635;;31635;;01/01/2005
 ;;LR;;31638;;31656;;01/01/2005
 ;;LR;;31708;;31715;;01/01/2005
 ;;LR;;32000;;32000;;01/01/1999;;01/01/2003
 ;;LR;;32000;;32151;;01/01/2003;;01/01/2005
 ;;LR;;32000;;32005;;01/01/2005
 ;;LR;;32002;;32151;;01/01/1999;;01/01/2003
 ;;LR;;32020;;32151;;01/01/2005
 ;;LR;;32200;;32420;;01/01/1999
 ;;LR;;32491;;32491;;01/01/2005
 ;;LR;;32501;;32501;;01/01/2005
 ;;LR;;32664;;32664;;01/01/2002
 ;;LR;;32900;;32960;;01/01/1999
 ;;LR;;33141;;33141;;01/01/2005
 ;;LR;;33508;;33508;;01/01/2005
 ;;LR;;34001;;34501;;01/01/1999
 ;;LR;;34510;;34530;;01/01/2005
 ;;LR;;34530;;35045;;01/01/1999;;01/01/2001
 ;;LR;;34530;;34530;;01/01/2001;;01/01/2005
 ;;LR;;34808;;34812;;01/01/2001
 ;;LR;;34820;;34820;;01/01/2001
 ;;LR;;34833;;34834;;01/01/2003;;01/01/2004
 ;;LR;;35001;;35045;;01/01/2001;;01/01/2004
 ;;LR;;34833;;35045;;01/01/2004
 ;;LR;;35091;;35152;;01/01/2005
 ;;LR;;35121;;35162;;01/01/1999;;01/01/2005
 ;;LR;;35184;;35184;;01/01/1999
 ;;LR;;35190;;35207;;01/01/1999;;01/01/2005
 ;;LR;;35190;;35480;;01/01/2005
 ;;LR;;35226;;35236;;01/01/1999;;01/01/2005
 ;;LR;;35256;;35266;;01/01/1999;;01/01/2005
 ;;LR;;35286;;35321;;01/01/1999;;01/01/2005
 ;;LR;;35341;;35355;;01/01/1999;;01/01/2005
 ;;LR;;35371;;35450;;01/01/1999;;01/01/2005
 ;;LR;;35454;;35471;;01/01/1999;;01/01/2005
 ;;;;;;;;;;