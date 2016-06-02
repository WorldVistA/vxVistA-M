VFDTMG ;DSS/SMP - MAIN ENTRY TO VFDTMG* ;12/11/2015 12:00
 ;;15.0;DSS,INC VXVISTA OPEN SOURCE;**36**;07 Feb 2014;Build 1
 ;Copyright 1995-2015,Document Storage Systems Inc. All Rights Reserved
 ;
 ;**********************************************************************
 ;*  TMG* is licensed by Dr. Kevin S. Toppenberg under Apache 2.0.     *
 ;*  VFDTMG** is derived from Dr. Toppenberg's TMG software and is     *
 ;*  released  under Apache 2.0.                                       *
 ;*                                                                    *
 ;* (c) 2015 Dr. Kevin S. Toppenberg                                   *
 ;* (c) 2015 DSS, Inc.                                                 *
 ;**********************************************************************
 ;
 Q
 ;
DEMOGET(RESULT,DFN) ; RPC - VFDTMG PATIENT DEMO GET
 ; Cloned from TMG GET PATIENT DEMOGRAPHICS
 ;
 ;"Purpose: To return array with demographcs details about patient
 ;"Input: RESULT (this is the output array)
 ;"       DFN : The record number in file #2 of the patient to inquire about.
 ;"Results: Results passed back in RESULT array.  Format as follows:
 ;"              The results are in format: KeyName=Value,
 ;"              There is no SET order these will appear.
 ;"              Here are the KeyName names that will be provided.
 ;"              If the record has no value, then value will be empty
 ;"      IEN=record#
 ;"      COMBINED_NAME=
 ;"      LNAME=
 ;"      FNAME=
 ;"      MNAME=
 ;"      PREFIX=
 ;"      SUFFIX=
 ;"      DEGREE
 ;"      DOB=
 ;"      SEX=
 ;"      SS_NUM=
 ;"      ADDRESS_LINE_1=
 ;"      ADDRESS_LINE_2=
 ;"      ADDRESS_LINE_3=
 ;"      CITY=
 ;"      STATE=
 ;"      ZIP4=
 ;"      BAD_ADDRESS=
 ;"      TEMP_ADDRESS_LINE_1=
 ;"      TEMP_ADDRESS_LINE_2=
 ;"      TEMP_ADDRESS_LINE_3=
 ;"      TEMP_CITY=
 ;"      TEMP_STATE=
 ;"      TEMP_ZIP4=
 ;"      TEMP_STARTING_DATE=
 ;"      TEMP_ENDING_DATE=
 ;"      TEMP_ADDRESS_ACTIVE=
 ;"      CONF_ADDRESS_LINE_1=
 ;"      CONF_ADDRESS_LINE_2=
 ;"      CONF_ADDRESS_LINE_3=
 ;"      CONF_CITY=
 ;"      CONF_STATE=
 ;"      CONF_ZIP4=
 ;"      CONF_STARTING_DATE=
 ;"      CONF_ENDING_DATE=
 ;"      CONF_ADDRESS_ACTIVE=
 ;"      PHONE_RESIDENCE=
 ;"      PHONE_WORK=
 ;"      PHONE_CELL=
 ;"      PHONE_TEMP=
 ;
 ;"Note, for the following, there may be multiple entries.  # is record number
 ;"      ALIAS # NAME
 ;"      ALIAS # SSN
 ;
 G DEMOGET^VFDTMG01
 Q
 ;
DEMOSET(RESULT,DFN,INFO)  ;" SET PATIENT INFO
 ;"Purpose: To SET demographcs details about patient
 ;"Input: RESULT (this is the output array)
 ;"       DFN : The record number in file #2 of the patient to inquire about.
 ;"       INFO: Format as follows:
 ;"              The results are in format: INFO("KeyName")=Value,
 ;"              There is no SET order these will appear.
 ;"              Here are the KeyName names that will be provided.
 ;"              If the record has no value, then value will be empty
 ;"              If a record should be deleted, its value will be @
 ;"      INFO("COMBINED_NAME")=
 ;"      INFO("PREFIX")=
 ;"      INFO("SUFFIX")=
 ;"      INFO("DEGREE")=
 ;"      INFO("DOB")=
 ;"      INFO("SEX")=
 ;"      INFO("SS_NUM")=
 ;"      INFO("ADDRESS_LINE_1")=
 ;"      INFO("ADDRESS_LINE_2")=
 ;"      INFO("ADDRESS_LINE_3")=
 ;"      INFO("CITY")=
 ;"      INFO("STATE")=
 ;"      INFO("ZIP4")=
 ;"      INFO("BAD_ADDRESS")=
 ;"      INFO("TEMP_ADDRESS_LINE_1")=
 ;"      INFO("TEMP_ADDRESS_LINE_2")=
 ;"      INFO("TEMP_ADDRESS_LINE_3")=
 ;"      INFO("TEMP_CITY")=
 ;"      INFO("TEMP_STATE")=
 ;"      INFO("TEMP_ZIP4")=
 ;"      INFO("TEMP_STARTING_DATE")=
 ;"      INFO("TEMP_ENDING_DATE")=
 ;"      INFO("TEMP_ADDRESS_ACTIVE")=
 ;"      INFO("CONF_ADDRESS_LINE_1")=
 ;"      INFO("CONF_ADDRESS_LINE_2")=
 ;"      INFO("CONF_ADDRESS_LINE_3")=
 ;"      INFO("CONF_CITY")=
 ;"      INFO("CONF_STATE")=
 ;"      INFO("CONF_ZIP4")=
 ;"      INFO("CONF_STARTING_DATE")=
 ;"      INFO("CONF_ENDING_DATE")=
 ;"      INFO("CONF_ADDRESS_ACTIVE")=
 ;"      INFO("PHONE_RESIDENCE")=
 ;"      INFO("PHONE_WORK")=
 ;"      INFO("PHONE_CELL")=
 ;"      INFO("PHONE_TEMP")=
 ;"Note, for the following, there may be multiple entries.  # is record number
 ;"  If a record should be added, it will be marked +1, +2 etc.
 ;"      INFO("ALIAS # NAME")=
 ;"      INFO("ALIAS # SSN")=
 ;"
 ;"Results: Results passed back in RESULT string:
 ;"          1              = success
 ;"          -1^Message     = failure
 ;
 G DEMOSET^VFDTMG01
 Q
