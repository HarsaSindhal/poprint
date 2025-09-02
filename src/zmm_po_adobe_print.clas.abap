CLASS zmm_po_adobe_print DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun .

    CLASS-DATA : access_token TYPE string .
    CLASS-DATA : xml_file TYPE string .
    TYPES :
      BEGIN OF struct,
        xdp_template TYPE string,
        xml_data     TYPE string,
        form_type    TYPE string,
        form_locale  TYPE string,
        tagged_pdf   TYPE string,
        embed_font   TYPE string,
      END OF struct.

    CLASS-METHODS :
      create_client
        IMPORTING url           TYPE string
        RETURNING VALUE(result) TYPE REF TO if_web_http_client
        RAISING   cx_static_check ,
      read_posts
        IMPORTING VALUE(purchaseorder) TYPE string
                  VALUE(manual)        TYPE string OPTIONAL
*                  VALUE(year)     TYPE string

        RETURNING VALUE(result12)      TYPE string
        RAISING   cx_static_check .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS lc_ads_render TYPE string VALUE '/ads.restapi/v1/adsRender/pdf'.
*    CONSTANTS  lv1_url    TYPE string VALUE 'https://adsrestapi-formsprocessing.cfapps.eu10.hana.ondemand.com/v1/adsRender/pdf?templateSource=storageName&TraceLevel=2'  .
*    CONSTANTS  lv2_url    TYPE string VALUE 'https://btp-b3lap1je.authentication.eu10.hana.ondemand.com'  .
    CONSTANTS lc_storage_name TYPE string VALUE 'templateSource=storageName'.
    CONSTANTS lc_template_name TYPE string VALUE 'ZPURCHASE_ORDER_PRINT/ZPURCHASE_ORDER_PRINT'.

ENDCLASS.



CLASS ZMM_PO_ADOBE_PRINT IMPLEMENTATION.


  METHOD create_client .
    DATA(dest) = cl_http_destination_provider=>create_by_url( url ).
    result = cl_web_http_client_manager=>create_by_http_destination( dest ).
  ENDMETHOD .


  METHOD if_oo_adt_classrun~main.
*    DATA(test)  = read_posts( matdoc = '0000000000'  year = ' ' )  .
  ENDMETHOD.


  METHOD read_posts .

    DATA lv_xml TYPE string.
    DATA xsml TYPE string.
    DATA n TYPE n VALUE 0 .
*    DATA other TYPE string.
    DATA loding TYPE string.
    DATA insurance TYPE string.
    DATA unit TYPE string.
    DATA sumdis TYPE string.
    DATA sumbasic TYPE string.
    DATA discount3 TYPE p DECIMALS 2.
    DATA ig TYPE p DECIMALS 0.


    IF purchaseorder IS INITIAL.
      purchaseorder = '2100000045'.
    ENDIF.
    SELECT * FROM i_purchaseorderapi01 WITH PRIVILEGED ACCESS AS a
             LEFT JOIN i_purchaseorderitemapi01 WITH PRIVILEGED ACCESS AS b ON ( b~purchaseorder = a~purchaseorder )
             LEFT JOIN i_supplier WITH PRIVILEGED ACCESS AS c ON ( c~supplier = a~supplier )
             WHERE a~purchaseorder = @purchaseorder  INTO TABLE  @DATA(it) .
    IF sy-subrc = 0 AND (  cl_abap_context_info=>get_user_description(  )  =  'SAP Workflow Runtime'   OR  manual = 'X' ) .
      SORT it  ASCENDING BY  a-purchaseorder.
*    DELETE ADJACENT DUPLICATES FROM it COMPARING a-purchaseorder  .
*    DELETE it WHERE A- NE matdoc .
      READ TABLE it INTO DATA(wa) INDEX 1.

      SELECT SINGLE paymenttermsdescription FROM i_paymenttermstext WITH PRIVILEGED ACCESS  WHERE paymentterms = @wa-a-paymentterms AND language = 'E' INTO @DATA(pterms).
      SELECT SINGLE personfullname FROM i_businessuserbasic WITH PRIVILEGED ACCESS WHERE userid = @wa-a-createdbyuser INTO @DATA(prepaid) .
      SELECT SINGLE * FROM i_purorditmpricingelementapi01 WITH PRIVILEGED ACCESS  WHERE  purchaseorder  = @wa-a-purchaseorder AND purchaseorderitem = @wa-b-purchaseorderitem INTO @DATA(freight1) .
      SELECT SINGLE conditiontype , conditionamount FROM i_purorditmpricingelementapi01 WITH PRIVILEGED ACCESS WHERE  purchaseorder  = @wa-b-purchaseorder AND purchaseorderitem = @wa-b-purchaseorderitem AND conditiontype = 'ZR00' INTO @DATA(discount) .
      SELECT SINGLE conditiontype , conditionamount FROM i_purorditmpricingelementapi01 WITH PRIVILEGED ACCESS WHERE purchaseorder = @wa-a-purchaseorder AND purchaseorderitem = @wa-b-purchaseorderitem
       AND ( conditiontype = 'ZBRO' OR conditiontype = 'ZBR1' OR conditiontype = 'ZBR2' ) INTO @DATA(pf).
      SELECT SINGLE conditiontype , conditionamount FROM i_purorditmpricingelementapi01 WITH PRIVILEGED ACCESS WHERE  purchaseorder  = @wa-b-purchaseorder AND purchaseorderitem = @wa-b-purchaseorderitem
      AND ( conditiontype = 'ZFB1' OR conditiontype = 'ZFA1' OR conditiontype = 'ZFC1' OR conditiontype = 'ZFA3' OR conditiontype = 'ZFB4' ) INTO @DATA(freight) .
      SELECT SINGLE conditiontype , conditionamount FROM i_purorditmpricingelementapi01 WITH PRIVILEGED ACCESS WHERE purchaseorder = @wa-b-purchaseorder AND conditiontype = 'ZJEX' INTO  @DATA(totaltaxpacking).
      SELECT SINGLE conditiontype , freightsupplier FROM i_purorditmpricingelementapi01 WITH PRIVILEGED ACCESS WHERE purchaseorder = @wa-b-purchaseorder AND purchaseorderitem = @wa-b-purchaseorderitem
      AND ( conditiontype = 'ZBR0' OR conditiontype = 'ZBR1' OR conditiontype = 'ZBR2' ) INTO @DATA(sup) ..

      SELECT SINGLE * FROM zsupplier_details WITH PRIVILEGED ACCESS WHERE supplier = @wa-c-supplier INTO @DATA(email).
      SELECT SINGLE suppliername FROM i_supplier WITH PRIVILEGED ACCESS WHERE supplier = @sup-freightsupplier INTO @DATA(supname) .
      SELECT SINGLE schedulelinedeliverydate FROM i_purchaseordschedulelinetp_2 WITH PRIVILEGED ACCESS WHERE purchaseorder = @wa-b-purchaseorder AND purchaseorderitem = @wa-b-purchaseorderitem INTO @DATA(deldat).
      SELECT SINGLE conditiontype, conditionamount  FROM i_purorditmpricingelementapi01 WITH PRIVILEGED ACCESS WHERE  purchaseorder  = @wa-b-purchaseorder AND
       purchaseorderitem = @wa-b-purchaseorderitem AND conditiontype  = 'ZOT2'  INTO @DATA(other) .
      SELECT SINGLE conditiontype, conditionamount  FROM i_purorditmpricingelementapi01 WITH PRIVILEGED ACCESS WHERE  purchaseorder  = @wa-b-purchaseorder AND purchaseorderitem = @wa-b-purchaseorderitem
       AND ( conditiontype  = 'ZBR1' OR conditiontype  = 'ZBR2' OR conditiontype  = 'ZBR0' ) INTO @DATA(brokrage) .
      SELECT SINGLE * FROM  zmm_supplier_addr WITH PRIVILEGED ACCESS WHERE supplier = @wa-a-supplier INTO @DATA(add1).
      SELECT SINGLE * FROM i_purchaseordernotetp_2 WITH PRIVILEGED ACCESS  WHERE purchaseorder = @wa-a-purchaseorder INTO @DATA(spic).
      IF freight1-conditiontype = 'ZLOD' .
        loding = freight1-conditionamount.
      ELSEIF
      freight1-conditiontype = 'ZIN1' .
        insurance = freight1-conditionamount .

      ENDIF.


      DATA tras1 TYPE p DECIMALS 2.
      DATA tras2 TYPE p DECIMALS 2.
      DATA moise1 TYPE p DECIMALS 2.
      DATA bales TYPE string.
      DATA rateunit TYPE string .

*      tras1 = wa-b-yy1_trash_pdi.
*      tras2 = wa-b-yy1_trash_to_pdi.
*      moise1 = wa-b-yy1_moisture_pdi.

      IF wa-b-purchaseorderquantityunit = 'ZBL'.
        bales = wa-b-orderquantity .
      ELSEIF
      wa-b-purchaseorderquantityunit <> 'ZBL'.
        bales = ''.
      ENDIF.

      IF wa-b-orderpriceunit = 'ZCN'.
        rateunit =  'KCN'.
      ELSEIF
      wa-b-orderpriceunit = 'KMN'.
        rateunit = 'KMN'.

      ENDIF.


      DATA lc_template_name1 TYPE string .

      lc_template_name1 = 'ZPURCHASE_ORDER_PRINT/ZPURCHASE_ORDER_PRINT'.


      lv_xml =
      |<Form>| &&
      |<bdyMain>| &&
      |<purchaseorder>{ wa-a-purchaseorder }</purchaseorder>| &&
      |<Supplier>{ wa-a-supplier }</Supplier>| &&
      |<AddressLine1>{ wa-c-suppliername }</AddressLine1>| &&
      |<AddressLine2>{ add1-careofname }</AddressLine2>| &&
      |<AddressLine3>{ add1-streetprefixname1 }</AddressLine3>| &&
      |<AddressLine4>{ add1-streetprefixname2 }</AddressLine4>| &&
      |<AddressLine5>{ add1-city }{ ',' }{ add1-post }</AddressLine5>| &&
      |<GSTIN>{ wa-c-taxnumber3 }</GSTIN>| &&
      |<SupplierMobileNumber>{ wa-c-phonenumber1 }</SupplierMobileNumber>| &&
      |<SupplierEmailAddress>{ email-emailaddress }</SupplierEmailAddress>| &&
      |<Supplierpan.>{ wa-c-businesspartnerpannumber }</Supplierpan.>| &&
      |<statecode>{ wa-c-region }</statecode>| &&
      |<TextField10></TextField10>| &&
      |<DeliveryDate>{ deldat }</DeliveryDate>| &&
      |<Email></Email>| &&
      |<OfficeAddress>SUDIVA SPINNERS PVT. LTD.</OfficeAddress>| &&
      |<purchaseorderdate>{ wa-a-purchaseorderdate }</purchaseorderdate>| &&
*      |<Length>{ wa-b-yy1_length_pdi } - { wa-b-yy1_length1_pdi } </Length>| &&
      |<Trash>{ tras1 } - { tras2 }</Trash>| &&
*      |<RD>{ wa-b-yy1_rdcolorref_pdi } - { wa-b-yy1_rd_to_pdi }</RD>| &&
*      |<MIC>{ wa-b-yy1_micmicronairevalue_pdi } - { wa-b-yy1_mic_to_pdi }</MIC>| &&
*      |<MOICE>{ moise1 } - { wa-b-yy1_moisture_to_pdi }</MOICE>| &&
      |<CANCLERATE>{ wa-b-netpriceamount } { rateunit }</CANCLERATE>| &&
      |<NOOFBILL>{ bales }</NOOFBILL>| &&
      |<BROKER>{ supname }</BROKER>| &&
      |<Lotno>{ wa-b-requirementtracking }</Lotno>| &&
      |<orderdescription></orderdescription>|  .


      DATA:igst TYPE p DECIMALS 2,
           cgst TYPE p DECIMALS 2,
           sgst TYPE p DECIMALS 2.
      DATA:igst_rate TYPE p DECIMALS 1,
           cgst_rate TYPE p DECIMALS 1,
           sgst_rate TYPE p DECIMALS 1.
      DATA basicnew1 TYPE string.
      DATA taxnew1 TYPE string.
      DATA disnew1 TYPE string.
      DATA othnew1 TYPE string.
      DATA lodnew1 TYPE string.
      DATA insnew1 TYPE string.
      DATA netprice TYPE p DECIMALS 5.

      LOOP AT it INTO DATA(iv) .
        n = n + 1 .
        SELECT SINGLE conditionamount,conditiontype FROM i_purordpricingelementtp_2 WITH PRIVILEGED ACCESS WHERE purchaseorder = @iv-b-purchaseorder
      AND purchaseorderitem = @iv-b-purchaseorderitem AND conditiontype = 'ZPBX' INTO @DATA(basicnew).
        SELECT SINGLE conditionamount,conditiontype FROM i_purordpricingelementtp_2 WITH PRIVILEGED ACCESS  WHERE purchaseorder = @iv-b-purchaseorder
        AND purchaseorderitem = @iv-b-purchaseorderitem
        AND ( conditiontype = 'ZR00' OR conditiontype = 'ZR01' OR conditiontype = 'ZR02' )  INTO @DATA(disnew).
        SELECT SINGLE conditionamount,conditiontype FROM i_purordpricingelementtp_2 WITH PRIVILEGED ACCESS WHERE purchaseorder = @iv-b-purchaseorder
        AND purchaseorderitem = @iv-b-purchaseorderitem
        AND ( conditiontype = 'ZOT1' OR conditiontype = 'ZOT2' )  INTO @DATA(othnew).
        SELECT SINGLE conditionamount,conditiontype FROM i_purordpricingelementtp_2 WITH PRIVILEGED ACCESS WHERE purchaseorder = @iv-b-purchaseorder
        AND purchaseorderitem = @iv-b-purchaseorderitem
        AND  conditiontype = 'ZLOD'  INTO @DATA(lodnew).
        SELECT SINGLE conditionamount,conditiontype FROM i_purordpricingelementtp_2 WITH PRIVILEGED ACCESS WHERE purchaseorder = @iv-b-purchaseorder
        AND purchaseorderitem = @iv-b-purchaseorderitem
        AND  conditiontype = 'ZINS'  INTO @DATA(insnew).
        SELECT SINGLE conditionamount,conditiontype FROM i_purordpricingelementtp_2 WITH PRIVILEGED ACCESS WHERE purchaseorder = @iv-b-purchaseorder
        AND purchaseorderitem = @iv-b-purchaseorderitem
        AND  conditiontype = 'ZJEX'  INTO @DATA(taxnew).
        SELECT SINGLE productdescription FROM i_productdescription_2 WITH PRIVILEGED ACCESS WHERE product = @iv-b-material AND language = 'E' INTO @DATA(desc) .
        SELECT SINGLE productmanufacturernumber FROM i_product WITH PRIVILEGED ACCESS WHERE product = @wa-b-material INTO @DATA(manufac) .
        SELECT SINGLE  conditionamount ,
         conditiontype, conditionratevalue
         FROM i_purorditmpricingelementapi01 WITH PRIVILEGED ACCESS
         WHERE (   purchaseorder  = @iv-b-purchaseorder
           AND purchaseorderitem = @iv-b-purchaseorderitem )
         AND  ( conditiontype = 'ZR00' OR conditiontype = 'ZR01' OR conditiontype =
           'ZR02' )  INTO @DATA(discount1) .
        SELECT SINGLE NETAMOUNT FROM I_PurchaseOrderItemAPI01 WITH PRIVILEGED ACCESS
         WHERE purchaseorder     = @iv-b-purchaseorder
           AND purchaseorderitem = @iv-b-purchaseorderitem
           INTO @DATA(itemtax).
        DATA  weight TYPE string.
        DATA weight1 TYPE p DECIMALS 2.

        IF iv-b-material+0(5) = 'RMCOT' .
          weight  =  iv-b-orderquantity * 162 .
        ELSE .
          weight  =  iv-b-orderquantity .
        ENDIF.

        weight1 = weight .

        CASE iv-b-taxcode.
          WHEN 'V1'.
            cgst = itemtax / 100 * '2.5'.
            sgst = itemtax / 100 * '2.5'.
            cgst_rate = '2.5'.
            sgst_rate = '2.5'.
          WHEN 'V2'.
            cgst = itemtax / 100 * '6'.
            sgst = itemtax / 100 * '6'.
            cgst_rate = '6'.
            sgst_rate = '6'.
          WHEN 'V3'.
            cgst = itemtax / 100 * '9'.
            sgst = itemtax / 100 * '9'.
            cgst_rate = '9'.
            sgst_rate = '9'.
          WHEN 'V4'.
            cgst = itemtax / 100 * '14'.
            sgst = itemtax / 100 * '14'.
            cgst_rate = '14'.
            sgst_rate = '14'.
          WHEN 'V5'.
            igst = itemtax / 100 * '5'.
            igst_rate = 5.
          WHEN 'V6'.
            igst = itemtax / 100 * '12'.
            igst_rate = 12.
          WHEN 'V7'.
            igst = itemtax / 100 * '18'.
            igst_rate = 18.
          WHEN 'V8'.
            igst = itemtax / 100 * '28'.
            igst_rate = 28.
        ENDCASE.

        discount3 = discount1-conditionratevalue .
*      amount = iv-b-netpriceamount * iv-b-OrderQuantity.

        IF iv-b-baseunit = 'ST' .
          unit = 'PC' .
        ELSEIF  iv-b-baseunit = 'ZST' .
          unit = 'SET' .
        ELSEIF iv-b-baseunit = 'LE' .
          unit = 'AU' .
        ELSEIF iv-b-baseunit = 'ZNO' .
          unit = 'NO' .
        ELSE.
          ig = igst_rate.
          unit = iv-b-baseunit .
        ENDIF .

        DATA:net TYPE p DECIMALS 5.
        IF iv-b-orderpriceunit = 'KMN'.
*        iv-b-netpriceamount = iv-b-netpriceamount / '37.3245800'.
          netprice = iv-b-netpriceamount / '37.3245800'.
        ELSEIF iv-b-orderpriceunit = 'ZCN'.
*        iv-b-netpriceamount = iv-b-netpriceamount / '355.6189000'.
          netprice = iv-b-netpriceamount / '355.6189000'.
          ELSE.
           netprice = iv-b-netpriceamount .
        ENDIF.
*      netprice = iv-b-netpriceamount .

        lv_xml  =  lv_xml &&
          |<Row1>| &&
          |<Sno>{ n }</Sno>| &&
          |<Table12>| &&
          |<Row1>| &&
          |<material>{ iv-b-material }</material>| &&
          |</Row1>| &&
          |<Row2>| &&
          |<Materialdec>{ desc }</Materialdec>| &&
          |</Row2>| &&
          |<Row3>| &&
          |<Partno></Partno>| &&
          |</Row3>| &&
          |</Table12>| &&
          |<HSNCode>{ iv-b-br_ncm }</HSNCode>| &&
          |<Quantity>{ weight1  }</Quantity>| &&
          |<UOM>{ unit }</UOM>| &&
          |<rate>{ netprice }</rate>| &&
          |<dispersantage>{ discount3 }</dispersantage>| &&
          |<disamt>{ discount1-conditionamount }</disamt>| &&
          |<amount>{ iv-b-grossamount }</amount>| &&
          |<cgstpersantage>{ cgst_rate }</cgstpersantage>| &&
          |<cgstamount>{ cgst }</cgstamount>| &&
          |<sgstpersantage>{ sgst_rate }</sgstpersantage>| &&
          |<sgstamount>{ sgst }</sgstamount>| &&
          |<igstpersantage>{ ig }</igstpersantage>| &&
          |<igstamount>{ igst }</igstamount>| &&
          |</Row1>| .





        basicnew1 = basicnew1 + basicnew-conditionamount.
        taxnew1   = taxnew1  + taxnew-conditionamount .
        disnew1  = disnew1 + disnew-conditionamount .
        othnew1 = othnew1 + othnew-conditionamount .
        lodnew1 = lodnew1 + lodnew-conditionamount .
        insnew1 = insnew1 + insnew-conditionamount .
      ENDLOOP.

      DATA totalvalue TYPE string .
      DATA totv TYPE string .
      DATA totnew TYPE string.

      totalvalue = basicnew1 + disnew1    .
      totv = lodnew1 + taxnew1  + othnew1 + insnew1 .
      totnew = totalvalue + totv.

      lv_xml =  lv_xml &&
*    |<cgstamount></cgstamount> | &&
*    |<sgstamount></sgstamount>| &&
*    |<igstamount></igstamount>| &&
*    |<wordamount></wordamount>| &&
      |<Discount>{ basicnew1 }</Discount>| &&
      |<TotalBasicValue>{ disnew1 }</TotalBasicValue>| &&
      |<PackingForwarding>{ pf-conditionamount }</PackingForwarding>| &&
      |<OtherChange>{ othnew1 }</OtherChange>| &&
      |<Freight>{ freight-conditionamount }</Freight>| &&
      |<Loding>{ lodnew1 }</Loding>| &&
      |<Insurance>{ insnew1 }</Insurance>| &&
      |<TotalTaxValue>{ taxnew1 }</TotalTaxValue>| &&
      |<TotalValueIncTax>{ totnew }</TotalValueIncTax>| &&
      |<TermsCondition></TermsCondition>| &&
      |<PaymentTerms>{ pterms }</PaymentTerms>| &&
      |<IncotermsVersion>{ wa-a-incotermsclassification } , { wa-a-incotermslocation1 }</IncotermsVersion>| &&
*      |<BargainDate>{ wa-a-yy1_comparativedate_pdh }</BargainDate>| &&
*      |<Rate>{ wa-a-yy1_transit_insurance_pdh }</Rate>| &&
*      |<epcglicence>{ wa-a-yy1_epcglicence_pdh }</epcglicence>| &&
      |<FREIGHT>{ SWITCH #(  freight-conditionamount WHEN 0 THEN 0 ELSE 1 ) }</FREIGHT>| &&
      |<BROKRAGE>{ SWITCH #(  brokrage-conditionamount WHEN 0 THEN 0 ELSE 1 ) }</BROKRAGE>| &&
      |<FREIGHT></FREIGHT>| &&
      |<BROKRAGE></BROKRAGE>| &&
      |<specialinstructions>{ spic-plainlongtext }</specialinstructions>| &&
*      |<GINNERNAME>{ wa-a-yy1_stationno_pdh }</GINNERNAME>| &&
      |<prepaidby>{ prepaid }</prepaidby>| &&
      |<WATERMARK>{ wa-a-ReleaseIsNotCompleted }</WATERMARK>| &&
      |</bdyMain>| &&
      |</Form>| .

*|<epcglicence>{ wa-a-yy1_epcglicence_pdh }</epcglicence>| &&

      CALL METHOD ycl_test_adobe=>getpdf(
        EXPORTING
          xmldata  = lv_xml
          template = lc_template_name1
        RECEIVING
          result   = result12 ).

    ENDIF .
  ENDMETHOD.
ENDCLASS.
