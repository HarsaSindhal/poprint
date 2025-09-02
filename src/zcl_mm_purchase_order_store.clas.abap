
 CLASS zcl_mm_purchase_order_store DEFINITION
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
                  VALUE(MANUAL)         TYPE STRING OPTIONAL

        RETURNING VALUE(result12)      TYPE string
        RAISING   cx_static_check .
  PROTECTED SECTION.
  PRIVATE SECTION.

       CONSTANTS lc_ads_render TYPE string VALUE '/ads.restapi/v1/adsRender/pdf'.
*      CONSTANTS  lv1_url    TYPE string VALUE 'https://adsrestapi-formsprocessing.cfapps.eu10.hana.ondemand.com/v1/adsRender/pdf?templateSource=storageName&TraceLevel=2'  .
*      CONSTANTS  lv2_url    TYPE string VALUE 'https://btp-b3lap1je.authentication.eu10.hana.ondemand.com'  .
       CONSTANTS lc_storage_name TYPE string VALUE 'templateSource=storageName'.
  "     CONSTANTS lc_template_name TYPE string VALUE 'ZPURCHASE_ORDER_STORE_PRINT/ZPURCHASE_ORDER_STORE_PRINT'.

ENDCLASS.



CLASS ZCL_MM_PURCHASE_ORDER_STORE IMPLEMENTATION.


       METHOD create_client .
       DATA(dest) = cl_http_destination_provider=>create_by_url( url ).
       result = cl_web_http_client_manager=>create_by_http_destination( dest ).
  ENDMETHOD .


  METHOD if_oo_adt_classrun~main.
  ENDMETHOD.


  METHOD read_posts .

    DATA lv_xml TYPE string.
    DATA xsml TYPE string.
    DATA totbasicvalue TYPE string .
    DATA totdiscount TYPE string.
    DATA sumqty TYPE string.
    DATA sumrate TYPE string.
    DATA incluidtax TYPE string.
    DATA packing TYPE string.
    DATA UNIT TYPE STRING .
    DATA SUMDIS TYPE STRING.
    DATA SUMBASIC TYPE STRING.
    DATA SBA TYPE STRING.




       SELECT * FROM i_purchaseorderapi01 WITH PRIVILEGED ACCESS AS a
             LEFT JOIN i_purchaseorderitemapi01 WITH PRIVILEGED ACCESS AS b ON ( b~purchaseorder = a~purchaseorder AND B~PurchasingDocumentDeletionCode = '' )
             LEFT JOIN i_supplier WITH PRIVILEGED ACCESS AS c ON ( c~supplier = a~supplier )
             WHERE a~purchaseorder = @purchaseorder   INTO TABLE  @DATA(it) .

if sy-subrc = 0 and (  cl_abap_context_info=>get_user_description(  )  =  'SAP Workflow Runtime'   or  manual = 'X' ) .



 SORT it  ASCENDING BY b-purchaseorder  b-purchaseorderitem  .
*   DELETE ADJACENT DUPLICATES FROM it COMPARING a-purchaseorder  .
    READ TABLE it INTO DATA(wa) INDEX 1.

       SELECT SINGLE * FROM I_Supplier WHERE Supplier = @WA-B-Subcontractor INTO @DATA(SUP_ADD).

       SELECT SINGLE PaymentTermsDescription FROM i_paymenttermstext WITH PRIVILEGED ACCESS  WHERE paymentterms = @wa-a-paymentterms and Language = 'E' INTO @DATA(pterms).
       SELECT SINGLE personfullname FROM i_businessuserbasic WITH PRIVILEGED ACCESS WHERE userid = @wa-a-createdbyuser INTO @DATA(prepaid) .
       SELECT SINGLE * FROM zsupplier_detailS WITH PRIVILEGED ACCESS WHERE Supplier = @wa-c-Supplier INTO @data(email).
*       SELECT SINGLE msmenumber FROM zvendor_msme WITH PRIVILEGED ACCESS WHERE supplier = @wa-c-Supplier  INTO @data(MSME).
       SELECT SINGLE * from  ZMM_SUPPLIER_ADDR WITH PRIVILEGED ACCESS where Supplier = @wa-a-Supplier into @data(add1).
       SELECT SINGLE IncotermsClassificationName  FROM I_IncotermsClassificationText WITH PRIVILEGED ACCESS WHERE IncotermsClassification = @wa-a-IncotermsClassification
       AND Language = 'E' INTO @data(inc).
       SELECT SINGLE PurchasingGroupName FROM I_PurchasingGroup WITH PRIVILEGED ACCESS WHERE PurchasingGroup = @wa-a-PurchasingGroup INTO @data(pp).
       SELECT SINGLE StorageLocationName FROM I_StorageLocation WITH PRIVILEGED ACCESS WHERE StorageLocation = @wa-b-StorageLocation and Plant = @wa-b-Plant INTO @data(storage).
       SELECT SINGLE MasterFixedAsset FROM I_purordaccountassignmentapi01 WITH PRIVILEGED ACCESS WHERE PurchaseOrder = @wa-b-PurchaseOrder INTO @data(affet).

*        SELECT count( * )  FROM I_PurchaseOrderChangeDocument WHERE PurchaseOrder = @wa-a-PurchaseOrder  into @data(amd).
*      IF amd > 1.
*       SELECT  max( CreationDate ) FROM I_PurchaseOrderChangeDocument WHERE PurchaseOrder = @wa-a-PurchaseOrder  into @data(amd1).
*      ENDIF.

data ship1 TYPE string.
data ship2 TYPE string.
data ship3 TYPE string.

ship1 =  SUP_ADD-SupplierName  .
ship2 = SUP_ADD-BPAddrStreetName  .
ship3 = | { SUP_ADD-PostalCode } ' GstNO.'  { SUP_ADD-BPAddrStreetName } |.


if WA-B-Subcontractor is INITIAL or WA-B-Subcontractor = ''.

if
wa-b-Plant  = '1101' or wa-b-Plant  = '1103'.
ship1 = 'Sonaselection India Limited'.
ship2 = '18th KM Stone, Chittorgarh Road, Harmirgarh,'.
ship3 = 'Bhilwara, Rajasthan (311025)' .
ELSEIF
wa-b-Plant  = '3201'.
ship1 = 'SONA STYLES LIMITED'.
ship2 = 'Plot No 1/270/1 and 270/2, Chittor Road, Guwardi,'.
ship3 = 'Bhilwara, Rajasthan (311025)'.
ELSEIF
wa-b-Plant  = '5101'.
ship1 = 'Sona Processors (India) Limited'.
ship2 = '12th KM Stone, Village - Guwardi, Chittorgarh Road,'.
ship3 = 'Bhilwara, Rajasthan (311025)'.
ELSEIF
wa-b-Plant  = '7201'.
ship1 = 'Sona Texfab Private Limited'.
ship2 = '2, Sangam Tower, Old RTO, Gandhi nagar,'.
ship3 = 'Bhilwara, Rajasthan (311025)'.
ENDIF.

ENDIF.

    DATA lc_template_name1 TYPE string .

  lc_template_name1 =  'PURCHASE_ORDER'.

          lv_xml =
            |<form1>| &&
            |<Subform1>| &&
            |<POHEDSUBFORM>| &&
            |<CINNO></CINNO>| &&
            |<GSTIN></GSTIN>| &&
            |<PANNO></PANNO>| &&
*            |<POORDER></POORDER>| &&
*            |<PODATE></PODATE>| &&
            |</POHEDSUBFORM>| &&
            |<headersubform>| &&
            |<vendorsubform>| &&
            |<com_code>{ wa-a-CompanyCode }</com_code>| &&
            |<vendorcode>{ wa-a-Supplier }</vendorcode>| &&
            |<vendorname>{ wa-c-SupplierName }</vendorname>| &&
            |<addressline1>{ add1-CareOfName }</addressline1>| &&
            |<addressline2>{ add1-streete }, { add1-StreetPrefixName1 }</addressline2>| &&
            |<addressline3>{ add1-StreetPrefixName2 },{ add1-StreetSuffixName1 }</addressline3>| &&
            |<pincode>{ wa-c-PostalCode }</pincode>| &&
            |<city>{ wa-c-CityName }</city>| &&
            |<gstin>{ wa-c-TaxNumber3 }</gstin>| &&
            |<mobile>{ wa-c-phonenumber1 }</mobile>| &&
            |<email>{ email-EmailAddress }</email>| &&
*            |<mameno>{ MSME }</mameno>| &&
*            |<supplierrefno>{ wa-a-YY1_Partyinvoice_PDH }{ wa-a-CorrespncExternalReference }</supplierrefno>| &&
*            |<supplierrefdate>{ wa-a-YY1_OurInvoice_PDH }{ wa-a-CorrespncInternalReference }</supplierrefdate>| &&
            |<DataType>{ wa-a-PurchaseOrderType }</DataType>| &&
            |</vendorsubform>| &&
            |<shiptosubform>| &&
            |<shiptoadd></shiptoadd>| &&
            |<shipto>{ ship1 }</shipto>| &&
            |<sudivaspinners>{ ship1 }</sudivaspinners>| &&
            |<addressline1>{ ship2 }</addressline1>| &&
            |<addressline2>{ ship3 }</addressline2>| &&
*            |<mendmentreason>{ wa-a-YY1_AmendMentReason_PDH }</mendmentreason>| &&
*            |<amendmentdate>{ wa-a-YY1_AmendmentDate_PDH }</amendmentdate>| &&
*            |<comparativeno>{ wa-a-YY1_Comparativeno_PDH }</comparativeno>| &&
*            |<comparativedate>{ wa-a-YY1_ComparativeDate_PDH }</comparativedate>| &&
            |<department>{ pp }</department>| &&
            |<warehouse>{ storage }</warehouse>| &&
            |<exchangerate>{ wa-a-ExchangeRate }</exchangerate>| &&
            |<currancy>{ wa-a-DocumentCurrency }</currancy>| &&
            |<Affet>{ affet }</Affet>| &&
            |<incoterms>{ wa-a-IncotermsClassification }</incoterms>| &&
            |<incoterms_loc>{ wa-a-IncotermsTransferLocation }</incoterms_loc>| &&
            |</shiptosubform>| &&
            |</headersubform>| .
*            |<TABLESUBFORM>| .

       DATA:igst TYPE p DECIMALS 2,
         cgst TYPE p DECIMALS 2,
         sgst TYPE p DECIMALS 2,
         amount TYPE p DECIMALS 2.
    DATA:igst_rate TYPE p DECIMALS 1,
         cgst_rate TYPE p DECIMALS 1,
         sgst_rate TYPE p DECIMALS 1.
         DATA discount3 TYPE p DECIMALS 0.
         DATA SUMPF TYPE STRING.
         DATA SUMFREIGHT TYPE STRING.
         DATA totgst TYPE string.
         DATA TOTALAMOUNT TYPE P DECIMALS 2.
         DATA TOTALCGST TYPE P DECIMALS 2.
         DATA TOTALSGST TYPE P DECIMALS 2.
         DATA TOTALIGST TYPE P DECIMALS 2.
         DATA SUMOTHER11 TYPE P DECIMALS 2.
         DATA RATE1 TYPE p DECIMALS 2.
         DATA RATEAMT TYPE STRING.

SORT IT BY b-purchaseorderitem.

    LOOP AT it INTO DATA(iv) .


           SELECT SINGLE ConditionType , conditionamount FROM i_purorditmpricingelementapi01 WITH PRIVILEGED ACCESS
           WHERE  purchaseorder  = @IV-b-purchaseorder AND PurchaseOrderItem = @iv-b-PurchaseOrderItem AND conditionamount <> '' AND
           ( ConditionType = 'ZR00' OR ConditionType = 'ZR01'  OR ConditionType = 'ZR02' )   INTO @DATA(DISCOUNT1) .

          SELECT SINGLE Conditionratevalue   FROM i_purorditmpricingelementapi01 WITH PRIVILEGED ACCESS WHERE  purchaseorder  = @IV-b-purchaseorder   AND PurchaseOrderItem = @IV-b-PurchaseOrderItem AND Conditionratevalue <> '' AND ConditionType = 'ZR00'
INTO @DATA(DISCOUNT2) .


          SELECT SINGLE  ConditionRateValue  FROM i_purorditmpricingelementapi01 WITH PRIVILEGED ACCESS WHERE  purchaseorder  = @IV-b-purchaseorder   AND PurchaseOrderItem = @IV-b-PurchaseOrderItem AND Conditionratevalue <> '' AND ConditionType = 'ZR01'
INTO @DATA(DISCOUNT2_I) .
           discount3 = DISCOUNT2_I .

           SELECT SINGLE schedulelinedeliverydate FROM I_PurchaseOrdScheduleLineTP_2 WITH PRIVILEGED ACCESS WHERE PurchaseOrder = @IV-b-purchaseorder AND PurchaseOrderItem = @iv-b-PurchaseOrderItem INTO @data(deldat).
            SELECT SINGLE ConditionType , conditionamount FROM I_PurOrdPricingElementTP_2 WITH PRIVILEGED ACCESS WHERE  purchaseorder  = @iv-b-purchaseorder AND PurchaseOrderItem = @iv-b-PurchaseOrderItem
          AND ( ConditionType = 'ZPBX' OR  ConditionType = 'ZPB0' ) and ConditionAmount <> '' INTO @DATA(BAMOUNT) .
            SELECT SINGLE ProductManufacturerNumber FROM I_Product WITH PRIVILEGED ACCESS WHERE Product = @iv-b-Material  into @data(manufac) .
            SELECT SINGLE conditiontype , conditionamount FROM I_PurOrdPricingElementTP_2 WITH PRIVILEGED ACCESS WHERE purchaseorder = @iv-b-purchaseorder AND PurchaseOrderItem = @iv-b-PurchaseOrderItem
               AND ConditionType = 'ZJEX' INTO  @DATA(totaltaxpacking).
            SELECT SINGLE ConditionType , conditionamount FROM I_PurOrdPricingElementTP_2 WITH PRIVILEGED ACCESS WHERE  purchaseorder  = @iv-b-purchaseorder AND PurchaseOrderItem = @iv-b-PurchaseOrderItem
            AND conditionamount <> ''  AND ( ConditionType = 'ZOT1' OR ConditionType = 'ZOT2' OR ConditionType = 'ZC27')  INTO @DATA(OTHER11) .
            SELECT SINGLE conditiontype , conditionamount FROM i_purorditmpricingelementapi01 WITH PRIVILEGED ACCESS WHERE PurchaseOrder = @iv-a-PurchaseOrder AND PurchaseOrderItem = @iv-b-PurchaseOrderItem
      AND conditionamount <> '' AND ( ConditionType = 'ZPF1'
      OR ConditionType = 'ZPF0' OR ConditionType = 'ZPF2' ) INTO @DATA(pf).
      SELECT SINGLE ConditionType , conditionamount FROM I_PurOrdPricingElementTP_2 WITH PRIVILEGED ACCESS WHERE  purchaseorder  = @iv-b-purchaseorder AND PurchaseOrderItem = @iv-b-PurchaseOrderItem AND conditionamount <> ''
       AND ( ConditionType = 'ZFB1' OR ConditionType = 'ZFA1' OR ConditionType = 'ZFA3' OR ConditionType = 'ZFB4' OR ConditionType = 'ZFB2'
      OR ConditionType = 'ZFC1' ) INTO @DATA(freight) .
      SELECT SINGLE * FROM I_PurchaseRequisitionItemAPI01 WITH PRIVILEGED ACCESS WHERE  PurchaseRequisition = @iv-b-PurchaseRequisition AND PurchaseRequisitionItem = @iv-b-PurchaseRequisitionItem
       into @data(aa).
       SELECT SINGLE PurchasingGroupName FROM I_PurchasingGroup WITH PRIVILEGED ACCESS WHERE PurchasingGroup = @aa-PurchasingGroup into @data(pr).
       SELECT SINGLE conditionrateamount FROM I_PurOrdPricingElementTP_2 WITH PRIVILEGED ACCESS WHERE PurchaseOrder = @iv-b-PurchaseOrder
       AND PurchaseOrderItem = @iv-b-PurchaseOrderItem  AND ( ConditionType = 'ZPBX' or ConditionType = 'ZPB0' ) into @data(RATE).
        RATE1 = RATE.
            SELECT SINGLE NetAmount FROM i_purchaseorderitemapi01 WITH PRIVILEGED ACCESS
       WHERE purchaseorder     = @iv-b-purchaseorder
         AND purchaseorderitem = @iv-b-purchaseorderitem
         INTO @DATA(itemtax).


DATA : ITEMTEXT(5000) TYPE C .

*         if iv-b-YY1_Remark2_PDI  is INITIAL .
      SELECT SINGLE PlainLongText FROM I_PurchaseOrderItemNoteTP_2 WITH PRIVILEGED ACCESS WHERE PurchaseOrderItem = @iv-b-PurchaseOrderItem and PurchaseOrder = @iv-b-PurchaseOrder AND TextObjectType = 'F01'
                                      AND Language = 'E' INTO @DATA(itemtettext).
        ITEMTEXT = itemtettext .
*        ELSE .
*        ITEMTEXT = iv-b-YY1_Remark2_PDI  .


*       endif .

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

     amount =  iv-b-GrossAmount.

     if iv-b-baseunit is INITIAL .
     iv-b-baseunit = iv-b-PurchaseOrderQuantityUnit .
     ENDIF.

       IF iv-b-baseunit = 'ST' .

        unit = 'PC' .

      ELSEIF  iv-b-baseunit = 'ZST' .

        unit = 'SET' .

      ELSEIF iv-b-baseunit = 'LE' .
        unit = 'AU' .

      ELSEIF iv-b-baseunit = 'ZNO' .
        unit = 'NO' .

      ELSE.

       unit = iv-b-baseunit .
        ENDIF .
*     RATEAMT =    rate1 + DISCOUNT1-ConditionAmount .
     RATEAMT =    rate1  .

          lv_xml  =  lv_xml &&
*           |<Table1>| &&
            |<tabsub>| &&
            |<Row1>| &&
*            |<Row1>| &&
            |<sno></sno>| &&
            |<department>{ pr }</department>| &&
            |<PRNumber>{ iv-b-PurchaseRequisition }</PRNumber>| &&
            |<itemdescription>{ iv-b-Material }{ cl_abap_char_utilities=>cr_lf }{ iv-b-PurchaseOrderItemText }{ manufac }</itemdescription>| &&
            |<HSNcode>{ iv-b-br_ncm }</HSNcode>| &&
            |<UOM>{ unit }</UOM>| &&
            |<Qty>{ iv-b-OrderQuantity }</Qty>| &&
            |<Rate>{ RATEAMT }</Rate>| &&
            |<disper>{ discount3 }</disper>| &&
            |<disamount>{ DISCOUNT1-ConditionAmount }</disamount>| &&
            |<Amount>{ amount }</Amount>| &&
            |<cgstper>{ cgst_rate }</cgstper>| &&
            |<cgstamount>{ cgst }</cgstamount>| &&
            |<sgstper>{ sgst_rate }</sgstper>| &&
            |<sgstamount>{ sgst }</sgstamount>| &&
            |<igstper>{ igst_rate }</igstper>| &&
            |<igstamount>{ igst }</igstamount>| &&
*            |<igstAmount>{ igst }</igstAmount>| &&
            |<DeliveryDate>{ deldat }</DeliveryDate>| &&
            |</Row1>| &&
            |<lineitem>{ ITEMTEXT }</lineitem>| &&
           |</tabsub>| .
* |<Table1>| .


      SUMBASIC = SUMBASIC + amount .
      SUMDIS = SUMDIS + DISCOUNT1-ConditionAmount .
      SUMPF = SUMPF + pf-ConditionAmount.
     SUMFREIGHT = SUMFREIGHT + freight-ConditionAmount.
     "totgst = totgst + totaltaxpacking-ConditionAmount.
      SBA = SBA + BAMOUNT-ConditionAmount .
     TOTALAMOUNT = TOTALAMOUNT + amount.
     TOTALCGST = TOTALCGST + cgst .
     TOTALSGST = TOTALSGST + sgst .
     totaligst = totaligst + igst.
     SUMOTHER11 = SUMOTHER11 + OTHER11-ConditionAmount.
     totgst =  TOTALCGST + TOTALSGST +  totaligst.
  CLEAR : discount1 , manufac , DISCOUNT2,ITEMTEXT,itemtettext,cgst ,sgst,igst,cgst_rate,sgst_rate,igst_rate.
 ENDLOOP.


    DATA TOTALVALUE TYPE STRING .
    DATA TOTV TYPE STRING .
    DATA SUMBAMOUNT TYPE STRING.


    TOTALVALUE = SBA + SUMPF + sumfreight + totgst .
    TOTV = TOTALVALUE + SUMDIS + SUMOTHER11 .
*   SELECT SINGLE PlainLongText FROM I_PurchaseOrderNoteTP_2 WITH PRIVILEGED ACCESS WHERE PurchaseOrder = @wa-a-PurchaseOrder AND TextObjectType = 'F06'
*                                      AND Language = 'E' INTO @DATA(SHIPMOD).
*   SELECT SINGLE PlainLongText FROM I_PurchaseOrderNoteTP_2 WITH PRIVILEGED ACCESS WHERE PurchaseOrder = @wa-a-PurchaseOrder AND TextObjectType = 'F07'
*                                      AND Language = 'E' INTO @DATA(PAYMOD).
   SELECT SINGLE PlainLongText FROM I_PurchaseOrderNoteTP_2 WITH PRIVILEGED ACCESS WHERE PurchaseOrder = @wa-a-PurchaseOrder AND TextObjectType = 'F01'
                                      AND Language = 'E' INTO @DATA(PAYTRM).

     SUMBAMOUNT = SUMBAMOUNT + BAMOUNT-ConditionAmount.
     data basic TYPE string.
     data pf1 type string.
   data fright type string.
   data MANDCHARGEAMT type string.
   data SelvMonoChargeAMT type string.
   data LoadingChargeAMT type string.
   data ZCESS type string.
   data other type string.
   data tot type string.
    data dis type string.
    data dis1 type string.
  dis = SUMDIS * wa-a-ExchangeRate.
    dis1 = dis * -1.

      SELECT SINGLE  SUM( conditionamount ) FROM I_PurOrdPricingElementTP_2 WITH PRIVILEGED ACCESS WHERE  purchaseorder  = @purchaseorder AND conditionamount <> ''
       AND ( ConditionType = 'ZMCH'  ) INTO @DATA(MANDINGCHARG) .

   SELECT SINGLE SUM(  conditionamount ) FROM I_PurOrdPricingElementTP_2 WITH PRIVILEGED ACCESS WHERE  purchaseorder  = @purchaseorder AND conditionamount <> ''
       AND ( ConditionType = 'ZSCH'  ) INTO @DATA(SelvMonoCharge) .

        SELECT SINGLE  SUM( conditionamount ) FROM I_PurOrdPricingElementTP_2 WITH PRIVILEGED ACCESS WHERE  purchaseorder  = @purchaseorder AND conditionamount <> ''
       AND ( ConditionType = 'ZBCH'  ) INTO @DATA(LoadingCharge) .

       SELECT SINGLE  SUM( conditionamount ) FROM I_PurOrdPricingElementTP_2 WITH PRIVILEGED ACCESS WHERE  purchaseorder  = @purchaseorder AND conditionamount <> ''
       AND ( ConditionType = 'ZCES'  ) INTO @DATA(cess) .

             SELECT SINGLE  SUM( conditionamount ) FROM I_PurOrdPricingElementTP_2 WITH PRIVILEGED ACCESS WHERE  purchaseorder  = @purchaseorder AND conditionamount <> ''
       AND ( ConditionType = 'ZIN3'  ) INTO @DATA(other_charge) .

   IF  iv-a-PurchasingGroup = 'P21'.
    basic = SBA * 1.
      pf1 = SUMPF * 1.
      fright = sumfreight * 1.
      other = SUMOTHER11 * 1 + other_charge.
      tot = TOTV * 1 + other_charge.
      MANDCHARGEAMT     = MANDINGCHARG .
      SelvMonoChargeAMT = SelvMonoCharge.
      loadingchargeamt  = loadingcharge.
      zcess  = cess.

   ELSE.
      basic   = SBA * wa-a-ExchangeRate.
      pf1     = SUMPF * wa-a-ExchangeRate.
      fright  = sumfreight * wa-a-ExchangeRate.
      other   = SUMOTHER11 * wa-a-ExchangeRate + other_charge.
      tot     = TOTV * wa-a-ExchangeRate + other_charge .
      MANDCHARGEAMT     = MANDINGCHARG * wa-a-ExchangeRate.
      SelvMonoChargeAMT = SelvMonoCharge * wa-a-ExchangeRate.
      loadingchargeamt  = loadingcharge * wa-a-ExchangeRate.
      zcess  = cess.

    ENDIF.

    IF tot < 0 .
    tot = tot * -1.
    ENDIF.

      lv_xml =  lv_xml &&
         "|<totalamount>{ TOTALAMOUNT }</totalamount>| &&
         "|<cgstamount>{ TloadingchargeamtOTALCGST }</cgstamount>| &&
         "|<totsgst>{ TOTALSGST }</totsgst>| &&
         "|<totaligst>{ totaligst }</totaligst>| &&
         |<SUBFORMAMTWORDS>| &&
         |<amountinword></amountinword>| &&
         |<basicvalue>{ basic }</basicvalue>| &&
         |<discount>{ SUMDIS }</discount>| &&
         |<PF>{ pf1 }</PF>| &&
         |<OTHERCHARGE>{ fright }</OTHERCHARGE>| &&
         |<MendingCharge>{ MANDCHARGEAMT }</MendingCharge>| &&
         |<SelvMonoCharge>{ SelvMonoChargeAMT }</SelvMonoCharge>| &&
         |<LoadingCharge>{ loadingchargeamt }</LoadingCharge>| &&
         |<Cess>{ zcess }</Cess>| &&
         |<othercharge11>{ other }</othercharge11>| &&
         |<GSTTOTAL>{ totgst }</GSTTOTAL>| &&
         |<TOTALVALUE>{ tot }</TOTALVALUE>| &&
         |</SUBFORMAMTWORDS>| &&

         |<Import>| &&
         |<USDWORD></USDWORD>| &&
         |<INRWORD></INRWORD>| &&
         |<Table6>| &&
         |<Row1>| &&
         |<Curr>{ wa-a-DocumentCurrency }</Curr>| &&
          |</Row1>| &&
          |<Row2>| &&
          |<USDBASIC>{ SBA }</USDBASIC>| &&
          |<INRBASIC>{ basic }</INRBASIC>| &&
    |</Row2>| &&
    |<Row3>| &&
    |<USDDISC>{ SUMDIS }</USDDISC>| &&
    |<USDDISC>{ dis1 }</USDDISC>| &&
    |</Row3>| &&
    |<Row4>| &&
    |<USDPF>{ SUMPF }</USDPF>| &&
    |<INRPF>{ pf1 }</INRPF>| &&
    |</Row4>| &&
    |<Row5>| &&
    |<USDFRIGHT>{ sumfreight }</USDFRIGHT>| &&
    |<INRFRIGHT>{ fright }</INRFRIGHT>| &&
    |</Row5>| &&
    |<Row9>| &&
    |<USDMANDING>{ MANDCHARGEAMT }</USDMANDING>| &&
    |<INRMANDING>{ MANDCHARGEAMT }</INRMANDING>| &&
    |</Row9>| &&
    |<Row10>| &&
    |<USDSELVMONO>{ SelvMonoChargeAMT }</USDSELVMONO>| &&
    |<INRSELVMONO>{ SelvMonoChargeAMT }</INRSELVMONO>| &&
    |</Row10>| &&
    |<Row11>| &&
    |<USDLOADING>{ loadingchargeamt }</USDLOADING>| &&
    |<INRLOADING>{ loadingchargeamt }</INRLOADING>| &&
    |</Row11>| &&
    |<Row6>| &&
    |<USDOTHER>{ SUMOTHER11 }</USDOTHER>| &&
    |<INROTHER>{ other }</INROTHER>| &&
    |</Row6>| &&
    |<Row7>| &&
    |<USDGST>{ totgst }</USDGST>| &&
    |<INRGST>{ totgst * wa-a-ExchangeRate }</INRGST>| &&
    |</Row7>| &&
    |<Row8>| &&
    |<USDTOTAL>{ TOTV }</USDTOTAL>| &&
    |<INRTOTAL>{ tot }</INRTOTAL>| &&
    |</Row8>| &&
    |</Table6>| &&
    |</Import>| &&


         |<subformtermcondition>| &&
         |<TermsCondition></TermsCondition>| &&
         |<PriceBasic>{ wa-a-IncotermsClassification } ,{ inc } , { wa-a-IncotermsLocation1 }</PriceBasic>| &&
*         |<epcglicense>{ wa-a-yy1_epcglicence_pdh }</epcglicense>| &&
         |<paymentterm>{ pterms }</paymentterm>| &&
         |<paymentmode></paymentmode>| &&
         |<shippingmode></shippingmode>| &&
*         |<namoftransport>{ wa-a-YY1_MM_Transporter_Nam_PDH }</namoftransport>| &&
         |<tolerance>{ wa-b-OverdelivTolrtdLmtRatioInPct }</tolerance>| &&
*         |<transitinsurance>{ wa-a-YY1_Transit_insurance_PDH }</transitinsurance>| &&
         |</subformtermcondition>| &&
         |<specialsubform>| &&
         |<specialinsuranceremark>{ PAYTRM }</specialinsuranceremark>| &&
*         |<prepaidby>{ prepaid }</prepaidby>| &&
         |</specialsubform>| &&
         |<prepaidby>{ prepaid }</prepaidby>| &&
         |<checkedby></checkedby>| &&
         |<passedby></passedby>| &&
         |<authorisedby></authorisedby>| &&
         |</Subform1>| &&
         |<POORDER>{ wa-a-PurchaseOrder }</POORDER>| &&
         |<PODATE>{ wa-a-PurchaseOrderDate }</PODATE>| &&
         |<WATERMARK>{ wa-a-ReleaseIsNotCompleted }</WATERMARK>| &&
         |<purchasinggroup>{ wa-a-PurchasingGroup }</purchasinggroup>| &&
*         |<prepaidby>{ prepaid }</prepaidby>| &&
         |</form1>| .

*|<WATERMARK>{ wa-a-ReleaseIsNotCompleted }</WATERMARK>| &&

     CALL METHOD ycl_test_adobe=>getpdf(
      EXPORTING
        xmldata  = lv_xml
        template = lc_template_name1
      RECEIVING
        result   = result12 ).
else .

endif .

  ENDMETHOD.
ENDCLASS.
