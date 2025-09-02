class ZCL_MM_PO_PRINT_HTTP definition
  public
  create public .

public section.

  interfaces IF_HTTP_SERVICE_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_MM_PO_PRINT_HTTP IMPLEMENTATION.


  method IF_HTTP_SERVICE_EXTENSION~HANDLE_REQUEST.
data lv type string .




  DATA(req) = request->get_form_fields(  ).
    response->set_header_field( i_name = 'Access-Control-Allow-Origin' i_value = '*' ).
    response->set_header_field( i_name = 'Access-Control-Allow-Credentials' i_value = 'true' ).
    response->set_header_field( i_name = 'X-Frame-Options' i_value = 'ALLOW-FROM https://my413211.s4hana.cloud.sap' ).
*    response->set_header_field( i_name = 'X-Xss-Protection' i_value = '0' ).

    DATA Purchaseorder TYPE string .
    DATA store TYPE string.
*    DATA value1 TYPE string.


    purchaseorder = value #( req[ name = 'purchaseorder' ]-value optional ) .
    data(wf) = value #( req[ name = 'workflow' ]-value optional ) .

  DATA : multi_pdf TYPE string ,
         ROUND     TYPE string ,
         pdf_xstring TYPE xstring.

    DATA(l_merger) = cl_rspo_pdf_merger=>create_instance( ).

 DATA : purcder TYPE RANGE OF I_PurchaseOrderAPI01-PurchaseOrder.
    SPLIT purchaseorder AT  ',' INTO TABLE DATA(itab).

 LOOP AT itab into data(watab1)  .
        TRY.
          DATA(pdf1) = zcl_mm_purchase_order_store=>read_posts( purchaseorder =  CONV #( watab1 ) manual  = 'X' )  .
          CATCH cx_static_check.
            "handle exception
        ENDTRY.
        pdf_xstring = xco_cp=>string( pdf1 )->as_xstring( xco_cp_binary=>text_encoding->base64 )->value.
        l_merger->add_document( pdf_xstring ).

 ENDLOOP .

try.

     DATA(l_poczone_pdf) = l_merger->merge_documents( ).
      CATCH cx_rspo_pdf_merger INTO DATA(l_exception).
endtry.

DATA(str) =  xco_cp=>xstring( l_poczone_pdf
          )->as_string( xco_cp_binary=>text_encoding->base64
          )->value   .

lv = |<!DOCTYPE html><html><head><title>Base64 PDF</title></head><body><iframe src = "data:application/pdf;base64,{ str }"| &&
     |type="application/pdf" width="100%" height="600" sandbox='allow-scripts allow-same-origin' ></iframe></body></html>|  .
 if wf is not INITIAL  .
  response->set_text(  lv ) .

 else  .

    response->set_text(  str ) .


 ENDIF.


  endmethod.
ENDCLASS.
