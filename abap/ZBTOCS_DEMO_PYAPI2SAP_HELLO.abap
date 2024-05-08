*&---------------------------------------------------------------------*
*& PyAPI4SAP Demo - endpoint method /hello
*& Returns a simple "Hello SAP" without parameters
*&---------------------------------------------------------------------*
*& (c) 20324 mdjoerg
*& version: 08.05.2024 
*& project:  https://github.com/b-tocs/pyapi4abap
*& requires: https://github.com/b-tocs/abap_btocs_core
*&---------------------------------------------------------------------*
REPORT zbtocs_demo_pyapi2sap_hello.

* --------- interface
PARAMETERS: p_rfc TYPE rfcdest OBLIGATORY DEFAULT 'EXT_BTOCS_PYAPI4ABAP'.
SELECTION-SCREEN: ULINE.
PARAMETERS: p_trace AS CHECKBOX TYPE zbtocs_flag_display_trace DEFAULT abap_false.


* --------- call api
START-OF-SELECTION.

* ---------- local data
  DATA lt_msg      TYPE TABLE OF bapiret2.
  DATA lv_response TYPE string.
  DATA lv_conttype TYPE string.
  DATA lv_message  TYPE string.


* --------- init tooling
  DATA(lo_gui_utils) = zcl_btocs_factory=>create_gui_util( ).
  DATA(lo_logger)    = lo_gui_utils->get_logger( ).

  DATA(lo_connector) = zcl_btocs_factory=>create_web_service_connector( ).
  lo_connector->set_logger( lo_logger ).


* ---------- set endpoint/method
  IF p_rfc IS INITIAL.
    lo_logger->error( |rfc destination is required| ).
  ELSE.
    IF lo_connector->set_endpoint( p_rfc ) EQ abap_false.
      lo_logger->error( |set endpoint failed| ).
    ENDIF.
  ENDIF.

* ----------- prepare
  IF lo_connector->is_initialized( ) EQ abap_true.
* ----------- execute
    DATA(lo_response) = lo_connector->execute_get( iv_url = |/hello| ).
    IF lo_response IS INITIAL.
      lo_logger->error( |no response| ).
    ELSE.
* ----------- check result
      lv_response = lo_response->get_content( ).
      lv_conttype = lo_response->get_content_type( ).
      lt_msg      = lo_logger->get_messages( ).

      IF lv_response IS INITIAL.
        lo_logger->error( |no response| ).
      ELSE.
        IF lo_response->is_json_object( ) EQ abap_false.
          lo_logger->warning( |no json response| ).
        ELSE.
* ------------ get values from json object
          DATA(lo_answer) = lo_response->get_values_from_parsed_json( )->get_structure_value( ).
          lv_message = lo_answer->get_string( 'message' ).
        ENDIF.
      ENDIF.
    ENDIF.

* ------------ destroy
    lo_connector->destroy( ).
  ENDIF.


* ------------ Output results
END-OF-SELECTION.

* ------------- output raw response
  IF lv_response IS NOT INITIAL.
    cl_demo_output=>begin_section( title = |Response| ).
    cl_demo_output=>write_text( text = |Content-Type: { lv_conttype }| ).
    cl_demo_output=>write_html( lv_response ).
    cl_demo_output=>end_section( ).
  ENDIF.

* ------------- output extracted json data
  IF lv_message IS NOT INITIAL.
    cl_demo_output=>begin_section( title = |Data| ).
    cl_demo_output=>write_text( text = |message: { lv_message }| ).
    cl_demo_output=>end_section( ).
  ENDIF.

* ------------ output trace trace
  IF p_trace eq abap_true
    and lt_msg[] IS NOT INITIAL.
    cl_demo_output=>begin_section( title = |Trace| ).
    cl_demo_output=>write_data(
      value   = lt_msg
      name    = 'Trace'
    ).
    cl_demo_output=>end_section( ).
  ENDIF.

* ------------ display data
  cl_demo_output=>display( ).