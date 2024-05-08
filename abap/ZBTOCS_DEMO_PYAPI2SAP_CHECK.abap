*&---------------------------------------------------------------------*
*& PyAPI4SAP Demo - endpoint method /check
*& Send custom path and request parameters to the /check method as
*& HTTP GET request and get a structured json response back
*&---------------------------------------------------------------------*
*& (c) 20324 mdjoerg
*& version: 08.05.2024
*& project:  https://github.com/b-tocs/pyapi4abap
*& requires: https://github.com/b-tocs/abap_btocs_core
*&---------------------------------------------------------------------*
REPORT zbtocs_demo_pyapi2sap_check.

* --------- interface
PARAMETERS: p_rfc TYPE rfcdest OBLIGATORY DEFAULT 'EXT_BTOCS_PYAPI4ABAP'.
SELECTION-SCREEN: ULINE.
PARAMETERS: p_value TYPE i DEFAULT 100.
PARAMETERS: p_oval1 TYPE string LOWER CASE DEFAULT 'PyAPI4SAP is cool'.
PARAMETERS: p_oval2 TYPE p DECIMALS 3 DEFAULT '0.987'.
SELECTION-SCREEN: ULINE.
PARAMETERS: p_trace AS CHECKBOX TYPE zbtocs_flag_display_trace DEFAULT abap_false.


* --------- call api
START-OF-SELECTION.

* ---------- local data
  DATA lt_msg       TYPE TABLE OF bapiret2.
  DATA lv_response  TYPE string.
  DATA lv_conttype  TYPE string.
  DATA lv_value     TYPE i.
  DATA lv_oval1     TYPE string.
  DATA lv_oval2_str TYPE string.
  DATA lv_oval2_flt TYPE f.
  DATA lv_oval2_dec TYPE decfloat16.
  DATA lv_oval2_pkd TYPE p DECIMALS 4.



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
* ----------- build path
    DATA(lv_path) = |/check/{ p_value }?opt_value1={ p_oval1 }&opt_value2={ p_oval2 }|.
    lo_logger->debug( |used path: { lv_path }| ).
* ----------- execute
    DATA(lo_response) = lo_connector->execute_get( iv_url = lv_path ).
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
          TRY.
* ------------ get json object 'result'
              DATA(lo_answer) = lo_response->get_values_from_parsed_json( )->get_structure_value( ).
              DATA(lo_result) = lo_answer->get( 'result' )->get_structure_value( ).
              IF lo_result IS INITIAL.
                lo_logger->error( |json key 'result' missing| ).
              ELSE.
* ------------ get values from json object 'result'
                lv_value = lo_result->get_number_value( 'value' )->get_integer( ).
                lv_oval1 = lo_result->get_string( 'opt_value1' ).
                lv_oval2_str = lo_result->get_string( 'opt_value2' ).

*               get values from json value object for numbers - different options
                DATA(lo_oval2) = lo_result->get_number_value( 'opt_value2' ).
                lv_oval2_flt = lo_oval2->get_float( ).
                lo_oval2->get_as( CHANGING cv_value = lv_oval2_dec ).
                lo_oval2->get_as( CHANGING cv_value = lv_oval2_pkd ).
              ENDIF.
            CATCH cx_root INTO DATA(lx_exc).
              lo_logger->error( |Exception: { lx_exc->get_text( ) }| ).
          ENDTRY.
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
  IF lv_value IS NOT INITIAL.
    cl_demo_output=>begin_section( title = |Result Data| ).
    cl_demo_output=>write_text( text = |value: { lv_value }| ).
    cl_demo_output=>write_text( text = |opt_value1: { lv_oval1 }| ).
    IF lv_oval2_str IS NOT INITIAL.
      cl_demo_output=>write_text( text = |opt_value2 (string): { lv_oval2_str }| ).
      cl_demo_output=>write_text( text = |opt_value2 (float): { lv_oval2_flt }| ).
      cl_demo_output=>write_text( text = |opt_value2 (decimal): { lv_oval2_dec }| ).
      cl_demo_output=>write_text( text = |opt_value2 (packed): { lv_oval2_pkd }| ).
    ENDIF.
    cl_demo_output=>end_section( ).
  ENDIF.

* ------------ output trace trace
  IF p_trace EQ abap_true
    AND lt_msg[] IS NOT INITIAL.
    cl_demo_output=>begin_section( title = |Trace| ).
    cl_demo_output=>write_data(
      value   = lt_msg
      name    = 'Trace'
    ).
    cl_demo_output=>end_section( ).
  ENDIF.

* ------------ display data
  cl_demo_output=>display( ).