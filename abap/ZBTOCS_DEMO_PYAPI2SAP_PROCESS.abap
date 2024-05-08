*&---------------------------------------------------------------------*
*& PyAPI4SAP Demo - endpoint method /process
*& Send json payload to the python api service via HTTP POST
*& get a JSON array payload back and transform it to ABAP data
*&---------------------------------------------------------------------*
*& (c) 20324 mdjoerg
*& version: 08.05.2024
*& project:  https://github.com/b-tocs/pyapi4abap
*& requires: https://github.com/b-tocs/abap_btocs_core
*&---------------------------------------------------------------------*
REPORT zbtocs_demo_pyapi2sap_process.

* --------- interface
PARAMETERS: p_rfc TYPE rfcdest OBLIGATORY DEFAULT 'EXT_BTOCS_PYAPI4ABAP'.
SELECTION-SCREEN: ULINE.
PARAMETERS: p_value TYPE string LOWER CASE DEFAULT 'PyAPI4SAP is cool'.
PARAMETERS: p_count TYPE i DEFAULT 10.
PARAMETERS: p_offset TYPE i DEFAULT 1.
SELECTION-SCREEN: ULINE.
PARAMETERS: p_trace AS CHECKBOX TYPE zbtocs_flag_display_trace DEFAULT abap_false.


* --------- call api
START-OF-SELECTION.

* ---------- local data
  DATA lt_msg       TYPE TABLE OF bapiret2.
  DATA lv_response  TYPE string.
  DATA lv_conttype  TYPE string.
  DATA: BEGIN OF ls_record,
          index TYPE i,
          value TYPE string,
        END OF ls_record.
  DATA lt_record LIKE TABLE OF ls_record.



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
* ----------- prepare request
    DATA(lo_request) = lo_connector->new_request( ).
    DATA(lo_json)    = lo_request->new_json_object( ).
    DATA(lo_mgr)     = lo_json->get_manager( ).

    lo_json->set(
        iv_name      = 'value'
        io_value     = lo_mgr->new_string( p_value )
    ).

    lo_json->set(
        iv_name      = 'count'
        io_value     = lo_mgr->new_number( p_count )
    ).

    lo_json->set(
        iv_name      = 'offset'
        io_value     = lo_mgr->new_number( p_offset )
    ).


* ----------- execute
    DATA(lo_response) = lo_connector->execute( iv_api_path = '/process' ).
    IF lo_response IS INITIAL
      OR lo_response->is_http_request_success( ) EQ abap_false.
      lo_logger->error( |no response| ).
    ELSE.
* ----------- check result
      lv_response = lo_response->get_content( ).
      lv_conttype = lo_response->get_content_type( ).
      lt_msg      = lo_logger->get_messages( ).

      IF lv_response IS INITIAL.
        lo_logger->error( |no response| ).
      ELSE.
        IF lo_response->is_json_array( ) EQ abap_false.
          lo_logger->warning( |no json response| ).
        ELSE.
          TRY.
* ------------ get json array and loop over records
              DATA(lo_result) = lo_response->get_values_from_parsed_json( )->get_array_value( ).
              IF lo_result IS INITIAL.
                lo_logger->error( |json key 'result' missing| ).
              ELSE.
                DO lo_result->count( ) TIMES.
                  DATA(lo_record) = lo_result->get_structure( iv_index  = sy-index ).
                  CLEAR ls_record.
                  ls_record-index = lo_record->get_number_value( 'index' )->get_integer( ).
                  ls_record-value = lo_record->get_string( 'value' ).
                  APPEND ls_record TO lt_record.
                ENDDO.
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
  IF lt_record[] IS NOT INITIAL.
    cl_demo_output=>begin_section( title = |Result Data| ).
    cl_demo_output=>write_data(
      value   = lt_record
      name    = 'array'
    ).
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