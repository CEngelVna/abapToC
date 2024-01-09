CLASS zcl_zabap_toc DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS create IMPORTING source_transport TYPE trkorr
                             target_system    TYPE tr_target
                   RETURNING VALUE(toc)       TYPE trkorr
                   RAISING   zcx_zabap_exception.

    METHODS release IMPORTING toc TYPE trkorr
                    RAISING   zcx_zabap_exception.

    METHODS import IMPORTING toc             TYPE trkorr
                             target_system   TYPE tr_target
                   RETURNING VALUE(ret_code) TYPE trretcode
                   RAISING   zcx_zabap_exception.

    METHODS import_objects IMPORTING source_transport      TYPE trkorr
                                     destination_transport TYPE trkorr
                           RAISING   zcx_zabap_exception.

    METHODS check_status_in_system IMPORTING toc      TYPE trkorr
                                             !system  TYPE tr_target
                                   EXPORTING imported TYPE abap_bool
                                             !rc      TYPE i
                                   RAISING   zcx_zabap_exception.

  PRIVATE SECTION.
    DATA c_transport_type_toc TYPE trfunction VALUE 'T'.
    DATA m_transports         TYPE TABLE OF trkorr.

    METHODS get_toc_description IMPORTING source_transport   TYPE trkorr
                                RETURNING VALUE(description) TYPE string
                                RAISING   zcx_zabap_exception.
ENDCLASS.


CLASS zcl_zabap_toc IMPLEMENTATION.
  METHOD check_status_in_system.
    DATA settings TYPE ctslg_settings.
    DATA cofiles  TYPE ctslg_cofile.

    APPEND system TO settings-systems.

    CALL FUNCTION 'TR_READ_GLOBAL_INFO_OF_REQUEST'
      EXPORTING iv_trkorr   = toc
                is_settings = settings
      IMPORTING es_cofile   = cofiles.

    IF cofiles-exists = abap_false.
      RAISE EXCEPTION TYPE zcx_zabap_exception
        EXPORTING message = CONV #( TEXT-e05 ).
    ENDIF.

    imported = cofiles-imported.
    rc = cofiles-rc.
  ENDMETHOD.

  METHOD create.
    TRY.
        cl_adt_cts_management=>create_empty_request(
          EXPORTING iv_type           = 'T'
                    iv_text           = CONV #( get_toc_description( source_transport ) )
                    iv_target         = target_system
          IMPORTING es_request_header = DATA(transport_header) ).
        import_objects( source_transport      = source_transport
                        destination_transport = transport_header-trkorr ).
        toc = transport_header-trkorr.

      CATCH cx_root INTO DATA(cx).
        RAISE EXCEPTION TYPE zcx_zabap_exception
          EXPORTING message = replace( val  = TEXT-e01
                                       sub  = '&1'
                                       with = cx->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD import.
    DATA error           TYPE string.
    DATA cs_request      TYPE trwbo_request.
    DATA target_system_1 TYPE tr_target.

    DATA(cts_api) = cl_cts_rest_api_factory=>create_instance( ).

    cs_request-h-trkorr = toc.
    cts_api->get_request_data( EXPORTING iv_read_headers = 'X'        " Read Headers of Requests/Tasks
*                                         iv_read_descr   = 'X'        " Read Short Texts for Requests/Tasks
                                         iv_read_client  = 'X'        " Read Source/Target Client of Requests/Tasks
                                         iv_read_target  = 'X'        " Read Target Package/Layer for Requests
*                                         iv_read_objs_keys = ' '        " Read Object Keys
*                                         iv_read_objs    = ' '        " Read Objects
*                                         iv_read_attributes = ' '        " Read Attributes
                               CHANGING  cs_request      = cs_request ). " Request/Task Data
*  CATCH cx_cts_rest_api_exception. " CTS REST API Exception

    target_system_1 = |Q60.{ cs_request-h-client }|.

    CALL FUNCTION 'ZABAP_TOC_UNPACK' DESTINATION 'Q60CLNT200'
      EXPORTING toc           = toc
                target_system = target_system_1
      IMPORTING ret_code      = ret_code
                error         = error.

    IF strlen( error ) > 0.
      RAISE EXCEPTION TYPE zcx_zabap_exception
        EXPORTING message = replace( val  = TEXT-e03
                                     sub  = '&1'
                                     with = error ).
    ENDIF.
  ENDMETHOD.

  METHOD import_objects.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA request_headers     TYPE trwbo_request_headers.
    DATA check_summary       TYPE trcheckresext_tab.
    DATA slin_res            TYPE REF TO cl_ci_transport_check.
    DATA atc_res             TYPE REF TO if_transport_check_service.
    DATA docu_res            TYPE trdocuresults.
    DATA pack_res            TYPE if_cts_rest_api=>ty_pack_check_results.
    DATA gtabkey_res         TYPE gtabkeyerror.
    DATA ev_canceled_by_user TYPE abap_bool.

    CLEAR m_transports.

    CALL FUNCTION 'TR_READ_REQUEST_WITH_TASKS'
      EXPORTING  iv_trkorr          = source_transport
      IMPORTING  et_request_headers = request_headers
      EXCEPTIONS invalid_input      = 1
                 OTHERS             = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_zabap_exception
        EXPORTING message = replace( val  = replace( val  = TEXT-e01
                                                     sub  = '&1'
                                                     with = |{ sy-subrc }| )
                                     sub  = '&2'
                                     with = 'TR_READ_REQUEST_WITH_TASKS' ).
    ENDIF.

    TRY.
        DATA(cts_api) = cl_cts_rest_api_factory=>create_instance( ).
        cts_api->check_objects_and_keys( EXPORTING iv_trkorr        = source_transport
                                         IMPORTING et_check_summary = check_summary " Object Checks: Result (with Check ID)
                                                   eo_slin_res      = slin_res
                                                   et_docu_res      = docu_res      " Results of Documentation Check
                                                   et_pack_res      = pack_res      " Table with Messages from the Package Check for Dict. Objects
                                                   et_gtabkey_res   = gtabkey_res ). " Error Messages After Global Key Check

        CALL FUNCTION 'TR_GET_OBJ_INSPECTION_RESULTS'
          IMPORTING et_summary     = check_summary
                    eo_slin_res    = slin_res
                    et_docu_res    = docu_res
                    et_pack_res    = pack_res
                    et_gtabkey_res = gtabkey_res
                    eo_atc_res     = atc_res.

        LOOP AT check_summary TRANSPORTING NO FIELDS WHERE errors > 0.
          EXIT.
        ENDLOOP.
        IF sy-subrc = 0.

          CALL FUNCTION 'TR_DISP_OBJ_INSPECTION_RESULTS'
            EXPORTING it_summary          = check_summary
                      io_slin_res         = slin_res
                      io_atc_res          = atc_res
                      it_docu_res         = docu_res
                      it_pack_res         = pack_res
                      it_gtabkey_res      = gtabkey_res
            IMPORTING ev_canceled_by_user = ev_canceled_by_user.

          RAISE EXCEPTION TYPE zcx_zabap_exception
            EXPORTING message = |Request: { source_transport } has errors.|.
        ENDIF.
        cts_api->release( iv_trkorr       = source_transport
                          iv_ignore_locks = abap_true ).
      CATCH cx_root INTO DATA(cx).
        RAISE EXCEPTION TYPE zcx_zabap_exception
          EXPORTING message = replace( val  = TEXT-e02
                                       sub  = '&1'
                                       with = cx->get_text( ) ).
    ENDTRY.
    CALL FUNCTION 'TR_COPY_COMM'
      EXPORTING  wi_dialog                = abap_false
                 wi_trkorr_from           = source_transport
                 wi_trkorr_to             = destination_transport
                 wi_without_documentation = abap_false
      EXCEPTIONS db_access_error          = 1                " Database access error
                 trkorr_from_not_exist    = 2                " first correction does not exist
                 trkorr_to_is_repair      = 3                " Target correction is repair
                 trkorr_to_locked         = 4                " Command file TRKORR_TO blocked, (SM12)
                 trkorr_to_not_exist      = 5                " second correction does not exist
                 trkorr_to_released       = 6                " second correction already released
                 user_not_owner           = 7                " User is not owner of first request
                 no_authorization         = 8                " No authorization for this function
                 wrong_client             = 9                " Different clients (source - target)
                 wrong_category           = 10               " Different category (source - target)
                 object_not_patchable     = 11
                 OTHERS                   = 12.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_zabap_exception
        EXPORTING message = replace( val  = replace( val  = TEXT-e01
                                                     sub  = '&1'
                                                     with = |{ sy-subrc }| )
                                     sub  = '&2'
                                     with = 'TR_COPY_COMM' ).
    ENDIF.
  ENDMETHOD.

  METHOD release.
    TRY.
        DATA(cts_api) = cl_cts_rest_api_factory=>create_instance( ).
        cts_api->release( iv_trkorr       = toc
                          iv_ignore_locks = abap_true ).

      CATCH cx_root INTO DATA(cx).
        RAISE EXCEPTION TYPE zcx_zabap_exception
          EXPORTING message = replace( val  = TEXT-e02
                                       sub  = '&1'
                                       with = cx->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_toc_description.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA texts      TYPE STANDARD TABLE OF sval.
    DATA cs_request TYPE trwbo_request.
    DATA answer     TYPE c LENGTH 1.

    DATA(cts_api) = cl_cts_rest_api_factory=>create_instance( ).

    cs_request-h-trkorr = source_transport.

    cts_api->get_request_data( EXPORTING iv_read_headers = 'X'        " Read Headers of Requests/Tasks
                                         iv_read_descr   = 'X'        " Read Short Texts for Requests/Tasks
*                                         iv_read_client  = ' '        " Read Source/Target Client of Requests/Tasks
*                                         iv_read_target  = ' '        " Read Target Package/Layer for Requests
*                                         iv_read_objs_keys = ' '        " Read Object Keys
*                                         iv_read_objs    = ' '        " Read Objects
*                                         iv_read_attributes = ' '        " Read Attributes
                               CHANGING  cs_request      = cs_request ). " Request/Task Data
*  CATCH cx_cts_rest_api_exception. " CTS REST API Exception

    texts = VALUE #( ( fieldname = 'DESCRIPTION' value = cs_request-h-as4text ) ).

    CALL FUNCTION 'POPUP_TO_GET_VALUE'
      EXPORTING fieldname = 'AS4TEXT'
                tabname   = 'E070V'
                titel     = 'TransportText'
                valuein   = 'K' && cs_request-h-as4text+1
      IMPORTING answer    = answer
                valueout  = description.
    IF sy-subrc <> 0 OR answer = 'C'.
      RAISE EXCEPTION TYPE zcx_zabap_exception
        EXPORTING message = 'Cancel'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
