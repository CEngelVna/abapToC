CLASS zcl_zabap_toc_report DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES tt_range_of_transport TYPE RANGE OF trkorr.
    TYPES tt_range_of_owner     TYPE RANGE OF tr_as4user.
    TYPES tt_range_of_date      TYPE RANGE OF as4date.

    METHODS constructor IMPORTING report_id TYPE sy-repid.

    METHODS gather_transports IMPORTING tranports        TYPE tt_range_of_transport OPTIONAL
                                        owners           TYPE tt_range_of_owner     OPTIONAL
                                        include_released TYPE abap_bool             DEFAULT abap_true
                                        include_tocs     TYPE abap_bool             DEFAULT abap_false
                                        dates            TYPE tt_range_of_date      OPTIONAL
                                        !client          TYPE trclient              DEFAULT sy-mandt
                                         include_parent TYPE any OPTIONAL.

    METHODS display                      IMPORTING layout_name   TYPE slis_vari OPTIONAL.
    METHODS get_layout_from_f4_selection RETURNING VALUE(layout) TYPE slis_vari.

  PRIVATE SECTION.
    TYPES t_icon TYPE c LENGTH 4.
    TYPES: BEGIN OF t_report,
             parent_transport          TYPE strkorr,
             transport                 TYPE trkorr,
             client                    TYPE trclient,
             type                      TYPE trfunction,
             target_system             TYPE tr_target,
             owner                     TYPE tr_as4user,
             creation_date             TYPE as4date,
             description               TYPE as4text,
             create_toc                TYPE t_icon,
             create_release_toc        TYPE t_icon,
             create_release_import_toc TYPE t_icon,
             toc_number                TYPE trkorr,
             toc_status                TYPE string,
             color                     TYPE lvc_t_scol,
           END OF t_report,
           tt_report TYPE STANDARD TABLE OF t_report WITH KEY transport WITH NON-UNIQUE SORTED KEY toc COMPONENTS toc_number.

    CONSTANTS: BEGIN OF c_icon,
                 create                TYPE t_icon VALUE '@EZ@',
                 create_release        TYPE t_icon VALUE '@4A@',
                 create_release_import TYPE t_icon VALUE '@K5@',
               END OF c_icon.
    CONSTANTS: BEGIN OF c_toc_columns,
                 create_toc                TYPE string VALUE 'CREATE_TOC',
                 create_release_toc        TYPE string VALUE 'CREATE_RELEASE_TOC',
                 create_release_import_toc TYPE string VALUE 'CREATE_RELEASE_IMPORT_TOC',
               END OF c_toc_columns.
    CONSTANTS: BEGIN OF c_status_color,
                 green  TYPE i VALUE 5,
                 yellow TYPE i VALUE 3,
                 red    TYPE i VALUE 6,
               END OF c_status_color.
    CONSTANTS c_status_check_interval_sec TYPE i VALUE 5.

    DATA timer              TYPE REF TO cl_gui_timer.
    DATA alv_table          TYPE REF TO cl_salv_table.
    DATA toc_manager        TYPE REF TO zcl_zabap_toc.
    DATA layout_key         TYPE salv_s_layout_key.
    DATA report_data        TYPE tt_report.
    DATA tocs_to_check      TYPE HASHED TABLE OF trkorr WITH UNIQUE KEY table_line.
    DATA m_tranports        TYPE tt_range_of_transport.
    DATA m_owners           TYPE tt_range_of_owner.
    DATA m_include_released TYPE abap_bool.
    DATA m_include_tocs     TYPE abap_bool.
    DATA m_dates            TYPE tt_range_of_date.
    DATA m_client           TYPE trclient.
    DATA m_include_parent TYPE abap_bool.

    METHODS set_column_hotspot_icon IMPORTING !column TYPE lvc_fname.

    METHODS set_fixed_column_text IMPORTING !column TYPE lvc_fname
                                            !text   TYPE scrtext_l.

    METHODS set_status_color IMPORTING !row   TYPE i
                                       !color TYPE i.

    METHODS set_entry_color IMPORTING !entry TYPE REF TO t_report
                                      !color TYPE i.

    METHODS set_status_timer  IMPORTING transport_to_check TYPE trkorr.
    METHODS prepare_alv_table IMPORTING layout_name        TYPE slis_vari OPTIONAL.
    METHODS update_import_status.
    METHODS on_timer_finished FOR EVENT finished OF cl_gui_timer IMPORTING sender.
    METHODS on_link_click FOR EVENT link_click OF cl_salv_events_table     IMPORTING !row !column.

    METHODS on_user_command FOR EVENT added_function OF cl_salv_events
      IMPORTING e_salv_function.

    METHODS on_double_click FOR EVENT double_click OF cl_salv_events_table        IMPORTING !row !column.
    METHODS show_transport_details IMPORTING transport TYPE trkorr.
ENDCLASS.


CLASS zcl_zabap_toc_report IMPLEMENTATION.
  METHOD constructor.
    layout_key = VALUE salv_s_layout_key( report = report_id ).
    toc_manager = NEW #( ).
    timer = NEW #( ).
    timer->interval = c_status_check_interval_sec.
    SET HANDLER on_timer_finished FOR timer.
  ENDMETHOD.

  METHOD gather_transports.
    m_tranports = tranports.
    m_owners = owners.
    m_include_released = include_released.
    m_include_tocs = include_tocs.
    m_client = client.
    m_dates = dates.
    m_include_parent = include_parent.

    SELECT
      FROM e070
             LEFT JOIN
               e070 AS parent ON parent~trkorr = e070~strkorr
                 LEFT JOIN
                   e07t ON e07t~trkorr = e070~trkorr
                     LEFT OUTER JOIN
                       e070c ON e070c~trkorr = e070~trkorr
      FIELDS e070~strkorr                    AS parent_transport,
             e070~trkorr                     AS transport,
             e070c~client                    AS client,
             e070~trfunction                 AS type,
             CASE WHEN parent~trkorr IS NULL
                THEN e070~tarsystem
                ELSE parent~tarsystem
             END                             AS target_system,
             e070~as4user                    AS owner,
             e070~as4date                    AS creation_date,
             e07t~as4text                    AS description,
             @c_icon-create                  AS create_toc,
             @c_icon-create_release          AS create_release_toc,
             @c_icon-create_release_import   AS create_release_import_toc
      WHERE ( e070~trkorr IN @m_tranports OR e070~strkorr IN @m_tranports )
        AND e070~as4user IN @m_owners
        AND ( @m_include_parent   = @abap_true OR e070~strkorr    <> @space )
        AND ( @m_include_released = @abap_true OR e070~trstatus   IN ( 'L', 'D' ) )
        AND ( @m_include_tocs     = @abap_true OR e070~trfunction <> 'T' )
        AND e070~as4date IN @m_dates
        AND e070c~client  = @m_client
        AND e070~trkorr  IN ( SELECT trkorr FROM e071 WHERE trkorr = e070~trkorr )
      ORDER BY e070~trkorr DESCENDING,
               e070~as4date DESCENDING
      INTO CORRESPONDING FIELDS OF TABLE @report_data.

    DELETE ADJACENT DUPLICATES FROM report_data COMPARING transport.
  ENDMETHOD.

  METHOD display.
    prepare_alv_table( layout_name ).
    alv_table->display( ).
  ENDMETHOD.

  METHOD on_link_click.
    DATA(selected) = REF #( report_data[ row ] ).
    CLEAR selected->color.
    DELETE tocs_to_check WHERE table_line = selected->toc_number.

    TRY.
        CASE column.
          "--------------------------------------------------
          WHEN c_toc_columns-create_toc.
            IF selected->toc_number IS INITIAL.
              selected->toc_number = toc_manager->create( source_transport = selected->transport
                                                          target_system    = selected->target_system ).
            ENDIF.
            selected->toc_status = TEXT-s01.
            set_status_color( row   = row
                              color = c_status_color-green ).

          "--------------------------------------------------
          WHEN c_toc_columns-create_release_toc.
            IF selected->toc_number IS INITIAL.
              selected->toc_number = toc_manager->create( source_transport = selected->transport
                                                          target_system    = selected->target_system ).
            ENDIF.
            toc_manager->release( selected->toc_number ).
            selected->toc_status = TEXT-s02.
            set_status_color( row   = row
                              color = c_status_color-green ).

          "--------------------------------------------------
          WHEN c_toc_columns-create_release_import_toc.
            IF selected->toc_number IS INITIAL.
              selected->toc_number = toc_manager->create( source_transport = selected->transport
                                                          target_system    = selected->target_system ).
            ENDIF.
            toc_manager->release( selected->toc_number ).
            DATA(rc) = CONV i( toc_manager->import( toc           = selected->toc_number
                                                    target_system = selected->target_system ) ).
            selected->toc_status = TEXT-s03.
            " set_status_timer( selected->toc_number ).
            selected->toc_status = replace( val  = TEXT-s04
                                            sub  = '&1'
                                            with = |{ rc }| ).
            set_status_color( row   = row
                              color = COND #( WHEN rc = 0 THEN c_status_color-green
                                              WHEN rc = 4 THEN c_status_color-yellow
                                              ELSE             c_status_color-red ) ).

          "--------------------------------------------------
          WHEN OTHERS.
        ENDCASE.

      CATCH zcx_zabap_exception INTO DATA(exception).
        selected->toc_status = exception->get_text( ).
        set_status_color( row   = row
                          color = c_status_color-red ).

    ENDTRY.

    alv_table->refresh( refresh_mode = if_salv_c_refresh=>full ).
  ENDMETHOD.

  METHOD set_column_hotspot_icon.
    DATA(col) = CAST cl_salv_column_table( me->alv_table->get_columns( )->get_column( column ) ).
    col->set_icon( if_salv_c_bool_sap=>true ).
    col->set_cell_type( if_salv_c_cell_type=>hotspot ).
  ENDMETHOD.

  METHOD set_fixed_column_text.
    DATA(col) = alv_table->get_columns( )->get_column( column ).
    IF strlen( text ) > 20.
      col->set_long_text( text ).
      col->set_fixed_header_text( 'L' ).
    ELSEIF strlen( text ) > 10.
      col->set_long_text( text ).
      col->set_medium_text( CONV #( text ) ).
      col->set_fixed_header_text( 'M' ).
    ELSE.
      col->set_long_text( text ).
      col->set_medium_text( CONV #( text ) ).
      col->set_short_text( CONV #( text ) ).
      col->set_fixed_header_text( 'S' ).
    ENDIF.
  ENDMETHOD.

  METHOD prepare_alv_table.
    cl_salv_table=>factory( EXPORTING r_container  = cl_gui_container=>screen0
                            IMPORTING r_salv_table = alv_table

                            CHANGING  t_table      = report_data ).

    " Set columns as icons
    set_column_hotspot_icon( CONV #( c_toc_columns-create_toc ) ).
    set_column_hotspot_icon( CONV #( c_toc_columns-create_release_toc ) ).
    set_column_hotspot_icon( CONV #( c_toc_columns-create_release_import_toc ) ).

    " Set column texts
    set_fixed_column_text( column = CONV #( c_toc_columns-create_toc )
                           text   = CONV #( TEXT-c01 ) ).
    set_fixed_column_text( column = CONV #( c_toc_columns-create_release_toc )
                           text   = CONV #( TEXT-c02 ) ).
    set_fixed_column_text( column = CONV #( c_toc_columns-create_release_import_toc )
                           text   = CONV #(  TEXT-c03 ) ).
    set_fixed_column_text( column = 'TOC_NUMBER'
                           text   = CONV #( TEXT-c04 ) ).
    set_fixed_column_text( column = 'TOC_STATUS'
                           text   = CONV #( TEXT-c05 ) ).

    " Set handlers
    DATA(event) = alv_table->get_event( ).
    SET HANDLER me->on_link_click FOR event.
    SET HANDLER me->on_user_command FOR event.
    SET HANDLER me->on_double_click FOR event.

    " Set layouts
    alv_table->get_layout( )->set_key( layout_key ).
    alv_table->get_layout( )->set_save_restriction( if_salv_c_layout=>restrict_none ).
    alv_table->get_layout( )->set_default( abap_true ).
    IF layout_name IS NOT INITIAL.
      alv_table->get_layout( )->set_initial_layout( layout_name ).
    ENDIF.
    " Enable standard report functions
    alv_table->get_functions( )->set_default( ).

    alv_table->get_functions( )->add_function( name     = 'ONCTOC'
                                               icon     = CONV #( c_icon-create_release_import )
                                               text     = CONV #( TEXT-c03 )
                                               tooltip  = CONV #( TEXT-c03 )
                                               position = 1 ). " Positioning Function

    alv_table->get_functions( )->add_function( name     = 'REFRESH'
                                               icon     = CONV #( icon_refresh )
                                               text     = 'Refresh'
                                               tooltip  = 'Refresh'
                                               position = 1 ). " Positioning Function

    alv_table->get_selections( )->set_selection_mode( value = if_salv_c_selection_mode=>row_column ).

    " Color
    alv_table->get_columns( )->set_color_column( 'COLOR' ).
  ENDMETHOD.

  METHOD get_layout_from_f4_selection.
    layout = cl_salv_layout_service=>f4_layouts( s_key    = layout_key
                                                 restrict = if_salv_c_layout=>restrict_none )-layout.
  ENDMETHOD.

  METHOD set_status_color.
    DATA(color_cell) = REF #( report_data[ row ]-color ).
    CLEAR color_cell->*.
    APPEND VALUE #( fname = 'TOC_STATUS'
                    color = VALUE #( col = color ) ) TO color_cell->*.
  ENDMETHOD.

  METHOD set_entry_color.
    CLEAR entry->color.
    APPEND VALUE #( fname = 'TOC_STATUS'
                    color = VALUE #( col = color ) ) TO entry->color.
  ENDMETHOD.

  METHOD on_timer_finished.
    update_import_status( ).
    IF lines( tocs_to_check ) > 0.
      sender->interval = c_status_check_interval_sec.
      sender->run( ).
    ENDIF.
  ENDMETHOD.

  METHOD set_status_timer.
    IF NOT line_exists( tocs_to_check[ table_line = transport_to_check ] ).
      INSERT transport_to_check INTO TABLE tocs_to_check.
    ENDIF.

    timer->run( ).
  ENDMETHOD.

  METHOD update_import_status.
    DATA tocs_to_remove TYPE RANGE OF trkorr.

    LOOP AT tocs_to_check REFERENCE INTO DATA(toc).
      DATA(entry) = REF #( me->report_data[ KEY toc toc_number = toc->* ] OPTIONAL ).
      IF entry IS NOT BOUND.
        APPEND VALUE #( sign   = 'I'
                        option = 'EQ'
                        low    = toc->* ) TO tocs_to_remove.
        CONTINUE.
      ENDIF.

      TRY.
          toc_manager->check_status_in_system( EXPORTING toc      = toc->*
                                                         system   = entry->target_system
                                               IMPORTING imported = DATA(imported)
                                                         rc       = DATA(rc) ).
          IF imported = abap_true.
            entry->toc_status = replace( val  = TEXT-s04
                                         sub  = '&1'
                                         with = |{ rc }| ).
            set_entry_color( entry = entry
                             color = COND #( WHEN rc = 0 THEN c_status_color-green
                                             WHEN rc = 8 THEN c_status_color-red
                                             ELSE             c_status_color-yellow ) ).
            APPEND VALUE #( sign   = 'I'
                            option = 'EQ'
                            low    = toc->* ) TO tocs_to_remove.
          ENDIF.

        CATCH zcx_zabap_exception INTO DATA(exception).
          entry->toc_status = exception->get_text( ).
          set_entry_color( entry = entry
                           color = c_status_color-red ).

      ENDTRY.
    ENDLOOP.
    IF lines( tocs_to_remove ) > 0.
      DELETE tocs_to_check WHERE table_line IN tocs_to_remove.
    ENDIF.

    alv_table->refresh( s_stable     = VALUE #( )
                        refresh_mode = if_salv_c_refresh=>full ).
    cl_gui_cfw=>set_new_ok_code( new_code = '&REFRESHG' ).
  ENDMETHOD.

  METHOD on_user_command.
    DATA toc_number TYPE trkorr.
    DATA rc         TYPE i.

    CASE e_salv_function.
      WHEN 'ONCTOC'.

        DATA(selected_rows) = alv_table->get_selections( )->get_selected_rows( ).

        TRY.
            LOOP AT selected_rows INTO DATA(row).
              DATA(selected) = REF #( report_data[ row ] ).

              TRY.
                  IF toc_number IS INITIAL.
                    toc_number = toc_manager->create( source_transport = selected->transport
                                                      target_system    = selected->target_system ).

                  ELSE.

                    toc_manager->import_objects( source_transport      = selected->transport
                                                 destination_transport = toc_number ).

                  ENDIF.

                  selected->toc_number = toc_number.
                CATCH zcx_zabap_exception INTO DATA(exception).
                  selected->toc_status = exception->get_text( ).
                  set_status_color( row   = row
                                    color = c_status_color-red ).
              ENDTRY.
            ENDLOOP.

            toc_manager->release( toc_number ).
            rc = toc_manager->import( toc           = toc_number
                                      target_system = selected->target_system ).

          CATCH zcx_zabap_exception INTO exception.
            LOOP AT selected_rows INTO row.
              selected = REF #( report_data[ row ] ).

              selected->toc_status = exception->get_text( ).
              set_status_color( row   = row
                                color = c_status_color-red ).
            ENDLOOP.
            LOOP AT selected_rows INTO row.
              TRY.

                  selected = REF #( report_data[ row ] ).
                  selected->toc_status = TEXT-s03.
*              set_status_timer( selected->toc_number ).
                  selected->toc_status = replace( val  = TEXT-s04
                                                  sub  = '&1'
                                                  with = |{ rc }| ).
                  set_status_color( row   = row
                                    color = COND #( WHEN rc = 0 THEN c_status_color-green
                                                    WHEN rc = 4 THEN c_status_color-yellow
                                                    ELSE             c_status_color-red ) ).
                CATCH zcx_zabap_exception INTO exception.
                  selected->toc_status = exception->get_text( ).
                  set_status_color( row   = row
                                    color = c_status_color-red ).

              ENDTRY.
            ENDLOOP.

        ENDTRY.

      WHEN 'REFRESH'.
        gather_transports( tranports        = m_tranports
                           owners           = m_owners
                           include_released = m_include_released
                           include_tocs     = m_include_tocs
                           dates            = m_dates
                           client           = m_client ).

    ENDCASE.

    alv_table->refresh( refresh_mode = if_salv_c_refresh=>full ).
  ENDMETHOD.

  METHOD show_transport_details.
    DATA batch_input TYPE TABLE OF bdcdata.

    APPEND VALUE #( program  = 'RDDM0001'
                    dynpro   = '0200'
                    dynbegin = 'X'
                    fnam     = 'BDC_CURSOR'
                    fval     = 'TRDYSE01SN-TR_TRKORR'  ) TO batch_input.
    APPEND VALUE #( fnam = 'TRDYSE01SN-TR_TRKORR'
                    fval = transport ) TO batch_input.
    APPEND VALUE #( fnam = 'BDC_OKCODE'
                    fval = '=SINGLE_REQUEST' ) TO batch_input.

    CALL TRANSACTION 'SE01' USING batch_input MODE 'E' UPDATE 'S'.
  ENDMETHOD.

  METHOD on_double_click.
    DATA(selected) = REF #( report_data[ row ] ).

    CASE column.
      WHEN 'TRANSPORT'.
        show_transport_details( selected->transport ).

      WHEN OTHERS.

    ENDCASE.
  ENDMETHOD.
ENDCLASS.
