METHOD get_process_steps.
    DATA: lt_range      TYPE rsds_frange_t,
          lt_dfies_from TYPE STANDARD TABLE OF dfies,
          lt_dfies_to   TYPE STANDARD TABLE OF dfies,
          lt_comp       TYPE abap_component_tab.

    FIELD-SYMBOLS <lt_range>  TYPE rsds_selopt_t.

    READ TABLE mt_param ASSIGNING FIELD-SYMBOL(<ls_mparam>)
      WITH KEY process = iv_process BINARY SEARCH.
    IF sy-subrc = 0.
      IF et_params IS REQUESTED.
        et_params = VALUE #( FOR <ls_par> IN mt_param
                             WHERE ( process = iv_process )
                              ( <ls_par> ) ).
        SORT et_params BY sequence ASCENDING.
        RETURN.
      ENDIF.

    ENDIF.

    SELECT sequence,
           ref,
           from_table,
           to_table
      FROM zbw_t_process
      INTO TABLE @DATA(lt_process)
      WHERE process = @iv_process
        AND active  = @abap_true
      ORDER BY sequence ASCENDING.

    IF sy-subrc EQ 0.

      SELECT ref,
             setkind,
             fieldname,
             pos,
             sign,
             opti AS option,
             low,
             high
      FROM zbw_t_param
      INTO TABLE @DATA(lt_param)
          FOR ALL ENTRIES IN @lt_process
      WHERE ref = @lt_process-ref.
      SORT lt_param BY pos ASCENDING.
    ENDIF.

    LOOP AT lt_process ASSIGNING FIELD-SYMBOL(<ls_proc>).

      CLEAR: lt_range,
             lt_dfies_from,
             lt_dfies_to,
             lt_comp.

      APPEND INITIAL LINE TO et_params ASSIGNING FIELD-SYMBOL(<ls_params>).
      <ls_params>-process   = iv_process.
      <ls_params>-sequence  = <ls_proc>-sequence.
      <ls_params>-ref       = <ls_proc>-ref.

* Get FROM TABLE Structure
      IF <ls_proc>-from_table IS NOT INITIAL.
        CALL FUNCTION 'DDIF_FIELDINFO_GET'
          EXPORTING
            tabname        = <ls_proc>-from_table
          TABLES
            dfies_tab      = lt_dfies_from
          EXCEPTIONS
            not_found      = 1
            internal_error = 2
            OTHERS         = 3.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

      ENDIF.

* Get TO TABLE Structure
      IF <ls_proc>-to_table IS NOT INITIAL.
        CALL FUNCTION 'DDIF_FIELDINFO_GET'
          EXPORTING
            tabname        = <ls_proc>-to_table
          TABLES
            dfies_tab      = lt_dfies_to
          EXCEPTIONS
            not_found      = 1
            internal_error = 2
            OTHERS         = 3.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
      ENDIF.

* Get Target and WHERE
      LOOP AT lt_param ASSIGNING FIELD-SYMBOL(<ls_param>)
                                        WHERE ref = <ls_proc>-ref.

        CASE <ls_param>-setkind.
          WHEN '9'. " WHERE

*      Check for dynamic entry
            IF <ls_param>-low(1) = '{' OR <ls_param>-high(1) = '{'.

              DO 2 TIMES.
                DATA(lv_value) = SWITCH string( sy-index
                                                WHEN 1 THEN 'LOW'
                                                WHEN 2 THEN 'HIGH' ).
                ASSIGN COMPONENT lv_value OF STRUCTURE <ls_param>
                TO FIELD-SYMBOL(<la_value>).

                IF <la_value>(1) = '{'.
                  REPLACE '{' INTO <la_value> WITH space.
                  REPLACE '}' INTO <la_value> WITH space.
                  CONDENSE <la_value> NO-GAPS.

                  ASSIGN COMPONENT <la_value>
                  OF STRUCTURE cs_target TO FIELD-SYMBOL(<la_val>).

                  IF sy-subrc EQ 0.
                    <la_value> = <la_val>.
                  ENDIF.

                ENDIF.

              ENDDO.
            ENDIF.

* Fill where select options
            READ TABLE lt_range ASSIGNING FIELD-SYMBOL(<ls_frt>)
            WITH KEY fieldname = <ls_param>-fieldname.
            IF sy-subrc EQ 0.

              APPEND VALUE #( sign    = <ls_param>-sign
                              option  = <ls_param>-option
                              low     = <ls_param>-low
                              high    = <ls_param>-high ) TO <ls_frt>-selopt_t.

            ELSE.
              APPEND INITIAL LINE TO lt_range ASSIGNING <ls_frt>.
              <ls_frt> = VALUE #( fieldname = <ls_param>-fieldname
                                  selopt_t  = VALUE #(
                                    ( sign    = <ls_param>-sign
                                      option  = <ls_param>-option
                                      low     = <ls_param>-low
                                      high    = <ls_param>-high ) )
                                  ).

            ENDIF.

          WHEN '1'. "Target

            IF et_params IS REQUESTED.
              IF <ls_proc>-from_table IS INITIAL.
                ASSIGN COMPONENT <ls_param>-fieldname
                OF STRUCTURE cs_target TO <la_val>.
                IF sy-subrc EQ 0.

                  <la_val> = <ls_param>-low.

                  APPEND INITIAL LINE TO lt_comp ASSIGNING FIELD-SYMBOL(<ls_comp>).

                  <ls_comp>-name = <ls_param>-fieldname.
                  <ls_comp>-type ?= cl_abap_elemdescr=>describe_by_data( <la_val> ).

                ENDIF.
              ENDIF.
            ENDIF.

        ENDCASE.
      ENDLOOP.

* WHERE String
      IF et_params IS NOT REQUESTED AND cs_target IS REQUESTED.

        LOOP AT lt_range ASSIGNING FIELD-SYMBOL(<ls_where>).

          DATA(lv_targ) = |CS_TARGET-| && <ls_where>-fieldname.
          ASSIGN (lv_targ) TO <lt_range>.
          IF sy-subrc EQ 0.
            APPEND LINES OF <ls_where>-selopt_t TO <lt_range>.
          ENDIF.
        ENDLOOP.
        <ls_params>-trg_str = REF #( cs_target ).

      ELSE.

        IF iv_where_tab IS NOT INITIAL.
          <ls_params>-where_range = lt_range.
        ELSE.
          LOOP AT lt_range ASSIGNING <ls_frt>.
            DATA(lv_tabix) = sy-tabix.

            DATA(lv_where) = cl_lib_seltab=>new( it_sel = <ls_frt>-selopt_t
                         )->sql_where_condition(
                                  iv_field = CONV #( <ls_frt>-fieldname ) ).

            CASE lv_tabix.
              WHEN 1.
                <ls_params>-where = <ls_params>-where && | ( | && lv_where && | ) |.
              WHEN OTHERS.
                <ls_params>-where = <ls_params>-where && | AND ( | && lv_where && | ) |.
            ENDCASE.
          ENDLOOP.
          REPLACE ALL OCCURRENCES OF ')' IN <ls_params>-where WITH ' ) '.
        ENDIF.
      ENDIF.

** Build new structure with the replacement fields
      IF lt_comp IS NOT INITIAL.
        TRY.
            DATA(lh_str) = cl_abap_structdescr=>create(
                                      p_components = lt_comp ).
          CATCH cx_sy_struct_creation .
        ENDTRY.

        CREATE DATA <ls_params>-trg_str TYPE HANDLE lh_str.
        ASSIGN <ls_params>-trg_str->* TO FIELD-SYMBOL(<ls_wa>).
        <ls_wa> = CORRESPONDING #( cs_target ).

      ENDIF.

    ENDLOOP.

    APPEND LINES OF et_params TO mt_param.
    SORT mt_param BY process sequence ASCENDING.
  ENDMETHOD.
