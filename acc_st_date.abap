METHOD if_ex_workorder_update~at_save.

  TYPES: BEGIN OF ts_afvg.
          INCLUDE TYPE afvgb.
  TYPES: indold TYPE sy-tabix,
         END OF ts_afvg,
         ti_afvg TYPE STANDARD TABLE OF ts_afvg.

  TYPES:   BEGIN OF jest_buf .
          INCLUDE TYPE jest_upd.
  TYPES:     mod       TYPE char1,
             inact_old TYPE jest-inact,
             END OF jest_buf,
             tt_del TYPE STANDARD TABLE OF jest_buf.

  DATA: lv_vornr  TYPE vornr.

  FIELD-SYMBOLS : <fs_all_operations> TYPE ti_afvg. " All operations
  FIELD-SYMBOLS : <fs_filled_isdd> TYPE ti_afvg.    " Operations with actual start date
  FIELD-SYMBOLS : <fs_del> TYPE tt_del.             " Buffer structure for deleted status

  IF sy-tcode <> 'CO02'.
    EXIT.
  ENDIF.

  ASSIGN ('(SAPLCOBO)AFVG_BT[]') TO <fs_all_operations>. " Operation list
  IF <fs_all_operations> IS ASSIGNED.

    SORT <fs_all_operations> BY vornr.

    DATA lt_filled_isdd TYPE ti_afvg.
    ASSIGN lt_filled_isdd TO <fs_filled_isdd>.

*  All operations with filled actual start date (isdd)
    LOOP AT <fs_all_operations> ASSIGNING FIELD-SYMBOL(<ls_data>).
      IF <ls_data>-isdd IS NOT INITIAL.
        APPEND <ls_data> TO <fs_filled_isdd>.
      ENDIF.
    ENDLOOP.

    LOOP AT <fs_all_operations> ASSIGNING <ls_data>.

* Check if the operation exist in db or is new
      SELECT SINGLE afvc~vornr
        INTO lv_vornr
        FROM afvc
        INNER JOIN afko
        ON afvc~aufpl = afko~aufpl
        WHERE afvc~vornr = <ls_data>-vornr
          AND afko~aufnr = is_header_dialog-aufnr.

      IF sy-subrc = 0.
        CONTINUE.
      ELSE.

* Assign start date from next available operation if missing
        IF <ls_data>-isdd IS INITIAL.

          LOOP AT <fs_all_operations> ASSIGNING FIELD-SYMBOL(<ls_data1>) FROM sy-tabix.
            IF <ls_data1>-isdd IS NOT INITIAL.
              <ls_data>-isdd = <ls_data1>-isdd.
              <ls_data>-isdz = sy-uzeit.

              " Clear <ls_data1>-isdd only if isdz, iedd, or iedz are empty
              IF <ls_data1>-isdz IS INITIAL OR <ls_data1>-iedd IS INITIAL OR <ls_data1>-iedz IS INITIAL.
                CLEAR : <ls_data1>-isdd , <ls_data1>-isdz.
              ENDIF.

              EXIT.
            ENDIF.
          ENDLOOP.

        ENDIF.

      ENDIF.
    ENDLOOP.

***                new requirement
* get the buffer struc that holds the operation status on save
    ASSIGN ('(SAPLBSVA)JEST_BUF[]') TO <fs_del>.
    IF <fs_del> IS ASSIGNED.

      LOOP AT <fs_del> ASSIGNING FIELD-SYMBOL(<ls_del>).

*  Check if the operation status is being deleted (status I0013)
        IF <ls_del>-mod = 'I' AND <ls_del>-stat = 'I0013'.

* Check if the operation was already marked as deleted in DB
          SELECT SINGLE stat
            INTO @DATA(lv_db_stat)
            FROM jest
            WHERE objnr = @<ls_del>-objnr
            AND inact = ' '.  " Active status

* If the status is different from db and buffer it means that that operation in being deleted
          IF sy-subrc = 0 AND lv_db_stat <> <ls_del>-stat.

* Get details of the operation marked for deletion
            SELECT afvc~aufpl, afvc~aplzl, afvc~vornr, afvc~objnr
              INTO TABLE @DATA(lt_buffer_del)
              FROM afko
              INNER JOIN afvc ON afko~aufpl = afvc~aufpl
              WHERE afvc~objnr = @<ls_del>-objnr
                AND afko~aufnr = @is_header_dialog-aufnr.

            LOOP AT <fs_all_operations> ASSIGNING <ls_data>.
              READ TABLE lt_buffer_del WITH KEY vornr = <ls_data>-vornr TRANSPORTING NO FIELDS.

              IF sy-subrc = 0. " Operation is marked as deleted

                READ TABLE <fs_all_operations> TRANSPORTING NO FIELDS WITH KEY vornr = <ls_data>-vornr.
                DATA(lv_index) = sy-tabix.

* Get the already deleted operations
                SELECT afvc~aufpl, afvc~aplzl, afvc~vornr, afvc~objnr, jest~stat, tj02t~txt04 AS status_description
                  INTO TABLE @DATA(lt_db_deleted)
                  FROM afko
                  INNER JOIN afvc ON afko~aufpl = afvc~aufpl
                  INNER JOIN jest ON afvc~objnr = jest~objnr
                  INNER JOIN tj02t ON jest~stat = tj02t~istat
                  WHERE afko~aufnr = @is_header_dialog-aufnr
                    AND jest~stat = 'I0013'  "deleted operations
                    AND jest~inact = ' '     " Active statuses only
                  AND spras = @sy-langu.

                " Look for the next operation that is not deleted and with empty isdd
                LOOP AT <fs_all_operations> ASSIGNING FIELD-SYMBOL(<ls_next_data>) FROM lv_index + 1.

                  READ TABLE lt_buffer_del WITH KEY vornr = <ls_next_data>-vornr TRANSPORTING NO FIELDS.

                  READ TABLE lt_db_deleted WITH KEY vornr = <ls_next_data>-vornr TRANSPORTING NO FIELDS.
                  IF sy-subrc <> 0.

                    IF sy-subrc <> 0 AND <ls_next_data>-isdd IS INITIAL .
                      <ls_next_data>-isdd = <ls_data>-isdd.
                      <ls_next_data>-isdz = <ls_data>-isdz.

                      CLEAR: <ls_data>-isdd, <ls_data>-isdz.
                      EXIT.
                    ENDIF.
                  ENDIF.
                ENDLOOP.
              ENDIF.
            ENDLOOP.

          ELSE.
            CONTINUE. " status is the same , no changes
          ENDIF.

        ENDIF.
      ENDLOOP.


    ENDIF.
  ENDIF.

ENDMETHOD.
