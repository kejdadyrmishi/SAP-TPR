REPORT zkd_alv_total.

DATA : lv_ebeln TYPE ebeln.
SELECT-OPTIONS: s_ebeln FOR lv_ebeln.

CLASS lcl_alv DEFINITION FINAL .

  PUBLIC SECTION.
    METHODS: execute.

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_alv,
             ebeln TYPE ebeln,
             ebelp TYPE ebelp,
             txz01 TYPE txz01,
             netpr TYPE bprei,
             waers TYPE waers,
           END OF ty_alv,
           tt_alv TYPE STANDARD TABLE OF ty_alv.

    DATA: mt_alv  TYPE tt_alv,
          mt_fcat TYPE slis_t_fieldcat_alv,
          mt_sort TYPE slis_t_sortinfo_alv.

    METHODS: get_fcat  ,
      display_alv,
      get_data.

ENDCLASS.

CLASS lcl_alv IMPLEMENTATION.

  METHOD execute.
    get_data( ).
    get_fcat( ).
    display_alv( ).

  ENDMETHOD.

  METHOD get_data.

    SELECT ekko~ebeln,
           ekpo~ebelp,
           ekpo~txz01,
           ekpo~netpr,
           ekko~waers
      FROM ekko
      JOIN ekpo
      ON ekko~ebeln = ekpo~ebeln
     INTO TABLE @mt_alv
      WHERE ekko~ebeln IN @s_ebeln.

  ENDMETHOD.

  METHOD get_fcat.

    APPEND VALUE #( fieldname = 'EBELN'
                    up        = 'X'
                    subtot    = 'X' ) TO mt_sort.

    mt_fcat = VALUE #( tabname = 'mt_alv'
                      ( fieldname = 'EBELN'   seltext_m = 'Purchasing Document'  )
                      ( fieldname = 'EBELP'   seltext_m = 'Item'  )
                      ( fieldname = 'TXZ01'   seltext_m = 'Short Text'    )
                      ( fieldname = 'NETPR'   seltext_m = 'Net Price'  do_sum = abap_true  cfieldname = 'WAERS' )
                      ( fieldname = 'WAERS'   seltext_m = 'Currency'  ) ).

  ENDMETHOD.

  METHOD display_alv.

    DATA(ls_layout) = VALUE slis_layout_alv( colwidth_optimize = abap_true
                                         box_tabname       = 'mt_alv' ).

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program = sy-repid
        is_layout          = ls_layout
        it_fieldcat        = mt_fcat
        it_sort            = mt_sort
      TABLES
        t_outtab           = mt_alv
      EXCEPTIONS
        program_error      = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA(go_alv) = NEW lcl_alv( ).
  go_alv->execute( ).
