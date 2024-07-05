REPORT zkd_alv_tot_salv.

DATA : lv_ebeln TYPE ebeln .
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
          lo_salv TYPE REF TO cl_salv_table.

    METHODS:  display_alv,
      get_data.

ENDCLASS.

CLASS lcl_alv IMPLEMENTATION.

  METHOD execute.
    get_data( ).
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
      WHERE ekko~ebeln BETWEEN 3000000009 AND 4500012169.  "@s_ebeln.

  ENDMETHOD.

  METHOD display_alv.

    DATA: lo_column TYPE REF TO cl_salv_column.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lo_salv
          CHANGING
            t_table      = mt_alv ).

        DATA(lo_layout) = lo_salv->get_layout( ).
        lo_salv->get_columns( )->set_optimize( ).
        lo_salv->get_functions( )->set_all( abap_true ).

        DATA(lo_columns) = lo_salv->get_columns( ).
        lo_column ?= lo_columns->get_column( 'NETPR' ).
      lo_column->set_currency_column( 'WAERS' ).

       DATA(lo_aggregations) = lo_salv->get_aggregations( ).
        lo_aggregations->add_aggregation(
          EXPORTING
            columnname  = 'NETPR'
            aggregation = if_salv_c_aggregation=>total ).

        DATA(lr_sort) = lo_salv->get_sorts( ).
        DATA(lr_sort_column) = lr_sort->add_sort(
         EXPORTING
            columnname = 'EBELN' ).

        lr_sort_column->set_subtotal(
          EXPORTING
            value = if_salv_c_bool_sap=>true ).

      CATCH cx_salv_msg
            cx_salv_not_found
            cx_salv_existing.
    ENDTRY.

    lo_salv->display( ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  DATA(go_alv) = NEW lcl_alv( ).
  go_alv->execute( ).
