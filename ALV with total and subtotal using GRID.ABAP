REPORT zkd_alv_tot_grid.

DATA: lv_ebeln TYPE ebeln.
SELECT-OPTIONS: s_ebeln FOR lv_ebeln.

CLASS lcl_alv DEFINITION FINAL.

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

    DATA: mt_alv       TYPE tt_alv,
          mo_alv_grid  TYPE REF TO cl_gui_alv_grid,         " declare an object
          mo_cust_grid TYPE REF TO cl_gui_custom_container, " declare an object
          mt_gridfcat  TYPE lvc_t_fcat,
          mt_sort      TYPE lvc_t_sort.

    METHODS: get_data,
      container_grid,
      set_first_display,
      get_fcat_grid,
      get_layout RETURNING VALUE(rs_layout) TYPE lvc_s_layo.

ENDCLASS.

" screen for alv grid
DATA: BEGIN OF gs_screen100,
        ok_code TYPE syucomm,
      END OF gs_screen100.

CLASS lcl_alv IMPLEMENTATION.

  METHOD execute.
    get_data( ).
    set_first_display( ).
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

  METHOD get_fcat_grid.

    APPEND VALUE #( fieldname = 'EBELN'
                    up        = 'X'
                    subtot    = 'X' ) TO mt_sort.

    mt_gridfcat = VALUE #( tabname = 'mt_alv'
                  ( fieldname = 'EBELN'   reptext = 'Purchasing Document'  )
                  ( fieldname = 'EBELP'   reptext = 'Item'  )
                  ( fieldname = 'TXZ01'   reptext = 'Short Text'    )
                  ( fieldname = 'NETPR'   reptext = 'Net Price' do_sum = abap_true  cfieldname = 'WAERS' )
                  ( fieldname = 'WAERS'   reptext = 'Currency'  ) ).

  ENDMETHOD.

  METHOD container_grid.

    mo_cust_grid = NEW cl_gui_custom_container( container_name = 'CONT' ).
    mo_alv_grid = NEW cl_gui_alv_grid( i_parent = mo_cust_grid ).
  ENDMETHOD.

  METHOD set_first_display.

    container_grid( ).
    get_fcat_grid( ).

    DATA(ls_layout) = get_layout( ).

    TRY.
        mo_alv_grid->set_table_for_first_display( EXPORTING is_layout       = ls_layout
                                                  CHANGING  it_outtab       = mt_alv
                                                            it_sort = mt_sort
                                                            it_fieldcatalog = mt_gridfcat ).
        CALL SCREEN 100.
      CATCH cx_root INTO DATA(lx_root).
        MESSAGE lx_root TYPE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD get_layout.
    " Set the layout options for the ALV grid
    rs_layout = VALUE #( cwidth_opt = abap_true ).
  ENDMETHOD.

ENDCLASS.

MODULE status_0100 OUTPUT.
  SET PF-STATUS 'ZKD_STATUS'.
*  SET TITLEBAR 'TITLE_0100'.
ENDMODULE.
MODULE user_command_0100 INPUT.
  CASE gs_screen100-ok_code.
    WHEN 'FC_BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

START-OF-SELECTION.
  DATA(go_alv) = NEW lcl_alv( ).
  go_alv->execute( ).
