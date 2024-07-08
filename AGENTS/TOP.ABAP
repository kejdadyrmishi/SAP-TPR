
*&---------------------------------------------------------------------*
*&  Include           ZKD_FINAL_EXAM_TOP
*&---------------------------------------------------------------------*

PROGRAM zkd_final_exam.
"Type per tabelen
TYPES:BEGIN OF ty_sale,
        sel(1)    TYPE c,
        cod       TYPE zkd_employee-cod,
        firstname TYPE zkd_employee-firstname,
        lastname  TYPE zkd_employee-lastname,
        netwr     TYPE zkd_shitjet-netwr,
        descr     TYPE zkd_shitjet-descr,
        bonus     TYPE zkd_shitjet-netwr,
        counter   TYPE zkd_shitjet-counter,
      END OF ty_sale.

*screen 1
DATA:BEGIN OF gs_screen001,
       month   TYPE zkd_shitjet-smonth,
       year    TYPE zkd_shitjet-syear,
       ok_code TYPE sy-ucomm,
     END OF gs_screen001,

     BEGIN OF gs_screen002,
       netwr   TYPE zkd_shitjet-netwr,
       waers   TYPE zkd_shitjet-waers,
       ok_code TYPE sy-ucomm,
     END OF gs_screen002.

*screen 3
DATA: BEGIN OF gs_screen003,
        cod       TYPE zkd_employee-cod,
        firstname TYPE zkd_employee-firstname,
        lastname  TYPE zkd_employee-lastname,
        cnp       TYPE zkd_employee-cnp,
        hire_date TYPE zkd_employee-hire_date,
        ok_code   TYPE sy-ucomm,
      END OF gs_screen003,

*screen 4
      BEGIN OF gs_screen004,
        cod     TYPE zkd_shitjet-cod,
        netwr   TYPE zkd_shitjet-netwr,
        descr   TYPE zkd_shitjet-descr,
        ok_code TYPE sy-ucomm,
      END OF gs_screen004.

*struc and table for table type
DATA: gs_sale TYPE ty_sale,
      gt_sale TYPE STANDARD TABLE OF ty_sale.

*&SPWIZARD: DECLARATION OF TABLECONTROL 'GS_TAB_CONT' ITSELF
CONTROLS: GS_TAB_CONT TYPE TABLEVIEW USING SCREEN 0002.

*&SPWIZARD: LINES OF TABLECONTROL 'GS_TAB_CONT'
*DATA:     G_GS_TAB_CONT_LINES  LIKE SY-LOOPC.
