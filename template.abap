SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

SELECTION-SCREEN END OF BLOCK b1.


*----------------------------------------------------------------------*
*       CLASS lcl_%Class name% DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_%Class name% DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS execute.

  PRIVATE SECTION.
    METHODS extract_data.
ENDCLASS.                    "

DATA go_%Class name% TYPE REF TO lcl_%Class name%.

*----------------------------------------------------------------------*
*       CLASS lcl_%Class name% IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_%Class name% IMPLEMENTATION.

  METHOD execute.
    extract_data( ).

  ENDMETHOD.                    "execute

  METHOD extract_data.

  ENDMETHOD.                    "extract_data

ENDCLASS.    


START-OF-SELECTION.
  CREATE OBJECT go_%Class name%.
  go_%Class name%->execute( ).
