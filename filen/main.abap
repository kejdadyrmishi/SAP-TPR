INCLUDE zkd_final_top.

START-OF-SELECTION.
  DATA go_timesheet TYPE REF TO lcl_timesheet.

  go_timesheet = NEW #( ).
  go_timesheet->execute( ).


  INCLUDE zkd_final_implementation.
  INCLUDE zkd_final_forms.
