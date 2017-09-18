*&---------------------------------------------------------------------*
*&  Include           Z4W_LCL_HUMAN
*&---------------------------------------------------------------------*
CLASS lcl_human DEFINITION
  INHERITING FROM lcl_player.
  PUBLIC SECTION.
    METHODS:
      constructor,

      "! User clicked on column. Invoke on screen event 'AT LINE-SELECTION'.
      selection.

ENDCLASS.

CLASS lcl_human IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    me->set_name( `Basic human` ).
    me->set_color( 5 ).
  ENDMETHOD.

  METHOD selection.
    CASE sy-ucomm.
      WHEN 'PICK'.

        IF 1 < sy-curow AND sy-curow < 3.
          RAISE EVENT picked
           EXPORTING
             iw_column = CONV i( sy-cucol / '5.0' ).
        ENDIF.

    ENDCASE.
  ENDMETHOD.

ENDCLASS.