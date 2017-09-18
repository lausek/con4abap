*&---------------------------------------------------------------------*
*&  Include           Z4W_LCL_BOTS
*&---------------------------------------------------------------------*
" Basic bot. Places boxes randomly
CLASS lcl_bot DEFINITION
  INHERITING FROM lcl_player.
  PUBLIC SECTION.
    METHODS:
      constructor,
      pick REDEFINITION.

ENDCLASS.

CLASS lcl_bot IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    me->set_name( `Very dumb bot` ).
    me->set_color( 4 ).
  ENDMETHOD.

  METHOD pick.

    " TODO: Add random call here
    DATA(lw_col) = lcl_utils=>get_random_number( iw_to = lcl_utils=>c_field_width ).

    RAISE EVENT picked
      EXPORTING
        iw_column = lw_col.

  ENDMETHOD.

ENDCLASS.

" Places boxes in straight line; restarts on column 1
CLASS lcl_straight_bot DEFINITION
  INHERITING FROM lcl_bot.
  PUBLIC SECTION.
    METHODS:
      constructor,
      pick REDEFINITION.

  PRIVATE SECTION.
    DATA:
      w_col TYPE i VALUE 1.

ENDCLASS.

CLASS lcl_straight_bot IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    me->set_name( `Straight placer` ).
  ENDMETHOD.

  METHOD pick.

    RAISE EVENT picked
      EXPORTING
        iw_column = w_col.

    w_col = w_col + 1.

    IF w_col > 16.
      w_col = 1.
    ENDIF.

  ENDMETHOD.

ENDCLASS.