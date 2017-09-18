*&---------------------------------------------------------------------*
*&  Include           Z4W_LCL_CLEVER_BOT
*&---------------------------------------------------------------------*
CLASS lcl_clever_bot DEFINITION
  INHERITING FROM lcl_bot.
  PUBLIC SECTION.
    METHODS:
      constructor,
      pick REDEFINITION.

ENDCLASS.

CLASS lcl_clever_bot IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    me->set_name( `Clever bot` ).
  ENDMETHOD.

  METHOD pick.

    ASSERT o_gamefield IS BOUND.

    DATA(lw_col) = 1.
    DATA(li_chances) = o_gamefield->get_chances( me ).

    ASSIGN li_chances[ 1 ] TO FIELD-SYMBOL(<fs_chance>).
    IF sy-subrc = 0.

      lw_col = <fs_chance>-x.

    ELSE.

      " TODO: Use predicate to filter everything except me or initial
      DATA(li_human_chances) = o_gamefield->get_chances( o_gamefield->o_player_pool->o_human ).

      ASSIGN li_human_chances[ 1 ] TO <fs_chance>.
      IF sy-subrc = 0.

        lw_col = <fs_chance>-x.

      ELSE.

        lw_col = lcl_utils=>get_random_number( iw_to = lcl_utils=>c_field_width ).

      ENDIF.

    ENDIF.

    RAISE EVENT picked
      EXPORTING
        iw_column = lw_col.

  ENDMETHOD.

ENDCLASS.