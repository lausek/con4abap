*&---------------------------------------------------------------------*
*&  Include           Z4W_LCL_PLAYER
*&---------------------------------------------------------------------*
CLASS lcl_gamefield DEFINITION DEFERRED.

CLASS lcl_player DEFINITION.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF t_chance,
        count  TYPE i,
        player TYPE REF TO lcl_player,
        x      TYPE i,
        y      TYPE i,
      END OF t_chance,

      t_chances TYPE SORTED TABLE OF t_chance WITH NON-UNIQUE KEY count.

    EVENTS:
      "! Gamefield subscribes to this at player creation.
      "! Will be called when player confirms a move.
      "! @parameter iw_column |
      picked
        EXPORTING
          VALUE(iw_column) TYPE i.

    DATA:
      w_color TYPE i     READ-ONLY,
      w_name  TYPE name1 READ-ONLY.

    METHODS:
      "! Tell the player, that it is his turn now.
      "! @parameter rw_col |
      pick
        RETURNING VALUE(rw_col) TYPE i,

      "! Set the players name. If iw_name is initial, no changes will be made.
      "! @parameter iw_name |
      "! @parameter ro_ref |
      set_name
        IMPORTING
                  iw_name       TYPE simple
        RETURNING VALUE(ro_ref) TYPE REF TO lcl_player,

      "! Set the players box color. If iw_color is not between 1 and 7, no changes will be made.
      "! @parameter iw_color |
      "! @parameter ro_ref |
      set_color
        IMPORTING
                  iw_color      TYPE i
        RETURNING VALUE(ro_ref) TYPE REF TO lcl_player,

      "! Inform the player about his playfield. Required by bots for analysing their moves.
      "! @parameter io_field |
      "! @parameter ro_ref |
      set_field
        IMPORTING
                  io_field      TYPE REF TO lcl_gamefield
        RETURNING VALUE(ro_ref) TYPE REF TO lcl_player.

  PROTECTED SECTION.
    DATA:
      o_gamefield TYPE REF TO lcl_gamefield.

ENDCLASS.

CLASS lcl_player IMPLEMENTATION.

  METHOD pick.
  ENDMETHOD.

  METHOD set_name.
    IF iw_name IS NOT INITIAL.
      me->w_name = iw_name.
    ENDIF.

    ro_ref = me.
  ENDMETHOD.

  METHOD set_color.
    IF 1 <= iw_color AND iw_color <= 7.
      me->w_color = iw_color.
    ENDIF.

    ro_ref = me.
  ENDMETHOD.

  METHOD set_field.
    o_gamefield = io_field.

    ro_ref = me.
  ENDMETHOD.

ENDCLASS.