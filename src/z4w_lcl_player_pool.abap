*&---------------------------------------------------------------------*
*&  Include           Z4W_LCL_PLAYER_POOL
*&---------------------------------------------------------------------*
CLASS lcl_player_pool DEFINITION.
  PUBLIC SECTION.
    METHODS:
      "! Add a new player to the pool.
      "! @parameter io_player |
      "! @parameter ro_ref |
      add
        IMPORTING
                  io_player     TYPE REF TO lcl_player
        RETURNING VALUE(ro_ref) TYPE REF TO lcl_player_pool,

      "! Get the current player.
      "! @parameter ro_current |
      "! @raising cx_sy_itab_line_not_found |
      get
        RETURNING VALUE(ro_current) TYPE REF TO lcl_player
        RAISING   cx_sy_itab_line_not_found,

      "! Move to the next player.
      "! @parameter ro_next |
      next
        RETURNING VALUE(ro_next) TYPE REF TO lcl_player,

      "! Tells human player to make a pick.
      human_pick.

    DATA:
      " There should only be one human reference
      o_human   TYPE REF TO lcl_human,
      " All players will be saved here
      i_objects TYPE STANDARD TABLE OF REF TO lcl_player READ-ONLY,
      w_index   TYPE i VALUE 1.

  PRIVATE SECTION.
    TYPES:
        t_colors TYPE SORTED TABLE OF i WITH UNIQUE KEY TABLE_LINE.

    METHODS:
        "! Make sure that every player has a different color
        check_distinct_colors.

ENDCLASS.

CLASS lcl_player_pool IMPLEMENTATION.

  METHOD add.

    " We only have 7 different colors
    ASSERT lines( i_objects ) <= 7.

    APPEND io_player TO i_objects.

    TRY.
        IF o_human IS NOT BOUND.
          o_human = CAST #( io_player ).
        ENDIF.
      CATCH cx_sy_move_cast_error.
        ASSERT 1 = 2. "TODO: Insert proper exception here
    ENDTRY.

    check_distinct_colors( ).

    ro_ref = me.

  ENDMETHOD.

  METHOD get.

    ro_current = i_objects[ w_index ].

  ENDMETHOD.

  METHOD next.

    w_index = COND #(
      WHEN w_index = lines( i_objects )
        THEN 1
      ELSE w_index + 1
    ).

    ro_next = i_objects[ w_index ].

  ENDMETHOD.

  METHOD human_pick.
    ASSERT o_human IS BOUND.
    o_human->selection( ).
  ENDMETHOD.

  METHOD check_distinct_colors.

    DATA li_colors TYPE t_colors.

    LOOP AT i_objects ASSIGNING FIELD-SYMBOL(<fs_player>).

        INSERT <fs_player>->w_color INTO TABLE li_colors.

        " If color is already taken
        IF sy-subrc <> 0.

            DATA(lw_idx) = 0.
            DO lines( li_colors ) TIMES.
                lw_idx = lw_idx + 1.
                ASSIGN li_colors[ lw_idx ] TO FIELD-SYMBOL(<fs_color>).
                IF <fs_color> <> lw_idx.
                    <fs_player>->set_color( lw_idx ).
                    EXIT.
                ENDIF.
            ENDDO.

        ENDIF.

    ENDLOOP.


  ENDMETHOD.

ENDCLASS.