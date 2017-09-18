*&---------------------------------------------------------------------*
*&  Include           Z4W_LCL_GAMEFIELD
*&---------------------------------------------------------------------*
CLASS lcl_gamefield DEFINITION.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF t_coord,
        x TYPE i,
        y TYPE i,
      END OF t_coord,

      BEGIN OF t_mvector,
        x  TYPE i,
        y  TYPE i,
        sx TYPE i,
        sy TYPE i,
      END OF t_mvector,

      t_cell  TYPE REF TO lcl_player,
      t_line  TYPE STANDARD TABLE OF t_cell WITH EMPTY KEY,
      t_field TYPE STANDARD TABLE OF t_line WITH EMPTY KEY.

    DATA:
      o_player_pool    TYPE REF TO lcl_player_pool READ-ONLY,
      " TODO: Make this read-only
      o_current_player TYPE REF TO lcl_player,
      " TODO: Make this read-only
      i_field          TYPE t_field.

    METHODS:
      "! Constructor will receive a pool of players.
      "! @parameter io_players |
      constructor
        IMPORTING
          io_players TYPE REF TO lcl_player_pool,

      "! Add a box for io_player which position is specified under iw_column.
      "! @parameter iw_column |
      "! @parameter io_player |
      "! @parameter rw_success |
      insert_on_column
        IMPORTING
                  iw_column         TYPE i
                  io_player         TYPE REF TO lcl_player
        RETURNING VALUE(rw_success) TYPE abap_bool,

      "! Check if the game has any other reasons to end.
      "! Win checking is not done in here.
      "! @parameter rw_over |
      game_over
        RETURNING VALUE(rw_over) TYPE abap_bool,

      "! Used by bots to analyse where a player is close to a full row.
      "! @parameter io_player |
      "! @parameter ri_positions |
      get_chances
        IMPORTING
                  io_player           TYPE REF TO lcl_player
        RETURNING VALUE(ri_positions) TYPE lcl_player=>t_chances,

      "! Change turn to next player in player pool.
      next_player,

      "! Redraw screen and check if player has a full row now.
      "! Check for 'game_over' is also done here.
      update,

      "! Draw game toolbar, playfield and player information.
      redraw,

      "! Selection bar is the place where the user can pick which column
      "! he wants to fill next.
      "! Called in 'redraw'
      "! @parameter iw_width |
      draw_selection_bar
        IMPORTING
          iw_width TYPE i OPTIONAL,

      "! Display the current players from pool and their color.
      "! @parameter iw_width |
      draw_player_info
        IMPORTING
          iw_width TYPE i OPTIONAL,

      "! Event handler for 'picked' event of players.
      "! @parameter iw_column |
      "! @parameter sender |
      handle_pick FOR EVENT picked OF lcl_player
        IMPORTING
            iw_column
            sender,

      "! Checks if a player has won the game and returns true if so.
      "! Called in 'update'.
      "! @parameter io_player |
      "! @parameter rw_won |
      check_for_player
        IMPORTING
                  io_player     TYPE REF TO lcl_player
        RETURNING VALUE(rw_won) TYPE abap_bool.

  PRIVATE SECTION.
    METHODS:
      "! Invokes 'expand_on' and tests if its result is bigger than 3.
      "! @parameter iwa_base |
      "! @parameter iw_movex |
      "! @parameter iw_movey |
      "! @parameter rw_full |
      check_vector
        IMPORTING
                  iwa_base       TYPE t_mvector
                  iw_movex       TYPE i
                  iw_movey       TYPE i
        RETURNING VALUE(rw_full) TYPE abap_bool,

      "! Recursively check how long a streak of equal boxes continues.
      "! iwa_vector contains the original position plus a direction for x and y axis.
      "! The direction will be added on each new call and so create another vector.
      "!
      "! iw_depth increments on each deeper level and will be returned if the end of the streak was reached
      "! or depth is equal to 5.
      "!
      "! io_eqto is used for explicitly specifying the reference which should be equal. If not supplied, this
      "! will be initialized on first call.
      "! @parameter iwa_vector |
      "! @parameter iw_depth |
      "! @parameter io_eqto |
      "! @parameter rw_depth |
      expand_on
        IMPORTING
                  iwa_vector      TYPE t_mvector
                  iw_depth        TYPE i DEFAULT 1
                  io_eqto         TYPE REF TO lcl_player OPTIONAL
        RETURNING VALUE(rw_depth) TYPE i.

ENDCLASS.

CLASS lcl_gamefield IMPLEMENTATION.

  METHOD constructor.

    me->o_player_pool = io_players.

    TRY.
        me->o_current_player = io_players->get( ).
      CATCH cx_sy_itab_line_not_found.
        " No players in pool yet.
    ENDTRY.

    DO lcl_utils=>c_field_height TIMES.
      APPEND INITIAL LINE TO i_field ASSIGNING FIELD-SYMBOL(<fs_line>).
      DO lcl_utils=>c_field_width TIMES.
        APPEND INITIAL LINE TO <fs_line>.
      ENDDO.
    ENDDO.

  ENDMETHOD.

  METHOD get_chances.

    DEFINE check_line.
      lw_depth = expand_on(
        iwa_vector = VALUE #(
          x  = lw_col_idx + &1
          y  = lw_row_idx + &2
          sx = &1
          sy = &2
        )
        io_eqto = io_player
      )
      + expand_on(
        iwa_vector = VALUE #(
          x  = lw_col_idx + &3
          y  = lw_row_idx + &4
          sx = &3
          sy = &4
        )
        io_eqto = io_player
      ).
      IF 3 <= lw_depth.
        INSERT VALUE #(
          count     = lw_depth
          player    = io_player
          x         = lw_col_idx
          y         = lw_row_idx
        ) INTO TABLE ri_positions.
      ENDIF.
    END-OF-DEFINITION.

    DATA(lw_col_idx) = 1.
    DATA(lw_depth) = 0.

    WHILE lw_col_idx <= lcl_utils=>c_field_width.

      DATA(lw_row_idx) = 1.

      WHILE lw_row_idx <= lcl_utils=>c_field_height.

        IF ( lw_row_idx = lcl_utils=>c_field_height )
        OR ( lw_row_idx < lcl_utils=>c_field_height AND i_field[ lw_row_idx + 1 ][ lw_col_idx ] IS BOUND ).
          ASSIGN i_field[ lw_row_idx ][ lw_col_idx ] TO FIELD-SYMBOL(<fs_cell>).
        ENDIF.

        IF <fs_cell> IS ASSIGNED.

*          (  1,  0 ) + ( -1, 0 ) -> 4 <= depth; horizontal
          check_line 1 0 -1 0.

*          ( -1, -1 ) + ( 1,  1 ) -> 4 <= depth; diagonal-forward
          check_line -1 -1 1 1.

*          (  1, -1 ) + ( -1, 1 ) -> 4 <= depth; diagonal-backward
          check_line 1 -1 -1 1.

*          (  0,  1 )             -> 4 <= depth; down (no other direction here)
          lw_depth = expand_on(
            iwa_vector = VALUE #(
              x  = lw_col_idx
              " TODO: If the whole iw_column is empty, we don't need to subtract
              y  = lw_row_idx + COND #( WHEN lw_row_idx = lcl_utils=>c_field_height THEN 1 )
              sx = 0
              sy = 1
            )
            io_eqto = io_player
          ).
          IF 3 <= lw_depth.
            INSERT VALUE #(
              count   = lw_depth
              player  = io_player
              x       = lw_col_idx
              y       = lw_row_idx
            ) INTO TABLE ri_positions.
          ENDIF.

          " If the upper block was already found, we can break on this column
          UNASSIGN <fs_cell>.
          EXIT.

        ENDIF.

        lw_row_idx = lw_row_idx + 1.

      ENDWHILE.

      lw_col_idx = lw_col_idx + 1.

    ENDWHILE.

  ENDMETHOD.

  METHOD next_player.

    o_current_player = o_player_pool->next( ).

    o_current_player->pick( ).

  ENDMETHOD.

  METHOD insert_on_column.

    DATA(lw_idx) = lines( i_field ).

    IF iw_column < 1 OR lcl_utils=>c_field_width < iw_column.
      rw_success = abap_false.
      RETURN.
    ENDIF.

    DO.
      ASSIGN i_field[ lw_idx ] TO FIELD-SYMBOL(<fs_line>).
      IF sy-subrc <> 0.
*        No space for another cell
        rw_success = abap_false.
        RETURN.
      ENDIF.

      ASSIGN <fs_line>[ iw_column ] TO FIELD-SYMBOL(<fs_cell>).

      IF <fs_cell> IS NOT BOUND.
        <fs_cell> = io_player.
        rw_success = abap_true.
        RETURN.
      ENDIF.

      lw_idx = lw_idx - 1.
    ENDDO.

  ENDMETHOD.

  METHOD handle_pick.

    " TODO: check that iw_column is allowed
    IF insert_on_column(
        io_player = sender
        iw_column = iw_column
      ).
      update( ).
      next_player( ).
    ELSE.
      MESSAGE 'Column is full. Please choose another one.' TYPE 'S'.
      o_current_player->pick( ).
    ENDIF.

  ENDMETHOD.

  METHOD game_over.
    " TODO: Check if there is any space left in the gamefield
    rw_over = abap_true.
    LOOP AT i_field[ 1 ] ASSIGNING FIELD-SYMBOL(<fs_ref>).
      IF <fs_ref> IS NOT BOUND.
        rw_over = abap_false.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD update.

    me->redraw( ).

    " Has the player filled a row?
    IF check_for_player( o_current_player ).

      MESSAGE |{ o_current_player->w_name } wins!| TYPE 'I'.
      LEAVE LIST-PROCESSING.

    ELSEIF game_over( ).

      MESSAGE 'Draw!' TYPE 'I'.
      LEAVE LIST-PROCESSING.

    ENDIF.

  ENDMETHOD.

  METHOD expand_on.

    ASSIGN i_field[ iwa_vector-y ][ iwa_vector-x ] TO FIELD-SYMBOL(<fs_cell>).
    rw_depth = COND #(
      LET
        equal_to = COND #(
            WHEN io_eqto       IS SUPPLIED
             AND io_eqto       IS BOUND THEN io_eqto
            WHEN <fs_cell> IS ASSIGNED THEN <fs_cell>
        )
        temp_vec = VALUE t_mvector(
          BASE iwa_vector
          x = iwa_vector-x + iwa_vector-sx
          y = iwa_vector-y + iwa_vector-sy
        )
      IN
      " If the minimum row length limit should be increased - probably better to remove this
      WHEN iw_depth >= 5
        THEN iw_depth
      WHEN <fs_cell> IS NOT ASSIGNED
        OR <fs_cell> <> equal_to
          THEN iw_depth - 1
      ELSE expand_on(
        EXPORTING
          iw_depth   = iw_depth + 1
          io_eqto    = equal_to
          iwa_vector  = temp_vec
      )
    ).

  ENDMETHOD.

  METHOD check_vector.

    rw_full = boolc( 3 < expand_on(
        iwa_vector = VALUE #(
          BASE iwa_base
          sx  = iw_movex
          sy  = iw_movey
        )
      )
    ).

  ENDMETHOD.

  METHOD check_for_player.

    DATA(lw_row_idx) = lcl_utils=>c_field_height.

    WHILE lw_row_idx > 0.

      DATA(lw_col_idx) = 1.

      WHILE lw_col_idx <= lcl_utils=>c_field_width.

        IF i_field[ lw_row_idx ][ lw_col_idx ] = o_current_player.

          DATA(lwa_vector) = VALUE t_mvector(
            x = lw_col_idx
            y = lw_row_idx
          ).

*          expand_on( x = 0 y = -1 ) -> check cells above current
*          if 4 iterations are accomplished -> done

          rw_won = check_vector(
            iwa_base = lwa_vector
            iw_movex = -1
            iw_movey = -1
          ).
          IF rw_won = abap_true.
            RETURN.
          ENDIF.

          rw_won = check_vector(
            iwa_base = lwa_vector
            iw_movex = 0
            iw_movey = -1
          ).
          IF rw_won = abap_true.
            RETURN.
          ENDIF.

          rw_won = check_vector(
            iwa_base = lwa_vector
            iw_movex = 1
            iw_movey = -1
          ).
          IF rw_won = abap_true.
            RETURN.
          ENDIF.

          rw_won = check_vector(
            iwa_base = lwa_vector
            iw_movex = 1
            iw_movey = 0
          ).
          IF rw_won = abap_true.
            RETURN.
          ENDIF.

        ENDIF.

        lw_col_idx = lw_col_idx + 1.

      ENDWHILE.

      lw_row_idx = lw_row_idx - 1.

    ENDWHILE.

    rw_won = abap_false.

  ENDMETHOD.

  METHOD draw_selection_bar.

    FORMAT COLOR COL_TOTAL.

    IF iw_width IS SUPPLIED.
      ULINE AT (iw_width). NEW-LINE.
    ENDIF.

    DO lcl_utils=>c_field_width TIMES.
      WRITE '| VV ' HOTSPOT = 1 NO-GAP.
    ENDDO.
    WRITE '|'. NEW-LINE.

    IF iw_width IS SUPPLIED.
      ULINE AT (iw_width). NEW-LINE.
    ENDIF.

    FORMAT COLOR COL_BACKGROUND.

  ENDMETHOD.

  METHOD draw_player_info.

    NEW-LINE.

    IF iw_width IS SUPPLIED.
      ULINE AT (iw_width). NEW-LINE.
    ENDIF.

    LOOP AT o_player_pool->i_objects ASSIGNING FIELD-SYMBOL(<fs_player>).

      WRITE: '|', (3) space COLOR = <fs_player>->w_color, (30) <fs_player>->w_name, AT iw_width '|'.
      NEW-LINE.

    ENDLOOP.

    IF iw_width IS SUPPLIED.
      ULINE AT (iw_width). NEW-LINE.
    ENDIF.

    FORMAT COLOR COL_BACKGROUND.

  ENDMETHOD.

  METHOD redraw.

    sy-lsind = 0.

    sy-colno = 1.
    sy-linno = 1.

    DATA(lw_field_width) = lcl_utils=>c_field_width * ( lcl_utils=>c_box_width + 1 ) + 1.

    draw_selection_bar( iw_width = lw_field_width ).

    LOOP AT i_field ASSIGNING FIELD-SYMBOL(<fs_line>).

*      make each box 'size' high
      DO lcl_utils=>c_box_height TIMES.

        DATA(lw_line_index) = 1.

        DO.

          ASSIGN <fs_line>[ lw_line_index ] TO FIELD-SYMBOL(<fs_cell>).
          IF sy-subrc <> 0.
            EXIT.
          ENDIF.

          WRITE '|' NO-GAP.

          IF <fs_cell> IS BOUND.
            FORMAT COLOR = <fs_cell>->w_color.
          ELSE.
            FORMAT COLOR = 0.
          ENDIF.

*          make each box 'size' wide
          DO lcl_utils=>c_box_width TIMES.

            WRITE space NO-GAP.

          ENDDO.

          lw_line_index = lw_line_index + 1.

        ENDDO.

        WRITE '|' NO-GAP. NEW-LINE.

      ENDDO.

    ENDLOOP.

    ULINE AT (lw_field_width).

    WRITE /.

    FORMAT COLOR = 0.

    draw_player_info( iw_width = lw_field_width ).

  ENDMETHOD.

ENDCLASS.