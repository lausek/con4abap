*&---------------------------------------------------------------------*
*&  Include  z4w_lcl_test_class
*&---------------------------------------------------------------------*
CLASS lcl_test_class DEFINITION
    FOR TESTING
    RISK LEVEL HARMLESS
    DURATION SHORT.
  PUBLIC SECTION.
    CLASS-METHODS:
      class_constructor.

  PRIVATE SECTION.
    CLASS-DATA:
      o_player TYPE REF TO lcl_player,
      o_bot    TYPE REF TO lcl_player,
      o_iter   TYPE REF TO lcl_player_pool.

    METHODS:
      check_vertical FOR TESTING,
      check_horizontal FOR TESTING,
      check_diagonal_forward FOR TESTING,
      check_diagonal_backward FOR TESTING,
      check_color_assignment FOR TESTING.

ENDCLASS.

CLASS lcl_test_class IMPLEMENTATION.

  METHOD class_constructor.
    o_player    = NEW lcl_human( ).
    o_bot       = NEW lcl_clever_bot( ).
    o_iter      = NEW lcl_player_pool( )->add( o_player )->add( o_bot ).
  ENDMETHOD.

  METHOD check_vertical.

    DATA(lo_game) = NEW lcl_gamefield( o_iter ).
    lo_game->o_current_player = o_player.
    lo_game->i_field[ 6 ][ 6 ] = o_player.
    lo_game->i_field[ 5 ][ 6 ] = o_player.
    lo_game->i_field[ 4 ][ 6 ] = o_player.
    lo_game->i_field[ 3 ][ 6 ] = o_player.

    cl_aunit_assert=>assert_equals(
        act = lo_game->check_for_player( o_player )
        exp = abap_true
    ).

    lo_game->i_field[ 5 ][ 6 ] = o_bot.

    cl_aunit_assert=>assert_equals(
        act = lo_game->check_for_player( o_player )
        exp = abap_false
    ).

  ENDMETHOD.

  METHOD check_horizontal.

    DATA(lo_game) = NEW lcl_gamefield( o_iter ).
    lo_game->o_current_player = o_player.
    lo_game->i_field[ 6 ][ 6 ] = o_player.
    lo_game->i_field[ 6 ][ 5 ] = o_player.
    lo_game->i_field[ 6 ][ 4 ] = o_player.
    lo_game->i_field[ 6 ][ 3 ] = o_player.

    cl_aunit_assert=>assert_equals(
        act = lo_game->check_for_player( o_player )
        exp = abap_true
    ).

    lo_game->i_field[ 6 ][ 5 ] = o_bot.

    cl_aunit_assert=>assert_equals(
        act = lo_game->check_for_player( o_player )
        exp = abap_false
    ).

  ENDMETHOD.

  METHOD check_diagonal_forward.

    DATA(lo_game) = NEW lcl_gamefield( o_iter ).
    lo_game->o_current_player = o_player.
    lo_game->i_field[ 8 ][ 8 ] = o_player.
    lo_game->i_field[ 7 ][ 9 ] = o_player.
    lo_game->i_field[ 6 ][ 10 ] = o_player.
    lo_game->i_field[ 5 ][ 11 ] = o_player.

    cl_aunit_assert=>assert_equals(
        act = lo_game->check_for_player( o_player )
        exp = abap_true
    ).

    lo_game->i_field[ 6 ][ 10 ] = o_bot.

    cl_aunit_assert=>assert_equals(
        act = lo_game->check_for_player( o_player )
        exp = abap_false
    ).

  ENDMETHOD.

  METHOD check_diagonal_backward.

    DATA(lo_game) = NEW lcl_gamefield( o_iter ).
    lo_game->o_current_player = o_player.
    lo_game->i_field[ 8 ][ 8 ] = o_player.
    lo_game->i_field[ 7 ][ 7 ] = o_player.
    lo_game->i_field[ 6 ][ 6 ] = o_player.
    lo_game->i_field[ 5 ][ 5 ] = o_player.

    cl_aunit_assert=>assert_equals(
        act = lo_game->check_for_player( o_player )
        exp = abap_true
    ).

    lo_game->i_field[ 6 ][ 6 ] = o_bot.

    cl_aunit_assert=>assert_equals(
        act = lo_game->check_for_player( o_player )
        exp = abap_false
    ).

  ENDMETHOD.

  METHOD check_color_assignment.

    DATA(lo_p1) = NEW lcl_human( ).
    DATA(lo_p2) = NEW lcl_bot( ).

    DATA(lo_pool) = NEW lcl_player_pool( ).
    lo_pool->add( lo_p1 )->add( lo_p2 ).

    cl_aunit_assert=>assert_equals(
        act = lo_p1->w_color
        exp = 5
    ).

    cl_aunit_assert=>assert_equals(
        act = lo_p2->w_color
        exp = 4
    ).

    DATA(lo_p3) = NEW lcl_bot( )->set_color( 7 ).

    lo_pool->add( lo_p3 ).

    cl_aunit_assert=>assert_equals(
        act = lo_p3->w_color
        exp = 7
    ).

    DATA(lo_p4) = NEW lcl_bot( ).

    lo_pool->add( lo_p4 ).

    cl_aunit_assert=>assert_equals(
        act = lo_p4->w_color
        exp = 1
    ).

  ENDMETHOD.

ENDCLASS.