*&---------------------------------------------------------------------*
*&  REPORT    Z4W
*&---------------------------------------------------------------------*
*&  AUTOR     lausek
*&  DATE      24.07.2017 10:23:45
*&
*&  TODO      JOB/PROBLEM TO SOLVE
*&  TEST      DEBUG/TEST-SCENARIO
*&  RELEASE   TODO BEFORE RELEASING THE PROGRAM
*&---------------------------------------------------------------------*

REPORT z4w NO STANDARD PAGE HEADING.

*----------------------------------------------------------------------*
*        INCLUDES                                                      *
*----------------------------------------------------------------------*
INCLUDE:  Z4W_LCL_UTILS,
          Z4W_LCL_PLAYER,
          Z4W_LCL_BOTS,
          Z4W_LCL_HUMAN,
          Z4W_LCL_PLAYER_POOL,
          Z4W_LCL_GAMEFIELD,
          Z4W_LCL_CLEVER_BOT,
          Z4W_LCL_TEST_CLASS.

*----------------------------------------------------------------------*
*        SELECTION-SCREEN                                              *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK placeholder WITH FRAME.
PARAMETERS:
  p_name TYPE xubname DEFAULT sy-uname.
SELECTION-SCREEN SKIP.
PARAMETER:
  p_red     RADIOBUTTON GROUP clr,
  p_green   RADIOBUTTON GROUP clr,
  p_blue    RADIOBUTTON GROUP clr,
  p_yellow  RADIOBUTTON GROUP clr.
SELECTION-SCREEN END OF BLOCK placeholder.

*----------------------------------------------------------------------*
*        CLASSES                                                       *
*----------------------------------------------------------------------*
CLASS lcl_control DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA:
      o_field TYPE REF TO lcl_gamefield.

    CLASS-METHODS:
      main.

ENDCLASS.

CLASS lcl_control IMPLEMENTATION.

  METHOD main.

    DATA(lo_players)  = NEW lcl_player_pool( ).
    DATA(lo_bot)      = NEW lcl_clever_bot( ).

    lo_players->add( NEW lcl_human( )->set_name( p_name )->set_color(
        COND #(
            WHEN p_red      = abap_true THEN 6
            WHEN p_green    = abap_true THEN 5
            WHEN p_blue     = abap_true THEN 4
            ELSE 3 " yellow
        )
    ) ).
    lo_players->add( lo_bot ).

    DO lcl_utils=>get_random_number( iw_to = 6 ) TIMES.
      lo_players->next( ).
    ENDDO.

    o_field = NEW lcl_gamefield( lo_players ).

    lo_bot->set_field( o_field ).

    LOOP AT lo_players->i_objects ASSIGNING FIELD-SYMBOL(<lfs_player>).
      SET HANDLER o_field->handle_pick FOR <lfs_player>.
    ENDLOOP.

    o_field->redraw( ).

    lo_players->get( )->pick( ).

  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
*        EVENTS                                                        *
*----------------------------------------------------------------------*
AT LINE-SELECTION.
  lcl_control=>o_field->o_player_pool->human_pick( ).

START-OF-SELECTION.
  lcl_control=>main( ).