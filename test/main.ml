(*TEST PLAN: We have three sets of OUnit tests for the data types and
  function definitions in Levels.ml: game_border_tests,
  game_finish_tests, and obstacle_collision_tests ( whereas the Game.ml
  game loop, title screens, GUI etc. were tested manually). In
  game_border_tests, we test every wall in every level to make sure that
  the user cannot go over the bounds of the level. This involves testing
  the update_player function of each level: we place the player beyond
  the wall and check that this update function corrects the position of
  the player to be within bounds. This involves calculations using user
  input so running those tests will open a Graphics window and prompt
  you to press certain keys. In game_finish_tests, we test to make sure
  that the player can only finish each level if they're within the
  finish area and they meet all of the finish conditions of that level
  (for example, some levels require the player to collect one or more
  keys.) This involves testing the check_finish and check_collections
  functions of each level. We first check that check_finish returns
  false when the player is in the spawn position, and that it returns
  false if the player has only partially fulfilled the finish
  requirements (we use check_collections to collect keys one by one),
  and finally, we check that check_finish returns true when these
  conditions have been meet and the player is in the goal. Finally, in
  obstacle_collisions_test, we test to make sure that the
  check_collisions function of each level returns true or false when
  appropriate (we check for every obstacle). All other aspects of the
  game have been tested through play testing. Additionally, these OUnit
  tests utilize the fact that we know the implementation of these
  functions as well as a lot of the stored values (obstacle and player
  positions etc.) of each level so there is a degree of glass box
  testing, but it is mostly black box testing. All in all, these OUnit
  test cases ensure that the player cannot over step the bounds of any
  level, that the player can only finish each level after meeting
  certain conditions, and that obstacle collisions are properly
  detected. These are THE major functions of the game, so we believe
  that we have thoroughly ensured the correctness of our program with
  these test cases. NOTE: you can toggle the test_borders boolean
  directly below this to false if you want to skip the tests that
  involve user input because it can be a bit tedious. We meet all of the
  testing requirements with the last two sets of tests alone anyways.*)

(*Booleans for enabling/disabling test cases*)
let test_borders = true
let test_check_finish = true
let test_collisions = true

open Levels
open OUnit2
module A = TestLevel
module B = Level2
module C = Level3
module D = Level4
module E = Level5
module F = Level6
module G = Level7

(*[init_window ()] opens up an empty graph.*)
let init_window () =
  Graphics.open_graph " ";
  Graphics.resize_window 1000 600;
  ()

(*[update_border_test_screen level_num k] writes on the graph the
  current level number that borders are being tested for, indicated by
  level_num, as well as the next key, k, to be pressed by the tester.*)
let update_border_test_screen levl_num key_to_press =
  Graphics.clear_graph ();
  Graphics.moveto 75 400;
  Graphics.draw_string
    ("Level " ^ levl_num
   ^ ": Hi. We are testing border collision calculations (which depend \
      on user input). Please press the letters as they appear on the \
      screen:");
  Graphics.moveto 500 300;
  Graphics.draw_string key_to_press

(*[test_updated_player_y p e f] tests the equality of the expected x
  coordinate, e, and the x coordinate of the player p after the player
  update function, f, is applied to it.*)
let test_updated_player_x
    (player : player)
    expected
    (update_func : player -> player) =
  let updated_player = update_func player in
  assert_equal updated_player.x_pos expected ~printer:string_of_int

(*[test_updated_player_y p e f] tests the equality of the expected y
  coordinate, e, and the y coordinate of the player p after the player
  update function, f, is applied to it.*)
let test_updated_player_y
    (player : player)
    expected
    (update_func : player -> player) =
  let updated_player = update_func player in
  assert_equal updated_player.y_pos expected ~printer:string_of_int

(*global var for player radius*)
let rad = 20

(*These tests ensure that for every level, the player cannot step out of
  the bounds of the level. This is tested extensively because it's
  actually very easy to make a mistake in programming the border
  collisions.*)
let game_border_tests =
  if test_borders then
    [
      ( "Test_Game_Borders level" >:: fun _ ->
        (*LEVEL 1 BORDERS*)
        init_window ();
        update_border_test_screen "1" "Level 1: D";
        test_updated_player_x
          { x_pos = 25; y_pos = 293; radius = rad }
          70 A.update_player;
        update_border_test_screen "1" "Level 1: W";
        test_updated_player_y
          { x_pos = 91; y_pos = 240; radius = rad }
          270 A.update_player;
        update_border_test_screen "1" "Level 1: S";
        test_updated_player_y
          { x_pos = 104; y_pos = 361; radius = rad }
          330 A.update_player;
        update_border_test_screen "1" "Level 1: D";
        test_updated_player_x
          { x_pos = 184; y_pos = 383; radius = rad }
          220 A.update_player;
        update_border_test_screen "1" "Level 1: A";
        test_updated_player_x
          { x_pos = 184; y_pos = 219; radius = rad }
          220 A.update_player;
        update_border_test_screen "1" "Level 1: W";
        test_updated_player_y
          { x_pos = 395; y_pos = 443; radius = rad }
          390 A.update_player;
        update_border_test_screen "1" "Level 1: S";
        test_updated_player_y
          { x_pos = 395; y_pos = 180; radius = rad }
          210 A.update_player;
        update_border_test_screen "1" "Level 1: D";
        test_updated_player_x
          { x_pos = 760; y_pos = 384; radius = rad }
          730 A.update_player;
        update_border_test_screen "1" "Level 1: W";
        test_updated_player_y
          { x_pos = 811; y_pos = 363; radius = rad }
          330 A.update_player;
        update_border_test_screen "1" "Level 1: S";
        test_updated_player_y
          { x_pos = 811; y_pos = 240; radius = rad }
          270 A.update_player;
        update_border_test_screen "1" "Level 1: A";
        test_updated_player_x
          { x_pos = 897; y_pos = 300; radius = rad }
          860 A.update_player;

        (*Level 2 Borders*)
        update_border_test_screen "2" "Level 2: D";
        test_updated_player_x
          { x_pos = 25; y_pos = 293; radius = rad }
          70 B.update_player;
        update_border_test_screen "2" "Level 2: W";
        test_updated_player_y
          { x_pos = 91; y_pos = 240; radius = rad }
          270 B.update_player;
        update_border_test_screen "2" "Level 2: S";
        test_updated_player_y
          { x_pos = 104; y_pos = 361; radius = rad }
          330 B.update_player;
        update_border_test_screen "2" "Level 2: D";
        test_updated_player_x
          { x_pos = 184; y_pos = 383; radius = rad }
          220 B.update_player;
        update_border_test_screen "2" "Level 2: A";
        test_updated_player_x
          { x_pos = 184; y_pos = 219; radius = rad }
          220 B.update_player;
        update_border_test_screen "2" "Level 2: W";
        test_updated_player_y
          { x_pos = 395; y_pos = 443; radius = rad }
          390 B.update_player;
        update_border_test_screen "2" "Level 2: S";
        test_updated_player_y
          { x_pos = 395; y_pos = 180; radius = rad }
          210 B.update_player;
        update_border_test_screen "2" "Level 2: D";
        test_updated_player_x
          { x_pos = 760; y_pos = 384; radius = rad }
          730 B.update_player;
        update_border_test_screen "2" "Level 2: W";
        test_updated_player_y
          { x_pos = 811; y_pos = 363; radius = rad }
          330 B.update_player;
        update_border_test_screen "2" "Level 2: S";
        test_updated_player_y
          { x_pos = 811; y_pos = 240; radius = rad }
          270 B.update_player;
        update_border_test_screen "2" "Level 2: A";
        test_updated_player_x
          { x_pos = 897; y_pos = 300; radius = rad }
          860 B.update_player;

        (*Level 3 Borders*)
        update_border_test_screen "3" "Level 3: D";
        test_updated_player_x
          { x_pos = 25; y_pos = 293; radius = rad }
          70 C.update_player;
        update_border_test_screen "3" "Level 3: W";
        test_updated_player_y
          { x_pos = 91; y_pos = 240; radius = rad }
          270 C.update_player;
        update_border_test_screen "3" "Level 3: S";
        test_updated_player_y
          { x_pos = 104; y_pos = 361; radius = rad }
          330 C.update_player;
        update_border_test_screen "3" "Level 3: D";
        test_updated_player_x
          { x_pos = 184; y_pos = 383; radius = rad }
          220 C.update_player;
        update_border_test_screen "3" "Level 3: A";
        test_updated_player_x
          { x_pos = 184; y_pos = 219; radius = rad }
          220 C.update_player;
        update_border_test_screen "3" "Level 3: W";
        test_updated_player_y
          { x_pos = 395; y_pos = 443; radius = rad }
          390 C.update_player;
        update_border_test_screen "3" "Level 3: S";
        test_updated_player_y
          { x_pos = 395; y_pos = 180; radius = rad }
          210 C.update_player;
        update_border_test_screen "3" "Level 3: D";
        test_updated_player_x
          { x_pos = 760; y_pos = 384; radius = rad }
          730 C.update_player;
        update_border_test_screen "3" "Level 3: W";
        test_updated_player_y
          { x_pos = 811; y_pos = 363; radius = rad }
          330 C.update_player;
        update_border_test_screen "3" "Level 3: S";
        test_updated_player_y
          { x_pos = 811; y_pos = 240; radius = rad }
          270 C.update_player;
        update_border_test_screen "3" "Level 3: A";
        test_updated_player_x
          { x_pos = 897; y_pos = 300; radius = rad }
          860 C.update_player;

        (*Level 4 Borders*)
        update_border_test_screen "4" "Level 4: D";
        test_updated_player_x
          { x_pos = 25; y_pos = 293; radius = rad }
          70 D.update_player;
        update_border_test_screen "4" "Level 4: W";
        test_updated_player_y
          { x_pos = 91; y_pos = 240; radius = rad }
          270 D.update_player;
        update_border_test_screen "4" "Level 4: S";
        test_updated_player_y
          { x_pos = 104; y_pos = 361; radius = rad }
          330 D.update_player;
        update_border_test_screen "4" "Level 4: D";
        test_updated_player_x
          { x_pos = 184; y_pos = 383; radius = rad }
          220 D.update_player;
        update_border_test_screen "4" "Level 4: A";
        test_updated_player_x
          { x_pos = 184; y_pos = 219; radius = rad }
          220 D.update_player;
        update_border_test_screen "4" "Level 4: W";
        test_updated_player_y
          { x_pos = 395; y_pos = 443; radius = rad }
          390 D.update_player;
        update_border_test_screen "4" "Level 4: S";
        test_updated_player_y
          { x_pos = 395; y_pos = 180; radius = rad }
          210 D.update_player;
        update_border_test_screen "4" "Level 4: D";
        test_updated_player_x
          { x_pos = 760; y_pos = 384; radius = rad }
          730 D.update_player;
        update_border_test_screen "4" "Level 4: W";
        test_updated_player_y
          { x_pos = 811; y_pos = 363; radius = rad }
          330 D.update_player;
        update_border_test_screen "4" "Level 4: S";
        test_updated_player_y
          { x_pos = 811; y_pos = 240; radius = rad }
          270 D.update_player;
        update_border_test_screen "4" "Level 4: A";
        test_updated_player_x
          { x_pos = 897; y_pos = 300; radius = rad }
          860 D.update_player;

        (*Level 5 Borders*)
        update_border_test_screen "5" "Level 5: D";
        test_updated_player_x
          { x_pos = 25; y_pos = 293; radius = rad }
          70 E.update_player;
        update_border_test_screen "5" "Level 5: W";
        test_updated_player_y
          { x_pos = 91; y_pos = 240; radius = rad }
          270 E.update_player;
        update_border_test_screen "5" "Level 5: S";
        test_updated_player_y
          { x_pos = 104; y_pos = 361; radius = rad }
          330 E.update_player;
        update_border_test_screen "5" "Level 5: D";
        test_updated_player_x
          { x_pos = 184; y_pos = 383; radius = rad }
          220 E.update_player;
        update_border_test_screen "5" "Level 5: A";
        test_updated_player_x
          { x_pos = 184; y_pos = 219; radius = rad }
          220 E.update_player;
        update_border_test_screen "5" "Level 5: W";
        test_updated_player_y
          { x_pos = 395; y_pos = 443; radius = rad }
          390 E.update_player;
        update_border_test_screen "5" "Level 5: S";
        test_updated_player_y
          { x_pos = 395; y_pos = 180; radius = rad }
          210 E.update_player;

        (*Level 6 Borders*)
        update_border_test_screen "6" "Level 6: D";
        test_updated_player_x
          { x_pos = 25; y_pos = 293; radius = rad }
          70 F.update_player;
        update_border_test_screen "6" "Level 6: W";
        test_updated_player_y
          { x_pos = 91; y_pos = 240; radius = rad }
          270 F.update_player;
        update_border_test_screen "6" "Level 6: S";
        test_updated_player_y
          { x_pos = 104; y_pos = 361; radius = rad }
          330 F.update_player;
        update_border_test_screen "6" "Level 6: D";
        test_updated_player_x
          { x_pos = 184; y_pos = 383; radius = rad }
          220 F.update_player;
        update_border_test_screen "6" "Level 6: A";
        test_updated_player_x
          { x_pos = 184; y_pos = 219; radius = rad }
          220 F.update_player;
        update_border_test_screen "6" "Level 6: W";
        test_updated_player_y
          { x_pos = 395; y_pos = 443; radius = rad }
          390 F.update_player;
        update_border_test_screen "6" "Level 6: S";
        test_updated_player_y
          { x_pos = 395; y_pos = 180; radius = rad }
          210 F.update_player;
        update_border_test_screen "6" "Level 6: D";
        test_updated_player_x
          { x_pos = 760; y_pos = 384; radius = rad }
          730 F.update_player;
        update_border_test_screen "6" "Level 6: W";
        test_updated_player_y
          { x_pos = 811; y_pos = 363; radius = rad }
          330 F.update_player;
        update_border_test_screen "6" "Level 6: S";
        test_updated_player_y
          { x_pos = 811; y_pos = 240; radius = rad }
          270 F.update_player;
        update_border_test_screen "6" "Level 6: A";
        test_updated_player_x
          { x_pos = 897; y_pos = 300; radius = rad }
          860 F.update_player;

        (*Level 7 Borders*)
        update_border_test_screen "7" "Level 7: D";
        test_updated_player_x
          { x_pos = 190; y_pos = 300; radius = rad }
          225 G.update_player;
        update_border_test_screen "7" "Level 7: W";
        test_updated_player_y
          { x_pos = 280; y_pos = 240; radius = rad }
          270 G.update_player;
        update_border_test_screen "7" "Level 7: S";
        test_updated_player_y
          { x_pos = 270; y_pos = 370; radius = rad }
          330 G.update_player;
        update_border_test_screen "7" "Level 7: A";
        test_updated_player_x
          { x_pos = 352; y_pos = 385; radius = rad }
          380 G.update_player;
        update_border_test_screen "7" "Level 7: D";
        test_updated_player_x
          { x_pos = 352; y_pos = 220; radius = rad }
          380 G.update_player;
        update_border_test_screen "7" "Level 7: S";
        test_updated_player_y
          { x_pos = 400; y_pos = 180; radius = rad }
          210 G.update_player;
        update_border_test_screen "7" "Level 7: W";
        test_updated_player_y
          { x_pos = 400; y_pos = 420; radius = rad }
          390 G.update_player;
        update_border_test_screen "7" "Level 7: A";
        test_updated_player_x
          { x_pos = 440; y_pos = 436; radius = rad }
          472 G.update_player;
        update_border_test_screen "7" "Level 7: D";
        test_updated_player_x
          { x_pos = 440; y_pos = 163; radius = rad }
          472 G.update_player;
        update_border_test_screen "7" "Level 7: S";
        test_updated_player_y
          { x_pos = 480; y_pos = 130; radius = rad }
          160 G.update_player;
        update_border_test_screen "7" "Level 7: W";
        test_updated_player_y
          { x_pos = 480; y_pos = 470; radius = rad }
          440 G.update_player;
        update_border_test_screen "7" "Level 7: D";
        test_updated_player_x
          { x_pos = 515; y_pos = 163; radius = rad }
          487 G.update_player;
        update_border_test_screen "7" "Level 7: A";
        test_updated_player_x
          { x_pos = 515; y_pos = 436; radius = rad }
          487 G.update_player;
        update_border_test_screen "7" "Level 7: S";
        test_updated_player_y
          { x_pos = 550; y_pos = 185; radius = rad }
          210 G.update_player;
        update_border_test_screen "7" "Level 7: W";
        test_updated_player_y
          { x_pos = 550; y_pos = 425; radius = rad }
          390 G.update_player;
        update_border_test_screen "7" "Level 7: D";
        test_updated_player_x
          { x_pos = 615; y_pos = 370; radius = rad }
          580 G.update_player;
        update_border_test_screen "7" "Level 7: A";
        test_updated_player_x
          { x_pos = 615; y_pos = 230; radius = rad }
          580 G.update_player;
        update_border_test_screen "7" "Level 7: S";
        test_updated_player_y
          { x_pos = 625; y_pos = 265; radius = rad }
          295 G.update_player;
        update_border_test_screen "7" "Level 7: W";
        test_updated_player_y
          { x_pos = 625; y_pos = 340; radius = rad }
          310 G.update_player;
        update_border_test_screen "7" "Level 7: D";
        test_updated_player_x
          { x_pos = 660; y_pos = 302; radius = rad }
          630 G.update_player;
        Graphics.close_graph () );
    ]
  else []

(*For each level, we first check that check_finish returns false
  (because the player should not be in the goal initially) and then
  check that it returns true when the player is in the finish area. For
  levels that require keys to unlock the finish, we additionally check
  that being in the finish initially returns false, but then returns
  true when all the keys have been collected*)
let game_finish_tests =
  if test_check_finish then
    [
      ( "Level 1 Test check_finish initially false" >:: fun _ ->
        assert_equal false
          (A.check_finish A.level)
          ~printer:string_of_bool );
      ( "Level 1 Test check_finish returns true in goal" >:: fun _ ->
        let level_player_in_fin =
          {
            start_pos_x = A.level.start_pos_x;
            start_pos_y = A.level.start_pos_y;
            obstacles = A.level.obstacles;
            collectables = A.level.collectables;
            finish_area = A.level.finish_area;
            exit_open = A.level.exit_open;
            p = { x_pos = 825; y_pos = 300; radius = A.level.p.radius };
          }
        in
        assert_equal true
          (A.check_finish level_player_in_fin)
          ~printer:string_of_bool );
      ( "Level 2 Test check_finish initially false" >:: fun _ ->
        assert_equal false
          (B.check_finish B.level)
          ~printer:string_of_bool );
      ( "Level 2 Test check_finish returns false in goal initially"
      >:: fun _ ->
        let level_player_in_fin =
          {
            start_pos_x = B.level.start_pos_x;
            start_pos_y = B.level.start_pos_y;
            obstacles = B.level.obstacles;
            collectables = B.level.collectables;
            finish_area = B.level.finish_area;
            exit_open = B.level.exit_open;
            p = { x_pos = 825; y_pos = 300; radius = B.level.p.radius };
          }
        in
        assert_equal false
          (B.check_finish level_player_in_fin)
          ~printer:string_of_bool );
      ( "Level 2 Test check_finish returns true after key collected"
      >:: fun _ ->
        let player_at_key =
          {
            start_pos_x = B.level.start_pos_x;
            start_pos_y = B.level.start_pos_y;
            obstacles = B.level.obstacles;
            collectables = B.level.collectables;
            finish_area = B.level.finish_area;
            exit_open = B.level.exit_open;
            p = { x_pos = 475; y_pos = 300; radius = B.level.p.radius };
          }
        in
        assert_equal false player_at_key.exit_open
          ~printer:string_of_bool;
        let level_with_collected_key =
          B.check_collections player_at_key
        in
        assert_equal true level_with_collected_key.exit_open
          ~printer:string_of_bool;
        let player_at_exit_with_key =
          {
            start_pos_x = level_with_collected_key.start_pos_x;
            start_pos_y = level_with_collected_key.start_pos_y;
            obstacles = level_with_collected_key.obstacles;
            collectables = level_with_collected_key.collectables;
            finish_area = level_with_collected_key.finish_area;
            exit_open = level_with_collected_key.exit_open;
            p = { x_pos = 825; y_pos = 300; radius = B.level.p.radius };
          }
        in
        assert_equal true
          (B.check_finish player_at_exit_with_key)
          ~printer:string_of_bool );
      ( "Level 3 Test check_finish initially false" >:: fun _ ->
        assert_equal false
          (C.check_finish C.level)
          ~printer:string_of_bool );
      ( "Level 3 Test check_finish returns true in goal" >:: fun _ ->
        let level_player_in_fin =
          {
            start_pos_x = C.level.start_pos_x;
            start_pos_y = C.level.start_pos_y;
            obstacles = C.level.obstacles;
            collectables = C.level.collectables;
            finish_area = C.level.finish_area;
            exit_open = C.level.exit_open;
            p = { x_pos = 825; y_pos = 300; radius = C.level.p.radius };
          }
        in
        assert_equal true
          (C.check_finish level_player_in_fin)
          ~printer:string_of_bool );
      ( "Level 4 Test check_finish initially false" >:: fun _ ->
        assert_equal false
          (D.check_finish D.level)
          ~printer:string_of_bool );
      ( "Level 4 Test check_finish returns false in goal initially"
      >:: fun _ ->
        let level_player_in_fin =
          {
            start_pos_x = D.level.start_pos_x;
            start_pos_y = D.level.start_pos_y;
            obstacles = D.level.obstacles;
            collectables = D.level.collectables;
            finish_area = D.level.finish_area;
            exit_open = D.level.exit_open;
            p = { x_pos = 825; y_pos = 300; radius = D.level.p.radius };
          }
        in
        assert_equal false
          (D.check_finish level_player_in_fin)
          ~printer:string_of_bool );
      ( "Level 4 Test check_finish returns true after key collected"
      >:: fun _ ->
        let player_at_key =
          {
            start_pos_x = D.level.start_pos_x;
            start_pos_y = D.level.start_pos_y;
            obstacles = D.level.obstacles;
            collectables = D.level.collectables;
            finish_area = D.level.finish_area;
            exit_open = D.level.exit_open;
            p = { x_pos = 475; y_pos = 300; radius = D.level.p.radius };
          }
        in
        assert_equal false player_at_key.exit_open
          ~printer:string_of_bool;
        let level_with_collected_key =
          D.check_collections player_at_key
        in
        assert_equal true level_with_collected_key.exit_open
          ~printer:string_of_bool;
        let player_at_exit_with_key =
          {
            start_pos_x = level_with_collected_key.start_pos_x;
            start_pos_y = level_with_collected_key.start_pos_y;
            obstacles = level_with_collected_key.obstacles;
            collectables = level_with_collected_key.collectables;
            finish_area = level_with_collected_key.finish_area;
            exit_open = level_with_collected_key.exit_open;
            p = { x_pos = 825; y_pos = 300; radius = D.level.p.radius };
          }
        in
        assert_equal true
          (D.check_finish player_at_exit_with_key)
          ~printer:string_of_bool );
      ( "Level 5 Test check_finish initially false" >:: fun _ ->
        assert_equal false
          (E.check_finish E.level)
          ~printer:string_of_bool );
      ( "Level 5 Test check_finish returns true in goal" >:: fun _ ->
        let level_player_in_fin =
          {
            start_pos_x = E.level.start_pos_x;
            start_pos_y = E.level.start_pos_y;
            obstacles = E.level.obstacles;
            collectables = E.level.collectables;
            finish_area = E.level.finish_area;
            exit_open = E.level.exit_open;
            p = { x_pos = 825; y_pos = 300; radius = E.level.p.radius };
          }
        in
        assert_equal true
          (E.check_finish level_player_in_fin)
          ~printer:string_of_bool );
      ( "Level 6 Test check_finish initially false" >:: fun _ ->
        assert_equal false
          (F.check_finish F.level)
          ~printer:string_of_bool );
      ( "Level 6 Test check_finish returns true in goal" >:: fun _ ->
        let level_player_in_fin =
          {
            start_pos_x = F.level.start_pos_x;
            start_pos_y = F.level.start_pos_y;
            obstacles = F.level.obstacles;
            collectables = F.level.collectables;
            finish_area = F.level.finish_area;
            exit_open = F.level.exit_open;
            p = { x_pos = 825; y_pos = 300; radius = F.level.p.radius };
          }
        in
        assert_equal true
          (F.check_finish level_player_in_fin)
          ~printer:string_of_bool );
      ( "Level 7 Test check_finish initially false" >:: fun _ ->
        assert_equal false
          (G.check_finish G.level)
          ~printer:string_of_bool );
      ( "Level 7 Test check_finish returns false in goal initially"
      >:: fun _ ->
        let level_player_in_fin =
          {
            start_pos_x = G.level.start_pos_x;
            start_pos_y = G.level.start_pos_y;
            obstacles = G.level.obstacles;
            collectables = G.level.collectables;
            finish_area = G.level.finish_area;
            exit_open = G.level.exit_open;
            p = { x_pos = 250; y_pos = 300; radius = G.level.p.radius };
          }
        in
        assert_equal false
          (G.check_finish level_player_in_fin)
          ~printer:string_of_bool );
      ( "Level 7 Test check_finish returns true after key collected"
      >:: fun _ ->
        let player_at_key1 =
          {
            start_pos_x = G.level.start_pos_x;
            start_pos_y = G.level.start_pos_y;
            obstacles = G.level.obstacles;
            collectables = G.level.collectables;
            finish_area = G.level.finish_area;
            exit_open = G.level.exit_open;
            p = { x_pos = 480; y_pos = 436; radius = G.level.p.radius };
          }
        in
        assert_equal false player_at_key1.exit_open
          ~printer:string_of_bool;
        let level_with_collected_key1 =
          G.check_collections player_at_key1
        in
        assert_equal false level_with_collected_key1.exit_open
          ~printer:string_of_bool;

        let player_at_key_2 =
          {
            start_pos_x = level_with_collected_key1.start_pos_x;
            start_pos_y = level_with_collected_key1.start_pos_y;
            obstacles = level_with_collected_key1.obstacles;
            collectables = level_with_collected_key1.collectables;
            finish_area = level_with_collected_key1.finish_area;
            exit_open = level_with_collected_key1.exit_open;
            p = { x_pos = 622; y_pos = 300; radius = G.level.p.radius };
          }
        in
        assert_equal false player_at_key_2.exit_open
          ~printer:string_of_bool;
        let level_with_collected_key2 =
          G.check_collections player_at_key_2
        in
        assert_equal false level_with_collected_key2.exit_open
          ~printer:string_of_bool;
        let player_at_key_3 =
          {
            start_pos_x = level_with_collected_key2.start_pos_x;
            start_pos_y = level_with_collected_key2.start_pos_y;
            obstacles = level_with_collected_key2.obstacles;
            collectables = level_with_collected_key2.collectables;
            finish_area = level_with_collected_key2.finish_area;
            exit_open = level_with_collected_key2.exit_open;
            p = { x_pos = 480; y_pos = 163; radius = G.level.p.radius };
          }
        in
        assert_equal false player_at_key_3.exit_open
          ~printer:string_of_bool;
        let level_with_collected_key3 =
          G.check_collections player_at_key_3
        in
        assert_equal true level_with_collected_key3.exit_open
          ~printer:string_of_bool;

        let player_at_exit_with_all_keys =
          {
            start_pos_x = level_with_collected_key3.start_pos_x;
            start_pos_y = level_with_collected_key3.start_pos_y;
            obstacles = level_with_collected_key3.obstacles;
            collectables = level_with_collected_key3.collectables;
            finish_area = level_with_collected_key3.finish_area;
            exit_open = level_with_collected_key3.exit_open;
            p = { x_pos = 250; y_pos = 300; radius = G.level.p.radius };
          }
        in
        assert_equal true
          (G.check_finish player_at_exit_with_all_keys)
          ~printer:string_of_bool );
    ]
  else []

let test_collide_obstacles test_name expected func lvl =
  test_name >:: fun _ -> assert_equal expected (func lvl)

let change_player_pos lvl x y =
  {
    start_pos_x = lvl.start_pos_x;
    start_pos_y = lvl.start_pos_y;
    obstacles = lvl.obstacles;
    collectables = lvl.collectables;
    finish_area = lvl.finish_area;
    exit_open = lvl.exit_open;
    p = { x_pos = x; y_pos = y; radius = lvl.p.radius };
  }

(*Tests every level to see if check_collisions returns True when
  appropriate and false when appropriate.*)
let obstacle_collision_tests =
  if test_collisions then
    [
      test_collide_obstacles "Level1 - expected no collision" false
        A.check_collisions A.level;
      test_collide_obstacles "Level1 - Obstacle 1" true
        A.check_collisions
        (change_player_pos A.level 289 200);
      test_collide_obstacles "Level1 - Obstacle 2" true
        A.check_collisions
        (change_player_pos A.level 464 385);
      test_collide_obstacles "Level1 - Obstacle 3" true
        A.check_collisions
        (change_player_pos A.level 639 200);
      test_collide_obstacles "Level2 - expected no collision" false
        B.check_collisions B.level;
      test_collide_obstacles "Level2 - Obstacle 1" true
        B.check_collisions
        (change_player_pos B.level 289 200);
      test_collide_obstacles "Level2 - Obstacle 2" true
        B.check_collisions
        (change_player_pos B.level 464 385);
      test_collide_obstacles "Level2 - Obstacle 3" true
        B.check_collisions
        (change_player_pos B.level 639 200);
      test_collide_obstacles "Level3 - expected no collision" false
        C.check_collisions C.level;
      test_collide_obstacles "Level3 - Obstacle 1" true
        C.check_collisions
        (change_player_pos C.level 289 200);
      test_collide_obstacles "Level3 - Obstacle 2" true
        C.check_collisions
        (change_player_pos C.level 375 200);
      test_collide_obstacles "Level3 - Obstacle 3" true
        C.check_collisions
        (change_player_pos C.level 464 385);
      test_collide_obstacles "Level3 - Obstacle 4" true
        C.check_collisions
        (change_player_pos C.level 550 200);
      test_collide_obstacles "Level3 - Obstacle 5" true
        C.check_collisions
        (change_player_pos C.level 639 200);
      test_collide_obstacles "Level4 - expected no collision" false
        D.check_collisions D.level;
      test_collide_obstacles "Level4 - Obstacle 1" true
        D.check_collisions
        (change_player_pos D.level 289 385);
      test_collide_obstacles "Level4 - Obstacle 2" true
        D.check_collisions
        (change_player_pos D.level 375 350);
      test_collide_obstacles "Level4 - Obstacle 3" true
        D.check_collisions
        (change_player_pos D.level 464 300);
      test_collide_obstacles "Level4 - Obstacle 4" true
        D.check_collisions
        (change_player_pos D.level 550 250);
      test_collide_obstacles "Level4 - Obstacle 5" true
        D.check_collisions
        (change_player_pos D.level 639 200);
      test_collide_obstacles "Level5 - expected no collision" false
        E.check_collisions E.level;
      test_collide_obstacles "Level5 - Obstacle 1" true
        E.check_collisions
        (change_player_pos E.level 249 200);
      test_collide_obstacles "Level5 - Obstacle 2" true
        E.check_collisions
        (change_player_pos E.level 335 200);
      test_collide_obstacles "Level5 - Obstacle 3" true
        E.check_collisions
        (change_player_pos E.level 590 200);
      test_collide_obstacles "Level5 - Obstacle 4" true
        E.check_collisions
        (change_player_pos E.level 679 200);
      test_collide_obstacles "Level6 - expected no collision" false
        F.check_collisions F.level;
      test_collide_obstacles "Level6 - Obstacle 1" true
        F.check_collisions
        (change_player_pos F.level 280 180);
      test_collide_obstacles "Level6 - Obstacle 2" true
        F.check_collisions
        (change_player_pos F.level 460 400);
      test_collide_obstacles "Level6 - Obstacle 3" true
        F.check_collisions
        (change_player_pos F.level 640 180);
      test_collide_obstacles "Level7 - expected no collision" false
        G.check_collisions G.level;
      test_collide_obstacles "Level7 - Obstacle 1" true
        G.check_collisions
        (change_player_pos G.level 470 385);
      test_collide_obstacles "Level7 - Obstacle 2" true
        G.check_collisions
        (change_player_pos G.level 573 295);
      test_collide_obstacles "Level7 - Obstacle 3" true
        G.check_collisions
        (change_player_pos G.level 470 195);
      test_collide_obstacles "Level7 - Obstacle 1" true
        G.check_collisions
        (change_player_pos G.level 375 295);
    ]
  else []

(*NOTE: There are a bunch of tests in game_border_tests, but they all
  count as one. Otherwise, we would have to keep closing and opening
  graphs for each test.*)
let suite =
  "search test suite"
  >::: List.flatten
         [
           game_border_tests;
           game_finish_tests;
           obstacle_collision_tests;
         ]

let _ = run_test_tt_main suite
