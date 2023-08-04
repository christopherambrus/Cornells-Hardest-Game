open Levels
(**Initializing all Level modules*)

module A = TestLevel
module B = Level2
module C = Level3
module D = Level4
module E = Level5
module F = Level6
module G = Level7

type game_state = {
  lives : int;
  current_level_id : int;
  did_you_win : bool;
}
(**Record type for the game_state*)

(**The initial game state.*)
let initial_game_state =
  { lives = 10; current_level_id = 1; did_you_win = false }

(**[init_window ()] opens an empty graph with the title "Cornell's
   Hardest Game"*)
let init_window () =
  Graphics.open_graph " ";
  Graphics.resize_window 1000 600;
  Graphics.set_window_title "Cornell's Hardest Game"

(**[init_title_screen ()] draws the game's title screen on the graph.*)
let init_title_screen () =
  let img = OPng.load_as_rgb24 "Slide1.png" [] in
  let img = OImages.rgb24 img in
  let nw = truncate (float img#width *. 0.9)
  and nh = truncate (float img#height *. 0.9) in
  let newimage = img#resize None nw nh in
  let g = Graphic_image.of_image newimage#image in
  Graphics.draw_image g (Graphics.size_x () - nw + 75) 0;
  let e = Graphics.wait_next_event [ Key_pressed ] in
  if e.keypressed then Graphics.clear_graph ()

(**[init_instructions_screen ()] draws the instructions screen on the
   graph.*)
let init_instructions_screen () =
  let img = OPng.load_as_rgb24 "Slide2.png" [] in
  let img = OImages.rgb24 img in
  let nw = truncate (float img#width *. 0.8)
  and nh = truncate (float img#height *. 0.8) in
  let newimage = img#resize None nw nh in
  let g = Graphic_image.of_image newimage#image in
  Graphics.draw_image g (Graphics.size_x () - nw - 1) 0;
  let e = Graphics.wait_next_event [ Key_pressed ] in
  if e.keypressed then Graphics.clear_graph ()

(**[pley_level g lvl] passes the current game state updates the current
   game state g according to the functions of the level represented by
   the string lvl. This updated game state is returned either when the
   player beats the level or loses all of their lives.*)
let play_level (game_state : game_state) lvl_module =
  (*If test_mode is set to true, obstacle collisions are not detected.
    To test a specific level, edit the [levels] list to include only the
    string representation of that level.*)
  let test_mode = false in

  (*Selecting Correct Module Function*)
  let initialize_game =
    match lvl_module with
    | "A" -> A.initialize_game
    | "B" -> B.initialize_game
    | "C" -> C.initialize_game
    | "D" -> D.initialize_game
    | "E" -> E.initialize_game
    | "F" -> F.initialize_game
    | "G" -> G.initialize_game
    | _ -> A.initialize_game
  in
  let update =
    match lvl_module with
    | "A" -> A.update
    | "B" -> B.update
    | "C" -> C.update
    | "D" -> D.update
    | "E" -> E.update
    | "F" -> F.update
    | "G" -> G.update
    | _ -> A.update
  in
  let initial_level =
    match lvl_module with
    | "A" -> A.level
    | "B" -> B.level
    | "C" -> C.level
    | "D" -> D.level
    | "E" -> E.level
    | "F" -> F.level
    | "G" -> G.level
    | _ -> A.level
  in
  let check_collisions =
    match lvl_module with
    | "A" -> A.check_collisions
    | "B" -> B.check_collisions
    | "C" -> C.check_collisions
    | "D" -> D.check_collisions
    | "E" -> E.check_collisions
    | "F" -> F.check_collisions
    | "G" -> G.check_collisions
    | _ -> A.check_collisions
  in
  let reset_player =
    match lvl_module with
    | "A" -> A.reset_player
    | "B" -> B.reset_player
    | "C" -> C.reset_player
    | "D" -> D.reset_player
    | "E" -> E.reset_player
    | "F" -> F.reset_player
    | "G" -> G.reset_player
    | _ -> A.reset_player
  in
  let check_collections =
    match lvl_module with
    | "A" -> A.check_collections
    | "B" -> B.check_collections
    | "C" -> C.check_collections
    | "D" -> D.check_collections
    | "E" -> E.check_collections
    | "F" -> F.check_collections
    | "G" -> G.check_collections
    | _ -> A.check_collections
  in
  let check_finish =
    match lvl_module with
    | "A" -> A.check_finish
    | "B" -> B.check_finish
    | "C" -> C.check_finish
    | "D" -> D.check_finish
    | "E" -> E.check_finish
    | "F" -> F.check_finish
    | "G" -> G.check_finish
    | _ -> A.check_finish
  in

  initialize_game initial_level;

  let rec play_game state_of_game is_done level =
    if test_mode then
      match Graphics.mouse_pos () with
      | a, b ->
          print_endline
            ("(" ^ string_of_int a ^ "," ^ string_of_int b ^ ")")
    else ();

    (*If game is done, return game_state*)
    if is_done then state_of_game
    else
      (*update obstacles and player positions*)
      let updated_level = update level state_of_game.lives in

      (*Branch if collision is detected. Resets player and deducts a
        life. If 0 lives are left, game ends.*)
      if check_collisions updated_level then
        let updated_level_2 =
          if test_mode then updated_level
          else reset_player updated_level
        in
        let updated_game_state =
          {
            lives =
              (if test_mode then state_of_game.lives
              else state_of_game.lives - 1);
            current_level_id = state_of_game.current_level_id;
            did_you_win = false;
          }
        in
        if updated_game_state.lives = 0 then
          play_game updated_game_state true updated_level_2
        else play_game updated_game_state false updated_level_2
      else
        let updated_level_2 = check_collections updated_level in

        (*Check if player reached finish. If not, loop again)*)
        if check_finish updated_level_2 then
          play_game
            {
              lives = state_of_game.lives;
              current_level_id = state_of_game.current_level_id + 1;
              did_you_win = true;
            }
            true updated_level_2
        else play_game state_of_game false updated_level_2
  in
  play_game game_state false initial_level

(**[init_victory_screen ()] draws the victory screen onto the graph.*)
let rec init_victory_screen () =
  Graphics.clear_graph ();
  Graphics.set_color Graphics.black;
  Graphics.set_text_size 1000;
  Graphics.moveto 400 300;
  Graphics.draw_string "Game Complete! Press q to quit.";
  let e = Graphics.wait_next_event [ Key_pressed ] in
  if e.keypressed && Graphics.read_key () = 'q' then
    Graphics.clear_graph ()
  else init_victory_screen ()

(**[initialize_game_over_screen ()] draws the game over screen onto the
   graph.*)
let rec init_game_over_screen () =
  Graphics.clear_graph ();
  Graphics.set_color Graphics.red;
  Graphics.set_text_size 1000;
  Graphics.moveto 400 300;
  Graphics.draw_string "Game Over. Press q to quit.";
  let e = Graphics.wait_next_event [ Key_pressed ] in
  if e.keypressed && Graphics.read_key () = 'q' then
    Graphics.clear_graph ()
  else init_game_over_screen ()

(**This starts up the game and handles transitions between levels and
   the title/victory/gameover screens*)
let () =
  let levels = [ "A"; "B"; "C"; "D"; "E"; "F"; "G" ] in
  init_window ();
  init_title_screen ();
  init_instructions_screen ();
  let rec start gs lvls_lst =
    match lvls_lst with
    | [] ->
        if gs.lives = 0 then (
          init_game_over_screen ();
          gs)
        else (
          init_victory_screen ();
          gs)
    | a :: b ->
        if gs.lives = 0 then (
          init_game_over_screen ();
          gs)
        else
          let new_game_state = play_level gs a in
          start new_game_state b
  in

  let final_game_state = start initial_game_state levels in

  let ignore_field _ = () in

  (*These are here because it won't run unless these fields are used at
    some point. They will be used when we add on more levels.*)
  ignore_field final_game_state.lives;
  ignore_field final_game_state.current_level_id;
  ignore_field final_game_state.did_you_win
