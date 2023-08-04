(**Record type representing the position and size of the player.*)
type player = {
  x_pos : int;
  y_pos : int;
  radius : int;
}

(**Record type representing the position of a collectable*)
type collectable = {
  x_pos : int;
  y_pos : int;
  collectable_type : string;
}

(**Record type representing the position of an obstacle and its type. Its
  type can contain information about the shape of the osbtacle, it's
  movement patterns, or any other unique qualities (it does not have a
  strict format).*)
type obstacle = {
  x_pos : int;
  y_pos : int;
  obstacle_type : string;
  count : int;
}

(**Record type rerpesenting the state of a level.*)
type t = {
  start_pos_x : int;
  start_pos_y : int;
  obstacles : obstacle list;
  collectables : collectable list;
  finish_area : int * int * int * int;
  exit_open : bool;
  p : player;
}

(**Module type definition for Levels.*)
module type Level = sig
  (*Initial state of the level.*)
  val level : t

  (**[initializes_game g] initializes the level g by drawing all of its
    game components (the player, obstacles, boders, collectabels, finish
    area) in their starting positions.*)
  val initialize_game : t -> unit

  (**[update g i] Clears the graph and draws all game components of g in
    their current positions. The integer i is also drawn on the graph to
    indicate the number of remaining lives. The positions of all the
    game components in g are then updated.*)
  val update : t -> int -> t

  (**[check_collisions g] checks the coordinates of the player and all
     the obstacle components in g and returns True if there is an
     overlap between the player and any of those obstacles. Returns
     false otherwise.*)
  val check_collisions : t -> bool

  (**[check_collecyions g] checks the coordinates of the player and all
    the collectable components in g to determine if there is an overlap
    between the player and those collectables. The collectables list in
    g is updated appropriately depending on the type of collectable.*)
  val check_collections : t -> t

  (**[check_finish g] returns true if the player in g overlaps with the
    finish area in g and exit_open in g is set to true. Returns false
    otherwise.*)
  val check_finish : t -> bool

  (**[reset_player g] resets the coordinates of the player component of g
    to the starting coordinates in g.*)
  val reset_player : t -> t

  (**[update_player p] updates the coordinates of player p based on user
    input.*)
  val update_player : player -> player
end

(**Global obstacle speed variable*)
let obstacle_speed = 3

(*--------------------------------- LEVEL 1
  --------------------------------------------*)

module TestLevel : Level = struct
  let level =
    {
      start_pos_x = 100;
      start_pos_y = 300;
      obstacles =
        [
          {
            x_pos = 289;
            y_pos = 200;
            obstacle_type = "square up";
            count = 0;
          };
          {
            x_pos = 464;
            y_pos = 385;
            obstacle_type = "square down";
            count = 0;
          };
          {
            x_pos = 639;
            y_pos = 200;
            obstacle_type = "square up";
            count = 0;
          };
        ];
      collectables = [];
      finish_area = (800, 275, 850, 325);
      exit_open = true;
      p = { x_pos = 100; y_pos = 300; radius = 20 };
    }

  (**[draw_obstacles obs] draws all of the obstacles in obs to the graph
    in their current positions.*)
  let rec draw_obstacles (obstacles : obstacle list) =
    match obstacles with
    | [] -> ()
    | a :: b ->
        if
          a.obstacle_type = "square up"
          || a.obstacle_type = "square down"
        then (
          Graphics.set_color Graphics.magenta;
          Graphics.fill_rect a.x_pos a.y_pos 20 20;
          draw_obstacles b)
        else draw_obstacles b

  (**[draw_goal lvl] draws the goal in lvl to the graph.*)
  let draw_goal lvl =
    let x1 = match lvl.finish_area with a, _, _, _ -> a in
    let y1 = match lvl.finish_area with _, b, _, _ -> b in
    let x2 = match lvl.finish_area with _, _, c, _ -> c in
    let y2 = match lvl.finish_area with _, _, _, d -> d in
    Graphics.set_color Graphics.green;
    Graphics.draw_rect x1 y1 (x2 - x1) (y2 - y1)

  (**[draw_borders ()] draws the borders of this level to the graph*)
  let draw_borders () =
    Graphics.set_color Graphics.black;
    Graphics.moveto 50 250;
    Graphics.lineto 200 250;
    Graphics.lineto 200 190;
    Graphics.lineto 750 190;
    Graphics.lineto 750 250;
    Graphics.lineto 880 250;
    Graphics.lineto 880 350;
    Graphics.lineto 750 350;
    Graphics.lineto 750 410;
    Graphics.lineto 200 410;
    Graphics.lineto 200 350;
    Graphics.lineto 50 350;
    Graphics.lineto 50 250

  let initialize_game level =
    draw_goal level;
    Graphics.set_color Graphics.red;
    Graphics.fill_circle level.start_pos_x level.start_pos_y 20;
    draw_obstacles level.obstacles;
    draw_borders ()

  let update_player (player : player) =
    let key = Graphics.read_key () in
    let newX =
      if key = 'a' then player.x_pos - 20
      else if key = 'd' then player.x_pos + 20
      else player.x_pos
    in
    let newY =
      if key = 's' then player.y_pos - 20
      else if key = 'w' then player.y_pos + 20
      else player.y_pos
    in

    let newXnew =
      if key = 'a' || key = 'd' then
        if (*Corner Cases*)
           (newY = 270 || newY = 330) && newX < 70 then 70
        else if (newY = 270 || newY = 330) && newX > 860 then 860
          (*Edge Cases*)
        else if newY > 270 && newY < 330 && newX <= 70 then 70
        else if newY > 270 && newY < 330 && newX >= 860 then 860
        else if (newY > 330 || newY < 270) && newX <= 220 then 220
        else if (newY > 330 || newY < 270) && newX >= 730 then 730
        else newX
      else newX
    in

    let newYnew =
      if key = 's' || key = 'w' then
        if (*Corner Cases*)
           (newX = 220 || newX = 730) && newY > 390 then 390
        else if (newX = 220 || newX = 730) && newY < 210 then 210
          (*Edge cases*)
        else if (newX < 220 || newX > 730) && newY >= 330 then 330
        else if (newX < 220 || newX > 730) && newY <= 270 then 270
        else if newX > 220 && newX < 730 && newY >= 390 then 390
        else if newX > 220 && newX < 730 && newY <= 210 then 210
        else newY
      else newY
    in

    { x_pos = newXnew; y_pos = newYnew; radius = player.radius }

  (**[update_obstacles obs] updates the positions of all the obstacles in
    obs.*)
  let rec update_obstacles (obstacles : obstacle list) =
    match obstacles with
    | [] -> []
    | a :: b ->
        if a.obstacle_type = "square up" then
          let updated_y_1 = a.y_pos + obstacle_speed in
          let updated_y_2 =
            if updated_y_1 > 385 then 385 else updated_y_1
          in
          let updated_obstacle_type =
            if updated_y_1 > 385 then "square down" else "square up"
          in
          {
            x_pos = a.x_pos;
            y_pos = updated_y_2;
            obstacle_type = updated_obstacle_type;
            count = 0;
          }
          :: update_obstacles b
        else
          let updated_y_1 = a.y_pos - obstacle_speed in
          let updated_y_2 =
            if updated_y_1 < 200 then 200 else updated_y_1
          in
          let updated_obstacle_type =
            if updated_y_1 < 200 then "square up" else "square down"
          in
          {
            x_pos = a.x_pos;
            y_pos = updated_y_2;
            obstacle_type = updated_obstacle_type;
            count = 0;
          }
          :: update_obstacles b

  let update (lvl : t) (lives : int) =
    Unix.sleepf 0.01;
    Graphics.clear_graph ();
    draw_borders ();
    Graphics.set_color Graphics.black;
    Graphics.set_text_size 1000000000;
    Graphics.moveto 25 560;
    Graphics.draw_string ("Lives: " ^ string_of_int lives);
    Graphics.moveto 475 560;
    Graphics.draw_string "LEVEL 1";
    Graphics.moveto 325 450;
    Graphics.draw_string
      "Dodge obstacles and reach the exit to finish a level";
    Graphics.set_color Graphics.red;
    Graphics.fill_circle lvl.p.x_pos lvl.p.y_pos 20;
    draw_obstacles lvl.obstacles;
    draw_goal lvl;
    let updated_obstacles = update_obstacles lvl.obstacles in
    if Graphics.key_pressed () then
      let updated_player = update_player lvl.p in
      {
        start_pos_x = lvl.start_pos_x;
        start_pos_y = lvl.start_pos_y;
        obstacles = updated_obstacles;
        collectables = lvl.collectables;
        finish_area = lvl.finish_area;
        p = updated_player;
        exit_open = true;
      }
    else
      {
        start_pos_x = lvl.start_pos_x;
        start_pos_y = lvl.start_pos_y;
        obstacles = updated_obstacles;
        collectables = lvl.collectables;
        finish_area = lvl.finish_area;
        exit_open = true;
        p = lvl.p;
      }

  let check_collisions lvl =
    let rec check_collisions_rec (player : player) obstacles =
      match obstacles with
      | [] -> false
      | a :: b ->
          if
            player.x_pos > a.x_pos - 20
            && player.x_pos < a.x_pos + 40
            && player.y_pos > a.y_pos - 20
            && player.y_pos < a.y_pos + 40
          then true
          else check_collisions_rec player b
    in
    check_collisions_rec lvl.p lvl.obstacles

  let reset_player (lvl : t) =
    {
      start_pos_x = lvl.start_pos_x;
      start_pos_y = lvl.start_pos_y;
      obstacles = lvl.obstacles;
      collectables = lvl.collectables;
      finish_area = lvl.finish_area;
      exit_open = true;
      p =
        {
          x_pos = lvl.start_pos_x;
          y_pos = lvl.start_pos_y;
          radius = 20;
        };
    }

  (**Level1 has no collectables, so this returns the current level state
    as is.*)
  let check_collections (lvl : t) = lvl

  let check_finish (lvl : t) =
    let x1 = match lvl.finish_area with a, _, _, _ -> a in
    let y1 = match lvl.finish_area with _, b, _, _ -> b in
    let x2 = match lvl.finish_area with _, _, c, _ -> c in
    let y2 = match lvl.finish_area with _, _, _, d -> d in

    if
      lvl.p.x_pos >= x1 && lvl.p.x_pos <= x2 && lvl.p.y_pos >= y1
      && lvl.p.y_pos <= y2
    then true
    else false
end

(*--------------------------------- LEVEL 2
  --------------------------------------------*)
module Level2 : Level = struct
  let level =
    {
      start_pos_x = 100;
      start_pos_y = 300;
      obstacles =
        [
          {
            x_pos = 289;
            y_pos = 200;
            obstacle_type = "square up";
            count = 0;
          };
          {
            x_pos = 464;
            y_pos = 385;
            obstacle_type = "square down";
            count = 0;
          };
          {
            x_pos = 639;
            y_pos = 200;
            obstacle_type = "square up";
            count = 0;
          };
        ];
      collectables =
        [ { x_pos = 475; y_pos = 300; collectable_type = "KEY" } ];
      finish_area = (800, 275, 850, 325);
      p = { x_pos = 100; y_pos = 300; radius = 20 };
      exit_open = false;
    }

  let rec draw_collectables colls_lst =
    match colls_lst with
    | [] -> ()
    | a :: b ->
        if a.collectable_type = "KEY" then (
          Graphics.set_color Graphics.yellow;
          Graphics.fill_circle a.x_pos a.y_pos 10;
          draw_collectables b)
        else ()

  (**[draw_obstacles obs] draws all of the obstacles in obs to the graph
    in their current positions.*)
  let rec draw_obstacles (obstacles : obstacle list) =
    match obstacles with
    | [] -> ()
    | a :: b ->
        if
          a.obstacle_type = "square up"
          || a.obstacle_type = "square down"
        then (
          Graphics.set_color Graphics.magenta;
          Graphics.fill_rect a.x_pos a.y_pos 20 20;
          draw_obstacles b)
        else draw_obstacles b

  (**[draw_goal lvl] draws the goal in lvl to the graph.*)
  let draw_goal lvl =
    let x1 = match lvl.finish_area with a, _, _, _ -> a in
    let y1 = match lvl.finish_area with _, b, _, _ -> b in
    let x2 = match lvl.finish_area with _, _, c, _ -> c in
    let y2 = match lvl.finish_area with _, _, _, d -> d in
    if lvl.exit_open then Graphics.set_color Graphics.green
    else Graphics.set_color Graphics.red;

    Graphics.draw_rect x1 y1 (x2 - x1) (y2 - y1)

  (**[draw_borders ()] draws the borders of this level to the graph*)
  let draw_borders () =
    Graphics.set_color Graphics.black;
    Graphics.moveto 50 250;
    Graphics.lineto 200 250;
    Graphics.lineto 200 190;
    Graphics.lineto 750 190;
    Graphics.lineto 750 250;
    Graphics.lineto 880 250;
    Graphics.lineto 880 350;
    Graphics.lineto 750 350;
    Graphics.lineto 750 410;
    Graphics.lineto 200 410;
    Graphics.lineto 200 350;
    Graphics.lineto 50 350;
    Graphics.lineto 50 250

  let initialize_game level =
    draw_goal level;
    Graphics.set_color Graphics.red;
    Graphics.fill_circle level.start_pos_x level.start_pos_y 20;
    draw_obstacles level.obstacles;
    draw_borders ()

  let update_player (player : player) =
    let key = Graphics.read_key () in
    let newX =
      if key = 'a' then player.x_pos - 20
      else if key = 'd' then player.x_pos + 20
      else player.x_pos
    in
    let newY =
      if key = 's' then player.y_pos - 20
      else if key = 'w' then player.y_pos + 20
      else player.y_pos
    in

    let newXnew =
      if key = 'a' || key = 'd' then
        if (*Corner Cases*)
           (newY = 270 || newY = 330) && newX < 70 then 70
        else if (newY = 270 || newY = 330) && newX > 860 then 860
          (*Edge Cases*)
        else if newY > 270 && newY < 330 && newX <= 70 then 70
        else if newY > 270 && newY < 330 && newX >= 860 then 860
        else if (newY > 330 || newY < 270) && newX <= 220 then 220
        else if (newY > 330 || newY < 270) && newX >= 730 then 730
        else newX
      else newX
    in

    let newYnew =
      if key = 's' || key = 'w' then
        if (*Corner Cases*)
           (newX = 220 || newX = 730) && newY > 390 then 390
        else if (newX = 220 || newX = 730) && newY < 210 then 210
          (*Edge cases*)
        else if (newX < 220 || newX > 730) && newY >= 330 then 330
        else if (newX < 220 || newX > 730) && newY <= 270 then 270
        else if newX > 220 && newX < 730 && newY >= 390 then 390
        else if newX > 220 && newX < 730 && newY <= 210 then 210
        else newY
      else newY
    in

    { x_pos = newXnew; y_pos = newYnew; radius = player.radius }

  (**[update_obstacles obs] updates the positions of all the obstacles in
    obs.*)
  let rec update_obstacles (obstacles : obstacle list) =
    match obstacles with
    | [] -> []
    | a :: b ->
        if a.obstacle_type = "square up" then
          let updated_y_1 = a.y_pos + obstacle_speed in
          let updated_y_2 =
            if updated_y_1 > 385 then 385 else updated_y_1
          in
          let updated_obstacle_type =
            if updated_y_1 > 385 then "square down" else "square up"
          in
          {
            x_pos = a.x_pos;
            y_pos = updated_y_2;
            obstacle_type = updated_obstacle_type;
            count = 0;
          }
          :: update_obstacles b
        else
          let updated_y_1 = a.y_pos - obstacle_speed in
          let updated_y_2 =
            if updated_y_1 < 200 then 200 else updated_y_1
          in
          let updated_obstacle_type =
            if updated_y_1 < 200 then "square up" else "square down"
          in
          {
            x_pos = a.x_pos;
            y_pos = updated_y_2;
            obstacle_type = updated_obstacle_type;
            count = 0;
          }
          :: update_obstacles b

  let update (lvl : t) (lives : int) =
    Unix.sleepf 0.01;
    Graphics.clear_graph ();
    draw_borders ();
    Graphics.set_color Graphics.black;
    Graphics.set_text_size 1000000000;
    Graphics.moveto 25 560;
    Graphics.draw_string ("Lives: " ^ string_of_int lives);
    Graphics.moveto 475 560;
    Graphics.draw_string "LEVEL 2";
    draw_collectables lvl.collectables;
    Graphics.set_color Graphics.red;
    Graphics.fill_circle lvl.p.x_pos lvl.p.y_pos 20;
    draw_obstacles lvl.obstacles;
    draw_goal lvl;
    Graphics.moveto 375 450;
    Graphics.draw_string "Collect keys to access locked exits";
    let updated_obstacles = update_obstacles lvl.obstacles in
    if Graphics.key_pressed () then
      let updated_player = update_player lvl.p in
      {
        start_pos_x = lvl.start_pos_x;
        start_pos_y = lvl.start_pos_y;
        obstacles = updated_obstacles;
        collectables = lvl.collectables;
        finish_area = lvl.finish_area;
        p = updated_player;
        exit_open = lvl.exit_open;
      }
    else
      {
        start_pos_x = lvl.start_pos_x;
        start_pos_y = lvl.start_pos_y;
        obstacles = updated_obstacles;
        collectables = lvl.collectables;
        finish_area = lvl.finish_area;
        p = lvl.p;
        exit_open = lvl.exit_open;
      }

  let check_collisions lvl =
    let rec check_collisions_rec (player : player) obstacles =
      match obstacles with
      | [] -> false
      | a :: b ->
          if
            player.x_pos > a.x_pos - 20
            && player.x_pos < a.x_pos + 40
            && player.y_pos > a.y_pos - 20
            && player.y_pos < a.y_pos + 40
          then true
          else check_collisions_rec player b
    in
    check_collisions_rec lvl.p lvl.obstacles

  let reset_player (lvl : t) =
    {
      start_pos_x = lvl.start_pos_x;
      start_pos_y = lvl.start_pos_y;
      obstacles = lvl.obstacles;
      collectables = lvl.collectables;
      finish_area = lvl.finish_area;
      exit_open = lvl.exit_open;
      p =
        {
          x_pos = lvl.start_pos_x;
          y_pos = lvl.start_pos_y;
          radius = 20;
        };
    }

  let check_collections (lvl : t) =
    let key_collected = ref false in
    key_collected := false;
    if lvl.exit_open then key_collected := true else ();
    let rec check_collections_rec colls_lst =
      match colls_lst with
      | [] -> []
      | a :: b ->
          if a.collectable_type = "KEY" then
            if
              lvl.p.x_pos > a.x_pos - 30
              && lvl.p.x_pos < a.x_pos + 30
              && lvl.p.y_pos > a.y_pos - 30
              && lvl.p.y_pos < a.y_pos + 30
            then (
              key_collected := true;
              check_collections_rec b)
            else a :: check_collections_rec b
          else check_collections_rec b
    in
    let new_collectables_lst = check_collections_rec lvl.collectables in
    {
      start_pos_x = lvl.start_pos_x;
      start_pos_y = lvl.start_pos_y;
      obstacles = lvl.obstacles;
      collectables = new_collectables_lst;
      finish_area = lvl.finish_area;
      exit_open = !key_collected;
      p = lvl.p;
    }

  let check_finish (lvl : t) =
    let x1 = match lvl.finish_area with a, _, _, _ -> a in
    let y1 = match lvl.finish_area with _, b, _, _ -> b in
    let x2 = match lvl.finish_area with _, _, c, _ -> c in
    let y2 = match lvl.finish_area with _, _, _, d -> d in

    if
      lvl.p.x_pos >= x1 && lvl.p.x_pos <= x2 && lvl.p.y_pos >= y1
      && lvl.p.y_pos <= y2 && lvl.exit_open
    then true
    else false
end

(*--------------------------------- LEVEL 3
  --------------------------------------------*)

module Level3 : Level = struct
  let level =
    {
      start_pos_x = 100;
      start_pos_y = 300;
      obstacles =
        [
          {
            x_pos = 289;
            y_pos = 200;
            obstacle_type = "square up";
            count = 0;
          };
          {
            x_pos = 375;
            y_pos = 200;
            obstacle_type = "square up";
            count = 0;
          };
          {
            x_pos = 464;
            y_pos = 385;
            obstacle_type = "square down";
            count = 0;
          };
          {
            x_pos = 550;
            y_pos = 200;
            obstacle_type = "square up";
            count = 0;
          };
          {
            x_pos = 639;
            y_pos = 200;
            obstacle_type = "square up";
            count = 0;
          };
        ];
      collectables = [];
      finish_area = (800, 275, 850, 325);
      exit_open = true;
      p = { x_pos = 100; y_pos = 300; radius = 20 };
    }

  (**[draw_obstacles obs] draws all of the obstacles in obs to the graph
    in their current positions.*)
  let rec draw_obstacles (obstacles : obstacle list) =
    match obstacles with
    | [] -> ()
    | a :: b ->
        if
          a.obstacle_type = "square up"
          || a.obstacle_type = "square down"
        then (
          Graphics.set_color Graphics.magenta;
          Graphics.fill_rect a.x_pos a.y_pos 20 20;
          draw_obstacles b)
        else draw_obstacles b

  (**[draw_goal lvl] draws the goal in lvl to the graph.*)
  let draw_goal lvl =
    let x1 = match lvl.finish_area with a, _, _, _ -> a in
    let y1 = match lvl.finish_area with _, b, _, _ -> b in
    let x2 = match lvl.finish_area with _, _, c, _ -> c in
    let y2 = match lvl.finish_area with _, _, _, d -> d in
    Graphics.set_color Graphics.green;
    Graphics.draw_rect x1 y1 (x2 - x1) (y2 - y1)

  (**[draw_borders ()] draws the borders of this level to the graph*)
  let draw_borders () =
    Graphics.set_color Graphics.black;
    Graphics.moveto 50 250;
    Graphics.lineto 200 250;
    Graphics.lineto 200 190;
    Graphics.lineto 750 190;
    Graphics.lineto 750 250;
    Graphics.lineto 880 250;
    Graphics.lineto 880 350;
    Graphics.lineto 750 350;
    Graphics.lineto 750 410;
    Graphics.lineto 200 410;
    Graphics.lineto 200 350;
    Graphics.lineto 50 350;
    Graphics.lineto 50 250

  let initialize_game level =
    draw_goal level;
    Graphics.set_color Graphics.red;
    Graphics.fill_circle level.start_pos_x level.start_pos_y 20;
    draw_obstacles level.obstacles;
    draw_borders ()

  let update_player (player : player) =
    let key = Graphics.read_key () in
    let newX =
      if key = 'a' then player.x_pos - 20
      else if key = 'd' then player.x_pos + 20
      else player.x_pos
    in
    let newY =
      if key = 's' then player.y_pos - 20
      else if key = 'w' then player.y_pos + 20
      else player.y_pos
    in

    let newXnew =
      if key = 'a' || key = 'd' then
        if (*Corner Cases*)
           (newY = 270 || newY = 330) && newX < 70 then 70
        else if (newY = 270 || newY = 330) && newX > 860 then 860
          (*Edge Cases*)
        else if newY > 270 && newY < 330 && newX <= 70 then 70
        else if newY > 270 && newY < 330 && newX >= 860 then 860
        else if (newY > 330 || newY < 270) && newX <= 220 then 220
        else if (newY > 330 || newY < 270) && newX >= 730 then 730
        else newX
      else newX
    in

    let newYnew =
      if key = 's' || key = 'w' then
        if (*Corner Cases*)
           (newX = 220 || newX = 730) && newY > 390 then 390
        else if (newX = 220 || newX = 730) && newY < 210 then 210
          (*Edge cases*)
        else if (newX < 220 || newX > 730) && newY >= 330 then 330
        else if (newX < 220 || newX > 730) && newY <= 270 then 270
        else if newX > 220 && newX < 730 && newY >= 390 then 390
        else if newX > 220 && newX < 730 && newY <= 210 then 210
        else newY
      else newY
    in

    { x_pos = newXnew; y_pos = newYnew; radius = player.radius }

  (**[update_obstacles obs] updates the positions of all the obstacles in
    obs.*)
  let rec update_obstacles (obstacles : obstacle list) =
    match obstacles with
    | [] -> []
    | a :: b ->
        if a.obstacle_type = "square up" then
          let updated_y_1 = a.y_pos + obstacle_speed in
          let updated_y_2 =
            if updated_y_1 > 385 then 385 else updated_y_1
          in
          let updated_obstacle_type =
            if updated_y_1 > 385 then "square down" else "square up"
          in
          {
            x_pos = a.x_pos;
            y_pos = updated_y_2;
            obstacle_type = updated_obstacle_type;
            count = 0;
          }
          :: update_obstacles b
        else
          let updated_y_1 = a.y_pos - obstacle_speed in
          let updated_y_2 =
            if updated_y_1 < 200 then 200 else updated_y_1
          in
          let updated_obstacle_type =
            if updated_y_1 < 200 then "square up" else "square down"
          in
          {
            x_pos = a.x_pos;
            y_pos = updated_y_2;
            obstacle_type = updated_obstacle_type;
            count = 0;
          }
          :: update_obstacles b

  let update (lvl : t) (lives : int) =
    Unix.sleepf 0.01;
    Graphics.clear_graph ();
    draw_borders ();
    Graphics.set_color Graphics.black;
    Graphics.set_text_size 1000000000;
    Graphics.moveto 25 560;
    Graphics.draw_string ("Lives: " ^ string_of_int lives);
    Graphics.moveto 475 560;
    Graphics.draw_string "LEVEL 3";
    Graphics.moveto 450 450;
    Graphics.draw_string "Good Luck! :)";
    Graphics.set_color Graphics.red;
    Graphics.fill_circle lvl.p.x_pos lvl.p.y_pos 20;
    draw_obstacles lvl.obstacles;
    draw_goal lvl;
    let updated_obstacles = update_obstacles lvl.obstacles in
    if Graphics.key_pressed () then
      let updated_player = update_player lvl.p in
      {
        start_pos_x = lvl.start_pos_x;
        start_pos_y = lvl.start_pos_y;
        obstacles = updated_obstacles;
        collectables = lvl.collectables;
        finish_area = lvl.finish_area;
        p = updated_player;
        exit_open = true;
      }
    else
      {
        start_pos_x = lvl.start_pos_x;
        start_pos_y = lvl.start_pos_y;
        obstacles = updated_obstacles;
        collectables = lvl.collectables;
        finish_area = lvl.finish_area;
        exit_open = true;
        p = lvl.p;
      }

  let check_collisions lvl =
    let rec check_collisions_rec (player : player) obstacles =
      match obstacles with
      | [] -> false
      | a :: b ->
          if
            player.x_pos > a.x_pos - 20
            && player.x_pos < a.x_pos + 40
            && player.y_pos > a.y_pos - 20
            && player.y_pos < a.y_pos + 40
          then true
          else check_collisions_rec player b
    in
    check_collisions_rec lvl.p lvl.obstacles

  let reset_player (lvl : t) =
    {
      start_pos_x = lvl.start_pos_x;
      start_pos_y = lvl.start_pos_y;
      obstacles = lvl.obstacles;
      collectables = lvl.collectables;
      finish_area = lvl.finish_area;
      exit_open = true;
      p =
        {
          x_pos = lvl.start_pos_x;
          y_pos = lvl.start_pos_y;
          radius = 20;
        };
    }

  (**Level 3 has no collectables, so this returns the current level state
    as is.*)
  let check_collections (lvl : t) = lvl

  let check_finish (lvl : t) =
    let x1 = match lvl.finish_area with a, _, _, _ -> a in
    let y1 = match lvl.finish_area with _, b, _, _ -> b in
    let x2 = match lvl.finish_area with _, _, c, _ -> c in
    let y2 = match lvl.finish_area with _, _, _, d -> d in

    if
      lvl.p.x_pos >= x1 && lvl.p.x_pos <= x2 && lvl.p.y_pos >= y1
      && lvl.p.y_pos <= y2
    then true
    else false
end

(*--------------------------------- LEVEL 4
  --------------------------------------------*)
module Level4 : Level = struct
  let level =
    {
      start_pos_x = 100;
      start_pos_y = 300;
      obstacles =
        [
          {
            x_pos = 289;
            y_pos = 385;
            obstacle_type = "square up";
            count = 0;
          };
          {
            x_pos = 375;
            y_pos = 350;
            obstacle_type = "square up";
            count = 0;
          };
          {
            x_pos = 464;
            y_pos = 300;
            obstacle_type = "square up";
            count = 0;
          };
          {
            x_pos = 550;
            y_pos = 250;
            obstacle_type = "square up";
            count = 0;
          };
          {
            x_pos = 639;
            y_pos = 200;
            obstacle_type = "square up";
            count = 0;
          };
        ];
      collectables =
        [ { x_pos = 475; y_pos = 300; collectable_type = "KEY" } ];
      finish_area = (800, 275, 850, 325);
      p = { x_pos = 100; y_pos = 300; radius = 20 };
      exit_open = false;
    }

  let rec draw_collectables colls_lst =
    match colls_lst with
    | [] -> ()
    | a :: b ->
        if a.collectable_type = "KEY" then (
          Graphics.set_color Graphics.yellow;
          Graphics.fill_circle a.x_pos a.y_pos 10;
          draw_collectables b)
        else ()

  (**[draw_obstacles obs] draws all of the obstacles in obs to the graph
    in their current positions.*)
  let rec draw_obstacles (obstacles : obstacle list) =
    match obstacles with
    | [] -> ()
    | a :: b ->
        if
          a.obstacle_type = "square up"
          || a.obstacle_type = "square down"
        then (
          Graphics.set_color Graphics.magenta;
          Graphics.fill_rect a.x_pos a.y_pos 20 20;
          draw_obstacles b)
        else draw_obstacles b

  (**[draw_goal lvl] draws the goal in lvl to the graph.*)
  let draw_goal lvl =
    let x1 = match lvl.finish_area with a, _, _, _ -> a in
    let y1 = match lvl.finish_area with _, b, _, _ -> b in
    let x2 = match lvl.finish_area with _, _, c, _ -> c in
    let y2 = match lvl.finish_area with _, _, _, d -> d in
    if lvl.exit_open then Graphics.set_color Graphics.green
    else Graphics.set_color Graphics.red;

    Graphics.draw_rect x1 y1 (x2 - x1) (y2 - y1)

  (**[draw_borders ()] draws the borders of this level to the graph*)
  let draw_borders () =
    Graphics.set_color Graphics.black;
    Graphics.moveto 50 250;
    Graphics.lineto 200 250;
    Graphics.lineto 200 190;
    Graphics.lineto 750 190;
    Graphics.lineto 750 250;
    Graphics.lineto 880 250;
    Graphics.lineto 880 350;
    Graphics.lineto 750 350;
    Graphics.lineto 750 410;
    Graphics.lineto 200 410;
    Graphics.lineto 200 350;
    Graphics.lineto 50 350;
    Graphics.lineto 50 250

  let initialize_game level =
    draw_goal level;
    Graphics.set_color Graphics.red;
    Graphics.fill_circle level.start_pos_x level.start_pos_y 20;
    draw_obstacles level.obstacles;
    draw_borders ()

  let update_player (player : player) =
    let key = Graphics.read_key () in
    let newX =
      if key = 'a' then player.x_pos - 20
      else if key = 'd' then player.x_pos + 20
      else player.x_pos
    in
    let newY =
      if key = 's' then player.y_pos - 20
      else if key = 'w' then player.y_pos + 20
      else player.y_pos
    in

    let newXnew =
      if key = 'a' || key = 'd' then
        if (*Corner Cases*)
           (newY = 270 || newY = 330) && newX < 70 then 70
        else if (newY = 270 || newY = 330) && newX > 860 then 860
          (*Edge Cases*)
        else if newY > 270 && newY < 330 && newX <= 70 then 70
        else if newY > 270 && newY < 330 && newX >= 860 then 860
        else if (newY > 330 || newY < 270) && newX <= 220 then 220
        else if (newY > 330 || newY < 270) && newX >= 730 then 730
        else newX
      else newX
    in

    let newYnew =
      if key = 's' || key = 'w' then
        if (*Corner Cases*)
           (newX = 220 || newX = 730) && newY > 390 then 390
        else if (newX = 220 || newX = 730) && newY < 210 then 210
          (*Edge cases*)
        else if (newX < 220 || newX > 730) && newY >= 330 then 330
        else if (newX < 220 || newX > 730) && newY <= 270 then 270
        else if newX > 220 && newX < 730 && newY >= 390 then 390
        else if newX > 220 && newX < 730 && newY <= 210 then 210
        else newY
      else newY
    in

    { x_pos = newXnew; y_pos = newYnew; radius = player.radius }

  (**[update_obstacles obs] updates the positions of all the obstacles in
    obs.*)
  let rec update_obstacles (obstacles : obstacle list) =
    match obstacles with
    | [] -> []
    | a :: b ->
        if a.obstacle_type = "square up" then
          let updated_y_1 = a.y_pos + obstacle_speed in
          let updated_y_2 =
            if updated_y_1 > 385 then 385 else updated_y_1
          in
          let updated_obstacle_type =
            if updated_y_1 > 385 then "square down" else "square up"
          in
          {
            x_pos = a.x_pos;
            y_pos = updated_y_2;
            obstacle_type = updated_obstacle_type;
            count = 0;
          }
          :: update_obstacles b
        else
          let updated_y_1 = a.y_pos - obstacle_speed in
          let updated_y_2 =
            if updated_y_1 < 200 then 200 else updated_y_1
          in
          let updated_obstacle_type =
            if updated_y_1 < 200 then "square up" else "square down"
          in
          {
            x_pos = a.x_pos;
            y_pos = updated_y_2;
            obstacle_type = updated_obstacle_type;
            count = 0;
          }
          :: update_obstacles b

  let update (lvl : t) (lives : int) =
    Unix.sleepf 0.01;
    Graphics.clear_graph ();
    draw_borders ();
    Graphics.set_color Graphics.black;
    Graphics.set_text_size 1000000000;
    Graphics.moveto 25 560;
    Graphics.draw_string ("Lives: " ^ string_of_int lives);
    Graphics.moveto 475 560;
    Graphics.draw_string "LEVEL 4";
    draw_collectables lvl.collectables;
    Graphics.set_color Graphics.red;
    Graphics.fill_circle lvl.p.x_pos lvl.p.y_pos 20;
    draw_obstacles lvl.obstacles;
    draw_goal lvl;
    let updated_obstacles = update_obstacles lvl.obstacles in
    if Graphics.key_pressed () then
      let updated_player = update_player lvl.p in
      {
        start_pos_x = lvl.start_pos_x;
        start_pos_y = lvl.start_pos_y;
        obstacles = updated_obstacles;
        collectables = lvl.collectables;
        finish_area = lvl.finish_area;
        p = updated_player;
        exit_open = lvl.exit_open;
      }
    else
      {
        start_pos_x = lvl.start_pos_x;
        start_pos_y = lvl.start_pos_y;
        obstacles = updated_obstacles;
        collectables = lvl.collectables;
        finish_area = lvl.finish_area;
        p = lvl.p;
        exit_open = lvl.exit_open;
      }

  let check_collisions lvl =
    let rec check_collisions_rec (player : player) obstacles =
      match obstacles with
      | [] -> false
      | a :: b ->
          if
            player.x_pos > a.x_pos - 20
            && player.x_pos < a.x_pos + 40
            && player.y_pos > a.y_pos - 20
            && player.y_pos < a.y_pos + 40
          then true
          else check_collisions_rec player b
    in
    check_collisions_rec lvl.p lvl.obstacles

  let reset_player (lvl : t) =
    {
      start_pos_x = lvl.start_pos_x;
      start_pos_y = lvl.start_pos_y;
      obstacles = lvl.obstacles;
      collectables = lvl.collectables;
      finish_area = lvl.finish_area;
      exit_open = lvl.exit_open;
      p =
        {
          x_pos = lvl.start_pos_x;
          y_pos = lvl.start_pos_y;
          radius = 20;
        };
    }

  let check_collections (lvl : t) =
    let key_collected = ref false in
    key_collected := false;
    if lvl.exit_open then key_collected := true else ();
    let rec check_collections_rec colls_lst =
      match colls_lst with
      | [] -> []
      | a :: b ->
          if a.collectable_type = "KEY" then
            if
              lvl.p.x_pos > a.x_pos - 30
              && lvl.p.x_pos < a.x_pos + 30
              && lvl.p.y_pos > a.y_pos - 30
              && lvl.p.y_pos < a.y_pos + 30
            then (
              key_collected := true;
              check_collections_rec b)
            else a :: check_collections_rec b
          else check_collections_rec b
    in
    let new_collectables_lst = check_collections_rec lvl.collectables in
    {
      start_pos_x = lvl.start_pos_x;
      start_pos_y = lvl.start_pos_y;
      obstacles = lvl.obstacles;
      collectables = new_collectables_lst;
      finish_area = lvl.finish_area;
      exit_open = !key_collected;
      p = lvl.p;
    }

  let check_finish (lvl : t) =
    let x1 = match lvl.finish_area with a, _, _, _ -> a in
    let y1 = match lvl.finish_area with _, b, _, _ -> b in
    let x2 = match lvl.finish_area with _, _, c, _ -> c in
    let y2 = match lvl.finish_area with _, _, _, d -> d in

    if
      lvl.p.x_pos >= x1 && lvl.p.x_pos <= x2 && lvl.p.y_pos >= y1
      && lvl.p.y_pos <= y2 && lvl.exit_open
    then true
    else false
end

(*--------------------------------- LEVEL 5
  --------------------------------------------*)
module Level5 : Level = struct
  let on_left = ref true

  let level =
    {
      start_pos_x = 100;
      start_pos_y = 300;
      obstacles =
        [
          {
            x_pos = 249;
            y_pos = 200;
            obstacle_type = "square up";
            count = 0;
          };
          {
            x_pos = 335;
            y_pos = 200;
            obstacle_type = "square up";
            count = 0;
          };
          {
            x_pos = 590;
            y_pos = 200;
            obstacle_type = "square up";
            count = 0;
          };
          {
            x_pos = 679;
            y_pos = 200;
            obstacle_type = "square up";
            count = 0;
          };
        ];
      collectables =
        [
          {
            x_pos = 435;
            y_pos = 285;
            collectable_type = "id1 open_portal";
          };
          {
            x_pos = 490;
            y_pos = 285;
            collectable_type = "id2 portal_target";
          };
        ];
      finish_area = (800, 275, 850, 325);
      exit_open = true;
      p = { x_pos = 100; y_pos = 300; radius = 20 };
    }

  let rec draw_collectables collectables =
    match collectables with
    | [] -> ()
    | h :: t ->
        if
          h.collectable_type = "id1 open_portal"
          || h.collectable_type = "id2 open_portal"
        then (
          Graphics.set_color Graphics.blue;
          Graphics.draw_rect h.x_pos h.y_pos 35 35;
          draw_collectables t)
        else if
          h.collectable_type = "id1 portal_target"
          || h.collectable_type = "id2 portal_target"
        then (
          Graphics.set_color Graphics.blue;
          Graphics.fill_rect h.x_pos h.y_pos 35 35;
          draw_collectables t)
        else draw_collectables t

  (**[draw_obstacles obs] draws all of the obstacles in obs to the graph
    in their current positions.*)
  let rec draw_obstacles (obstacles : obstacle list) =
    match obstacles with
    | [] -> ()
    | a :: b ->
        if
          a.obstacle_type = "square up"
          || a.obstacle_type = "square down"
        then (
          Graphics.set_color Graphics.magenta;
          Graphics.fill_rect a.x_pos a.y_pos 20 20;
          draw_obstacles b)
        else draw_obstacles b

  (**[draw_goal lvl] draws the goal in lvl to the graph.*)
  let draw_goal lvl =
    let x1 = match lvl.finish_area with a, _, _, _ -> a in
    let y1 = match lvl.finish_area with _, b, _, _ -> b in
    let x2 = match lvl.finish_area with _, _, c, _ -> c in
    let y2 = match lvl.finish_area with _, _, _, d -> d in
    Graphics.set_color Graphics.green;
    Graphics.draw_rect x1 y1 (x2 - x1) (y2 - y1)

  (**[draw_borders ()] draws the borders of this level to the graph*)
  let draw_borders () =
    Graphics.set_color Graphics.black;
    Graphics.moveto 50 250;
    Graphics.lineto 200 250;
    Graphics.lineto 200 190;
    Graphics.lineto 750 190;
    Graphics.lineto 750 250;
    Graphics.lineto 880 250;
    Graphics.lineto 880 350;
    Graphics.lineto 750 350;
    Graphics.lineto 750 410;
    Graphics.lineto 200 410;
    Graphics.lineto 200 350;
    Graphics.lineto 50 350;
    Graphics.lineto 50 250;
    Graphics.moveto 480 410;
    Graphics.lineto 480 190

  let initialize_game level =
    on_left := true;
    draw_goal level;
    Graphics.set_color Graphics.red;
    Graphics.fill_circle level.start_pos_x level.start_pos_y 20;
    draw_obstacles level.obstacles;
    draw_collectables level.collectables;
    draw_borders ()

  let update_player (player : player) =
    let key = Graphics.read_key () in
    let newX =
      if key = 'a' then player.x_pos - 20
      else if key = 'd' then player.x_pos + 20
      else player.x_pos
    in
    let newY =
      if key = 's' then player.y_pos - 20
      else if key = 'w' then player.y_pos + 20
      else player.y_pos
    in

    let newXnew =
      if key = 'a' || key = 'd' then
        if (*Corner Cases*)
           (newY = 270 || newY = 330) && newX < 70 then 70
        else if (newY = 270 || newY = 330) && newX > 860 then 860
          (*Edge Cases*)
        else if newY > 270 && newY < 330 && newX <= 70 then 70
        else if newY > 270 && newY < 330 && newX >= 860 then 860
        else if (newY > 330 || newY < 270) && newX <= 220 then 220
        else if (newY > 330 || newY < 270) && newX >= 730 then 730
        else newX
      else newX
    in

    let newYnew =
      if key = 's' || key = 'w' then
        if (*Corner Cases*)
           (newX = 220 || newX = 730) && newY > 390 then 390
        else if (newX = 220 || newX = 730) && newY < 210 then 210
          (*Edge cases*)
        else if (newX < 220 || newX > 730) && newY >= 330 then 330
        else if (newX < 220 || newX > 730) && newY <= 270 then 270
        else if newX > 220 && newX < 730 && newY >= 390 then 390
        else if newX > 220 && newX < 730 && newY <= 210 then 210
        else newY
      else newY
    in
    let newXnew_new =
      if !on_left && newXnew > 460 then 460
      else if (not !on_left) && newXnew < 500 then 500
      else newXnew
    in

    { x_pos = newXnew_new; y_pos = newYnew; radius = player.radius }

  (**[update_obstacles obs] updates the positions of all the obstacles in
    obs.*)
  let rec update_obstacles (obstacles : obstacle list) =
    match obstacles with
    | [] -> []
    | a :: b ->
        if a.obstacle_type = "square up" then
          let updated_y_1 = a.y_pos + obstacle_speed in
          let updated_y_2 =
            if updated_y_1 > 385 then 385 else updated_y_1
          in
          let updated_obstacle_type =
            if updated_y_1 > 385 then "square down" else "square up"
          in
          {
            x_pos = a.x_pos;
            y_pos = updated_y_2;
            obstacle_type = updated_obstacle_type;
            count = 0;
          }
          :: update_obstacles b
        else
          let updated_y_1 = a.y_pos - obstacle_speed in
          let updated_y_2 =
            if updated_y_1 < 200 then 200 else updated_y_1
          in
          let updated_obstacle_type =
            if updated_y_1 < 200 then "square up" else "square down"
          in
          {
            x_pos = a.x_pos;
            y_pos = updated_y_2;
            obstacle_type = updated_obstacle_type;
            count = 0;
          }
          :: update_obstacles b

  let update (lvl : t) (lives : int) =
    Unix.sleepf 0.01;
    Graphics.clear_graph ();
    draw_borders ();
    Graphics.set_color Graphics.black;
    Graphics.set_text_size 1000000000;
    Graphics.moveto 25 560;
    Graphics.draw_string ("Lives: " ^ string_of_int lives);
    Graphics.moveto 475 560;
    Graphics.draw_string "LEVEL 5";
    Graphics.moveto 385 450;
    Graphics.draw_string "Portals allow you to teleport :o";
    Graphics.set_color Graphics.red;
    Graphics.fill_circle lvl.p.x_pos lvl.p.y_pos 20;
    draw_obstacles lvl.obstacles;
    draw_collectables lvl.collectables;
    draw_goal lvl;
    let updated_obstacles = update_obstacles lvl.obstacles in
    if Graphics.key_pressed () then
      let updated_player = update_player lvl.p in
      {
        start_pos_x = lvl.start_pos_x;
        start_pos_y = lvl.start_pos_y;
        obstacles = updated_obstacles;
        collectables = lvl.collectables;
        finish_area = lvl.finish_area;
        p = updated_player;
        exit_open = true;
      }
    else
      {
        start_pos_x = lvl.start_pos_x;
        start_pos_y = lvl.start_pos_y;
        obstacles = updated_obstacles;
        collectables = lvl.collectables;
        finish_area = lvl.finish_area;
        exit_open = true;
        p = lvl.p;
      }

  let check_collisions lvl =
    let rec check_collisions_rec (player : player) obstacles =
      match obstacles with
      | [] -> false
      | a :: b ->
          if
            player.x_pos > a.x_pos - 20
            && player.x_pos < a.x_pos + 40
            && player.y_pos > a.y_pos - 20
            && player.y_pos < a.y_pos + 40
          then true
          else check_collisions_rec player b
    in
    check_collisions_rec lvl.p lvl.obstacles

  let reset_player (lvl : t) =
    on_left := true;
    {
      start_pos_x = lvl.start_pos_x;
      start_pos_y = lvl.start_pos_y;
      obstacles = lvl.obstacles;
      collectables =
        [
          {
            x_pos = 435;
            y_pos = 285;
            collectable_type = "id1 open_portal";
          };
          {
            x_pos = 490;
            y_pos = 285;
            collectable_type = "id2 portal_target";
          };
        ];
      finish_area = lvl.finish_area;
      exit_open = true;
      p =
        {
          x_pos = lvl.start_pos_x;
          y_pos = lvl.start_pos_y;
          radius = 20;
        };
    }

  let check_collections (lvl : t) =
    let rec check_collisions_rec
        (player : player)
        (collectables : collectable list) =
      match collectables with
      | [] -> lvl
      | a :: b ->
          if
            player.x_pos > a.x_pos - 20
            && player.x_pos < a.x_pos + 55
            && player.y_pos > a.y_pos - 20
            && player.y_pos < a.y_pos + 55
          then
            if a.collectable_type = "id1 open_portal" then (
              on_left := false;
              {
                start_pos_x = lvl.start_pos_x;
                start_pos_y = lvl.start_pos_y;
                obstacles = lvl.obstacles;
                collectables =
                  [
                    {
                      x_pos = 435;
                      y_pos = 285;
                      collectable_type = "id1 portal_target";
                    };
                    {
                      x_pos = 490;
                      y_pos = 285;
                      collectable_type = "id2 open_portal";
                    };
                  ];
                finish_area = lvl.finish_area;
                exit_open = lvl.exit_open;
                p = { x_pos = 554; y_pos = 300; radius = lvl.p.radius };
              })
            else if a.collectable_type = "id2 open_portal" then (
              on_left := true;
              {
                start_pos_x = lvl.start_pos_x;
                start_pos_y = lvl.start_pos_y;
                obstacles = lvl.obstacles;
                collectables =
                  [
                    {
                      x_pos = 435;
                      y_pos = 285;
                      collectable_type = "id1 open_portal";
                    };
                    {
                      x_pos = 490;
                      y_pos = 285;
                      collectable_type = "id2 portal_target";
                    };
                  ];
                finish_area = lvl.finish_area;
                exit_open = lvl.exit_open;
                p = { x_pos = 400; y_pos = 300; radius = lvl.p.radius };
              })
            else lvl
          else check_collisions_rec player b
    in
    check_collisions_rec lvl.p lvl.collectables

  let check_finish (lvl : t) =
    let x1 = match lvl.finish_area with a, _, _, _ -> a in
    let y1 = match lvl.finish_area with _, b, _, _ -> b in
    let x2 = match lvl.finish_area with _, _, c, _ -> c in
    let y2 = match lvl.finish_area with _, _, _, d -> d in

    if
      lvl.p.x_pos >= x1 && lvl.p.x_pos <= x2 && lvl.p.y_pos >= y1
      && lvl.p.y_pos <= y2
    then true
    else false
end

(*--------------------------------- LEVEL 6
  --------------------------------------------*)

module Level6 : Level = struct
  let level =
    {
      start_pos_x = 100;
      start_pos_y = 300;
      obstacles =
        [
          {
            x_pos = 280;
            y_pos = 180;
            obstacle_type = "turret up";
            count = 100;
          };
          {
            x_pos = 460;
            y_pos = 400;
            obstacle_type = "turret down";
            count = 100;
          };
          {
            x_pos = 640;
            y_pos = 180;
            obstacle_type = "turret up";
            count = 100;
          };
        ];
      collectables = [];
      finish_area = (800, 275, 850, 325);
      exit_open = true;
      p = { x_pos = 100; y_pos = 300; radius = 20 };
    }

  (**[draw_obstacles obs] draws all of the obstacles in obs to the graph
    in their current positions.*)
  let rec draw_obstacles (obstacles : obstacle list) =
    match obstacles with
    | [] -> ()
    | a :: b ->
        if
          a.obstacle_type = "turret up"
          || a.obstacle_type = "turret down"
        then (
          Graphics.set_color Graphics.blue;
          Graphics.fill_rect a.x_pos a.y_pos 20 20;
          draw_obstacles b)
        else if
          a.obstacle_type = "projectile up"
          || a.obstacle_type = "projectile down"
        then (
          Graphics.set_color Graphics.cyan;
          Graphics.fill_rect a.x_pos a.y_pos 8 20;
          draw_obstacles b)
        else draw_obstacles b

  (**[draw_goal lvl] draws the goal in lvl to the graph.*)
  let draw_goal lvl =
    let x1 = match lvl.finish_area with a, _, _, _ -> a in
    let y1 = match lvl.finish_area with _, b, _, _ -> b in
    let x2 = match lvl.finish_area with _, _, c, _ -> c in
    let y2 = match lvl.finish_area with _, _, _, d -> d in
    Graphics.set_color Graphics.green;
    Graphics.draw_rect x1 y1 (x2 - x1) (y2 - y1)

  (**[draw_borders ()] draws the borders of this level to the graph*)
  let draw_borders () =
    Graphics.set_color Graphics.black;
    Graphics.moveto 50 250;
    Graphics.lineto 200 250;
    Graphics.lineto 200 190;
    Graphics.lineto 750 190;
    Graphics.lineto 750 250;
    Graphics.lineto 880 250;
    Graphics.lineto 880 350;
    Graphics.lineto 750 350;
    Graphics.lineto 750 410;
    Graphics.lineto 200 410;
    Graphics.lineto 200 350;
    Graphics.lineto 50 350;
    Graphics.lineto 50 250

  let initialize_game level =
    draw_goal level;
    Graphics.set_color Graphics.red;
    Graphics.fill_circle level.start_pos_x level.start_pos_y 20;
    (*draw_obstacles level.obstacles; *)
    draw_borders ()

  let update_player (player : player) =
    let key = Graphics.read_key () in
    let newX =
      if key = 'a' then player.x_pos - 20
      else if key = 'd' then player.x_pos + 20
      else player.x_pos
    in
    let newY =
      if key = 's' then player.y_pos - 20
      else if key = 'w' then player.y_pos + 20
      else player.y_pos
    in

    let newXnew =
      if key = 'a' || key = 'd' then
        if (*Corner Cases*)
           (newY = 270 || newY = 330) && newX < 70 then 70
        else if (newY = 270 || newY = 330) && newX > 860 then 860
          (*Edge Cases*)
        else if newY > 270 && newY < 330 && newX <= 70 then 70
        else if newY > 270 && newY < 330 && newX >= 860 then 860
        else if (newY > 330 || newY < 270) && newX <= 220 then 220
        else if (newY > 330 || newY < 270) && newX >= 730 then 730
        else newX
      else newX
    in

    let newYnew =
      if key = 's' || key = 'w' then
        if (*Corner Cases*)
           (newX = 220 || newX = 730) && newY > 390 then 390
        else if (newX = 220 || newX = 730) && newY < 210 then 210
          (*Edge cases*)
        else if (newX < 220 || newX > 730) && newY >= 330 then 330
        else if (newX < 220 || newX > 730) && newY <= 270 then 270
        else if newX > 220 && newX < 730 && newY >= 390 then 390
        else if newX > 220 && newX < 730 && newY <= 210 then 210
        else newY
      else newY
    in

    { x_pos = newXnew; y_pos = newYnew; radius = player.radius }

  (**[update_obstacles obs] updates the positions of all the obstacles in
    obs.*)
  let rec update_obstacles (obstacles : obstacle list) =
    match obstacles with
    | [] -> []
    | a :: b ->
        if a.obstacle_type = "projectile up" then
          let updated_y_1 = a.y_pos + obstacle_speed in
          let updated_y_2 =
            if updated_y_1 > 390 then 390 else updated_y_1
          in
          let updated_obstacle_type =
            if updated_y_1 > 390 then "" else "projectile up"
          in
          {
            x_pos = a.x_pos;
            y_pos = updated_y_2;
            obstacle_type = updated_obstacle_type;
            count = 0;
          }
          :: update_obstacles b
        else if a.obstacle_type = "projectile down" then
          let updated_y_1 = a.y_pos - obstacle_speed in
          let updated_y_2 =
            if updated_y_1 < 190 then 190 else updated_y_1
          in
          let updated_obstacle_type =
            if updated_y_1 < 190 then "" else "projectile down"
          in
          {
            x_pos = a.x_pos;
            y_pos = updated_y_2;
            obstacle_type = updated_obstacle_type;
            count = 0;
          }
          :: update_obstacles b
        else if a.obstacle_type = "turret up" then
          let projectile =
            {
              x_pos = a.x_pos + 6;
              y_pos = a.y_pos;
              obstacle_type = "projectile up";
              count = 0;
            }
          in
          let turret =
            if a.count = 140 then
              [ projectile ]
              @ [
                  {
                    x_pos = a.x_pos;
                    y_pos = a.y_pos;
                    obstacle_type = a.obstacle_type;
                    count = 0;
                  };
                ]
            else
              [
                {
                  x_pos = a.x_pos;
                  y_pos = a.y_pos;
                  obstacle_type = a.obstacle_type;
                  count = a.count + 1;
                };
              ]
          in
          turret @ update_obstacles b
        else if a.obstacle_type = "turret down" then
          let projectile =
            {
              x_pos = a.x_pos + 6;
              y_pos = a.y_pos;
              obstacle_type = "projectile down";
              count = 0;
            }
          in
          let turret =
            if a.count = 140 then
              [ projectile ]
              @ [
                  {
                    x_pos = a.x_pos;
                    y_pos = a.y_pos;
                    obstacle_type = a.obstacle_type;
                    count = 0;
                  };
                ]
            else
              [
                {
                  x_pos = a.x_pos;
                  y_pos = a.y_pos;
                  obstacle_type = a.obstacle_type;
                  count = a.count + 1;
                };
              ]
          in
          turret @ update_obstacles b
        else update_obstacles b

  let update (lvl : t) (lives : int) =
    Unix.sleepf 0.01;
    Graphics.clear_graph ();
    draw_borders ();
    Graphics.set_color Graphics.black;
    Graphics.set_text_size 1000000000;
    Graphics.moveto 25 560;
    Graphics.draw_string ("Lives: " ^ string_of_int lives);
    Graphics.moveto 475 560;
    Graphics.draw_string "LEVEL 6";
    Graphics.moveto 325 450;
    Graphics.draw_string
      "Watch out for the projectiles from the turrets";
    Graphics.set_color Graphics.red;
    Graphics.fill_circle lvl.p.x_pos lvl.p.y_pos 20;
    draw_obstacles lvl.obstacles;
    draw_goal lvl;
    let updated_obstacles = update_obstacles lvl.obstacles in
    if Graphics.key_pressed () then
      let updated_player = update_player lvl.p in
      {
        start_pos_x = lvl.start_pos_x;
        start_pos_y = lvl.start_pos_y;
        obstacles = updated_obstacles;
        collectables = lvl.collectables;
        finish_area = lvl.finish_area;
        p = updated_player;
        exit_open = true;
      }
    else
      {
        start_pos_x = lvl.start_pos_x;
        start_pos_y = lvl.start_pos_y;
        obstacles = updated_obstacles;
        collectables = lvl.collectables;
        finish_area = lvl.finish_area;
        exit_open = true;
        p = lvl.p;
      }

  let check_collisions lvl =
    let rec check_collisions_rec (player : player) obstacles =
      match obstacles with
      | [] -> false
      | a :: b ->
          if
            player.x_pos > a.x_pos - 20
            && player.x_pos < a.x_pos + 40
            && player.y_pos > a.y_pos - 20
            && player.y_pos < a.y_pos + 40
          then true
          else check_collisions_rec player b
    in
    check_collisions_rec lvl.p lvl.obstacles

  let reset_player (lvl : t) =
    {
      start_pos_x = lvl.start_pos_x;
      start_pos_y = lvl.start_pos_y;
      obstacles = lvl.obstacles;
      collectables = lvl.collectables;
      finish_area = lvl.finish_area;
      exit_open = true;
      p =
        {
          x_pos = lvl.start_pos_x;
          y_pos = lvl.start_pos_y;
          radius = 20;
        };
    }

  (**Level6 has no collectables, so this returns the current level state
    as is.*)
  let check_collections (lvl : t) = lvl

  let check_finish (lvl : t) =
    let x1 = match lvl.finish_area with a, _, _, _ -> a in
    let y1 = match lvl.finish_area with _, b, _, _ -> b in
    let x2 = match lvl.finish_area with _, _, c, _ -> c in
    let y2 = match lvl.finish_area with _, _, _, d -> d in

    if
      lvl.p.x_pos >= x1 && lvl.p.x_pos <= x2 && lvl.p.y_pos >= y1
      && lvl.p.y_pos <= y2
    then true
    else false
end

(*--------------------------------- LEVEL 7
  --------------------------------------------*)

module Level7 : Level = struct
  let level =
    {
      start_pos_x = 320;
      start_pos_y = 300;
      obstacles =
        [
          {
            x_pos = 470;
            y_pos = 385;
            obstacle_type = "square right";
            count = 0;
          };
          {
            x_pos = 573;
            y_pos = 295;
            obstacle_type = "square down";
            count = 0;
          };
          {
            x_pos = 470;
            y_pos = 195;
            obstacle_type = "square left";
            count = 0;
          };
          {
            x_pos = 375;
            y_pos = 295;
            obstacle_type = "square up";
            count = 0;
          };
        ];
      collectables =
        [
          { x_pos = 623; y_pos = 303; collectable_type = "KEY" };
          { x_pos = 480; y_pos = 435; collectable_type = "KEY" };
          { x_pos = 480; y_pos = 165; collectable_type = "KEY" };
        ];
      finish_area = (225, 275, 275, 325);
      p = { x_pos = 320; y_pos = 300; radius = 20 };
      exit_open = false;
    }

  let rec draw_collectables colls_lst =
    match colls_lst with
    | [] -> ()
    | a :: b ->
        if a.collectable_type = "KEY" then (
          Graphics.set_color Graphics.yellow;
          Graphics.fill_circle a.x_pos a.y_pos 10;
          draw_collectables b)
        else ()

  (**[draw_obstacles obs] draws all of the obstacles in obs to the graph
    in their current positions.*)
  let rec draw_obstacles (obstacles : obstacle list) =
    match obstacles with
    | [] -> ()
    | a :: b ->
        if
          a.obstacle_type = "square up"
          || a.obstacle_type = "square down"
          || a.obstacle_type = "square right"
          || a.obstacle_type = "square left"
        then (
          Graphics.set_color Graphics.magenta;
          Graphics.fill_rect a.x_pos a.y_pos 20 20;
          draw_obstacles b)
        else draw_obstacles b

  (**[draw_goal lvl] draws the goal in lvl to the graph.*)
  let draw_goal lvl =
    let x1 = match lvl.finish_area with a, _, _, _ -> a in
    let y1 = match lvl.finish_area with _, b, _, _ -> b in
    let x2 = match lvl.finish_area with _, _, c, _ -> c in
    let y2 = match lvl.finish_area with _, _, _, d -> d in
    if lvl.exit_open then Graphics.set_color Graphics.green
    else Graphics.set_color Graphics.red;

    Graphics.draw_rect x1 y1 (x2 - x1) (y2 - y1)

  (**[draw_borders ()] draws the borders of this level to the graph*)
  let draw_borders () =
    Graphics.set_color Graphics.black;
    Graphics.moveto 205 250;
    Graphics.lineto 360 250;
    Graphics.lineto 360 190;
    Graphics.lineto 452 190;
    Graphics.lineto 452 140;
    Graphics.lineto 507 140;
    Graphics.lineto 507 190;
    Graphics.lineto 600 190;
    Graphics.lineto 600 275;
    Graphics.lineto 650 275;
    Graphics.lineto 650 330;
    Graphics.lineto 600 330;
    Graphics.lineto 600 410;
    Graphics.lineto 507 410;
    Graphics.lineto 507 460;
    Graphics.lineto 452 460;
    Graphics.lineto 452 410;
    Graphics.lineto 360 410;
    Graphics.lineto 360 350;
    Graphics.lineto 205 350;
    Graphics.lineto 205 250

  let initialize_game level =
    draw_goal level;
    Graphics.set_color Graphics.red;
    Graphics.fill_circle level.start_pos_x level.start_pos_y 20;
    draw_obstacles level.obstacles;
    draw_borders ()

  let update_player (player : player) =
    let key = Graphics.read_key () in
    let newX =
      if key = 'a' then player.x_pos - 20
      else if key = 'd' then player.x_pos + 20
      else player.x_pos
    in
    let newY =
      if key = 's' then player.y_pos - 20
      else if key = 'w' then player.y_pos + 20
      else player.y_pos
    in

    let newXnew =
      if key = 'a' || key = 'd' then
        if (*Corner Cases*)
           (newY = 270 || newY = 330) && newX < 225 then 225
        else if newY = 390 && newX < 380 then 380 (*Edge Cases*)
        else if newY > 270 && newY < 330 && newX <= 225 then 225
        else if newY > 255 && newY < 350 && newX >= 630 then 630
        else if
          ((newY > 330 && newY < 390) || (newY > 170 && newY < 270))
          && newX <= 380
        then 380
        else if
          ((newY > 310 && newY < 430) || (newY > 170 && newY < 295))
          && newX >= 580
        then 580
        else if
          ((newY > 390 && newY < 480) || (newY > 120 && newY < 210))
          && newX >= 487
        then 487
        else if
          ((newY > 390 && newY < 480) || (newY > 120 && newY < 210))
          && newX <= 472
        then 472
        else newX
      else newX
    in

    let newYnew =
      if key = 's' || key = 'w' then
        if (*Corner Cases*)
           newX = 380 && newY > 390 then 390
        else if newX = 380 && newY < 210 then 210 (*Edge cases*)
        else if (newX < 380 && newX > 185) && newY >= 330 then 330
        else if (newX < 380 && newX > 185) && newY <= 270 then 270
        else if
          ((newX < 472 && newX > 380) || (newX < 620 && newX > 487))
          && newY >= 390
        then 390
        else if
          ((newX < 472 && newX > 380) || (newX < 620 && newX > 487))
          && newY <= 210
        then 210
        else if (newX < 527 && newX > 432) && newY >= 440 then 440
        else if (newX < 527 && newX > 432) && newY <= 160 then 160
        else if (newX < 670 && newX > 580) && newY >= 310 then 310
        else if (newX < 670 && newX > 580) && newY <= 295 then 295
        else newY
      else newY
    in

    { x_pos = newXnew; y_pos = newYnew; radius = player.radius }

  (**[update_obstacles obs] updates the positions of all the obstacles in
    obs.*)
  let rec update_obstacles (obstacles : obstacle list) =
    match obstacles with
    | [] -> []
    | a :: b ->
        if a.obstacle_type = "square up" then
          let updated_y_1 = a.y_pos + obstacle_speed in
          let updated_y_2 =
            if updated_y_1 > 385 then 385 else updated_y_1
          in
          let updated_obstacle_type =
            if updated_y_1 > 385 then "square right" else "square up"
          in
          {
            x_pos = a.x_pos;
            y_pos = updated_y_2;
            obstacle_type = updated_obstacle_type;
            count = 0;
          }
          :: update_obstacles b
        else if a.obstacle_type = "square down" then
          let updated_y_1 = a.y_pos - obstacle_speed in
          let updated_y_2 =
            if updated_y_1 < 195 then 195 else updated_y_1
          in
          let updated_obstacle_type =
            if updated_y_1 < 195 then "square left" else "square down"
          in
          {
            x_pos = a.x_pos;
            y_pos = updated_y_2;
            obstacle_type = updated_obstacle_type;
            count = 0;
          }
          :: update_obstacles b
        else if a.obstacle_type = "square left" then
          let updated_x_1 = a.x_pos - obstacle_speed in
          let updated_x_2 =
            if updated_x_1 < 375 then 375 else updated_x_1
          in
          let updated_obstacle_type =
            if updated_x_1 < 375 then "square up" else "square left"
          in
          {
            x_pos = updated_x_2;
            y_pos = a.y_pos;
            obstacle_type = updated_obstacle_type;
            count = 0;
          }
          :: update_obstacles b
        else
          let updated_x_1 = a.x_pos + obstacle_speed in
          let updated_x_2 =
            if updated_x_1 > 573 then 573 else updated_x_1
          in
          let updated_obstacle_type =
            if updated_x_1 > 573 then "square down" else "square right"
          in
          {
            x_pos = updated_x_2;
            y_pos = a.y_pos;
            obstacle_type = updated_obstacle_type;
            count = 0;
          }
          :: update_obstacles b

  let update (lvl : t) (lives : int) =
    Unix.sleepf 0.01;
    Graphics.clear_graph ();
    draw_borders ();
    Graphics.set_color Graphics.black;
    Graphics.set_text_size 1000000000;
    Graphics.moveto 25 560;
    Graphics.draw_string ("Lives: " ^ string_of_int lives);
    Graphics.moveto 475 560;
    Graphics.draw_string "LEVEL 7";
    draw_collectables lvl.collectables;
    Graphics.set_color Graphics.red;
    Graphics.fill_circle lvl.p.x_pos lvl.p.y_pos 20;
    draw_obstacles lvl.obstacles;
    draw_goal lvl;
    let updated_obstacles = update_obstacles lvl.obstacles in
    if Graphics.key_pressed () then
      let updated_player = update_player lvl.p in
      {
        start_pos_x = lvl.start_pos_x;
        start_pos_y = lvl.start_pos_y;
        obstacles = updated_obstacles;
        collectables = lvl.collectables;
        finish_area = lvl.finish_area;
        p = updated_player;
        exit_open = lvl.exit_open;
      }
    else
      {
        start_pos_x = lvl.start_pos_x;
        start_pos_y = lvl.start_pos_y;
        obstacles = updated_obstacles;
        collectables = lvl.collectables;
        finish_area = lvl.finish_area;
        p = lvl.p;
        exit_open = lvl.exit_open;
      }

  let check_collisions lvl =
    let rec check_collisions_rec (player : player) obstacles =
      match obstacles with
      | [] -> false
      | a :: b ->
          if
            player.x_pos > a.x_pos - 20
            && player.x_pos < a.x_pos + 40
            && player.y_pos > a.y_pos - 20
            && player.y_pos < a.y_pos + 40
          then true
          else check_collisions_rec player b
    in
    check_collisions_rec lvl.p lvl.obstacles

  let reset_player (lvl : t) =
    {
      start_pos_x = lvl.start_pos_x;
      start_pos_y = lvl.start_pos_y;
      obstacles = lvl.obstacles;
      collectables = lvl.collectables;
      finish_area = lvl.finish_area;
      exit_open = lvl.exit_open;
      p =
        {
          x_pos = lvl.start_pos_x;
          y_pos = lvl.start_pos_y;
          radius = 20;
        };
    }

  let check_collections (lvl : t) =
    let key_collected = ref false in
    key_collected := false;
    if lvl.exit_open then key_collected := true else ();
    let rec check_collections_rec colls_lst =
      match colls_lst with
      | [] -> []
      | a :: b ->
          if a.collectable_type = "KEY" then
            if
              lvl.p.x_pos > a.x_pos - 30
              && lvl.p.x_pos < a.x_pos + 30
              && lvl.p.y_pos > a.y_pos - 30
              && lvl.p.y_pos < a.y_pos + 30
            then check_collections_rec b
            else a :: check_collections_rec b
          else check_collections_rec b
    in
    let new_collectables_lst = check_collections_rec lvl.collectables in
    (match new_collectables_lst with
    | [] -> key_collected := true
    | _ -> ());
    {
      start_pos_x = lvl.start_pos_x;
      start_pos_y = lvl.start_pos_y;
      obstacles = lvl.obstacles;
      collectables = new_collectables_lst;
      finish_area = lvl.finish_area;
      exit_open = !key_collected;
      p = lvl.p;
    }

  let check_finish (lvl : t) =
    let x1 = match lvl.finish_area with a, _, _, _ -> a in
    let y1 = match lvl.finish_area with _, b, _, _ -> b in
    let x2 = match lvl.finish_area with _, _, c, _ -> c in
    let y2 = match lvl.finish_area with _, _, _, d -> d in

    if
      lvl.p.x_pos >= x1 && lvl.p.x_pos <= x2 && lvl.p.y_pos >= y1
      && lvl.p.y_pos <= y2 && lvl.exit_open
    then true
    else false
end
