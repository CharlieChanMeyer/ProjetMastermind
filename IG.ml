open Graphics
 
let _ =
  open_graph "";
  set_window_title "Mastermind";
  resize_window 1500 1500;
  set_color black;
  fill_rect 500 100 200 50;
  fill_rect 500 200 200 500;
  set_color white;
  fill_circle 525 225 10; fill_circle 575 225 10; fill_circle 625 225 10; fill_circle 675 225 10; fill_circle 525 275 10; fill_circle 575 275 10; fill_circle 625 275 10; fill_circle 675 275 10; fill_circle 525 325 10; fill_circle 575 325 10; fill_circle 625 325 10; fill_circle 675 325 10;
  set_color red;
  fill_circle 525 125 10;
  set_color blue;
  fill_circle 575 125 10;
  set_color green;
  fill_circle 625 125 10;
  set_color yellow;
  fill_circle 675 125 10;; 
