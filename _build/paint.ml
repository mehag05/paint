(** The main paint application *)

;; open Gctx
;; open Widget

(******************************************)
(**    SHAPES, MODES, and PROGRAM STATE   *)
(******************************************)

(** A location in the paint_canvas widget *)
type point = position  (* from Gctx *)

(** The shapes that are visible in the paint canvas -- these make up the
    picture that the user has drawn, as well as any other "visible" elements
    that must show up in the canvas area (e.g. a "selection rectangle"). At
    the start of the homework, the only available shape is a line.  *)
(* TODO: You will modify this definition in Tasks 3, 4, 5 and maybe 6. *)
type shape = 
  | Line of {color: color; p1: point; p2: point; thicknessint: int}
  | Points of { color: Gctx.color; points: point list; thicknessint: int}
  | Ellipse of {color: Gctx.color; p: point; rx: int; ry: int; 
  thicknessint: int}

(** These are the possible interaction modes that the paint program might be
    in. Some interactions require two modes. For example, the GUI might
    recognize the first mouse click as starting a line and a second mouse
    click as finishing the line.

    To start out, there are only two modes:

      - LineStartMode means the paint program is waiting for the user to make
        the first click to start a line.

      - LineEndMode means that the paint program is waiting for the user's
        second click. The point associated with this mode stores the location
        of the user's first mouse click.  *)
(* TODO: You will need to modify this type in Tasks 3 and 4, and maybe 6. *)
type mode = 
  | LineStartMode
  | LineEndMode of point
  | PointMode 
  | EllipseStartMode
  | EllipseEndMode of point


(** The state of the paint program. *)
type state = {
  (** The sequence of all shapes drawn by the user, in order from
      least recent (the head) to most recent (the tail). *)
  shapes : shape Deque.deque;

  (** The input mode the Paint program is in. *)
  mutable mode : mode;

  (** The currently selected pen color. *)
  mutable color : color;

  (* TODO: You will need to add new state for Tasks 2, 5, and *)
  (* possibly 6 *) 
  mutable preview : shape option;
  mutable thicknessint : int; (*Added an integer for the thickness value*)
}

(** Initial values of the program state. *)
let paint : state = {
  shapes = Deque.create ();
  mode = LineStartMode;
  color = black;
  preview = None;
  thicknessint = 1;
  (* TODO: You will need to add new state for Tasks 2, 5, and maybe 6 *) 
}



(** This function creates a graphics context with the appropriate
    pen color. *)
(* TODO: You will need to modify this function in Task 5 *)
let with_params (g: gctx) (c: color) (b: int) : gctx =
  let g = with_color g c in
  let g = with_thickness g b in
  g


(*********************************)
(**    MAIN CANVAS REPAINTING    *)
(*********************************)

(** The paint_canvas repaint function.

    This function iterates through all the drawn shapes (in order of least
    recent to most recent so that they are layered on top of one another
    correctly) and uses the Gctx.draw_xyz functions to display them on the
    canvas.  *)

(* TODO: You will need to modify this repaint function in Tasks 2, 3,
   4, and possibly 5 or 6. For example, if the user is performing some
   operation that provides "preview" (see Task 2) the repaint function
   must also show the preview. *)
let repaint (g: gctx) : unit =
  let draw_shape (s: shape) : unit =
      begin match s with
        | Line l -> draw_line (with_params g l.color l.thicknessint) l.p1 l.p2
        | Points p -> 
        draw_points (with_params g p.color p.thicknessint) p.points
        | Ellipse e -> 
        draw_ellipse (with_params g e.color e.thicknessint) e.p e.rx e.ry
      end
    in
    Deque.iterate draw_shape paint.shapes;
    (begin match paint.preview with
    | None -> ()
    | Some n -> draw_shape n
    end)

(** Create the actual paint_canvas widget and its associated
    notifier_controller . *)
let ((paint_canvas : widget), (paint_canvas_controller : notifier_controller)) =
  canvas (600, 350) repaint


(************************************)
(**  PAINT CANVAS EVENT HANDLER     *)
(************************************)

(** The paint_action function processes all events that occur
    in the canvas region. *)
(* TODO: Tasks 2, 3, 4, 5, and 6 involve changes to paint_action. *)

(*Creating a function to draw ellipses properly*)
let ellipse_creator (p1: point) (p2: point): point * int * int =
  let (x1, y1) = p1 in
  let (x2, y2) = p2 in
  let ry = (y2 - y1)/2 in
  let rx = (x2 - x1)/2 in
  let center = 
  if x1 < x2 && y1 < y2 then (x1 + rx, y1 + ry)
  else if x1 < x2 && y1 >= y2 then (x1 + rx, y1 - ry)
  else if x1 >= x2 && y1 < y2 then (x1 - rx, y1 + ry)
  else (x1 - rx, y1 - ry) in 
  (center, rx, ry)

let paint_action (gc:gctx) (event:event) : unit =
  let p  = event_pos event gc in  (* mouse position *)
  begin match (event_type event) with
    | MouseDown ->
       (* This case occurs when the mouse has been clicked in the
          canvas, but before the button has been released. How we
          process the event depends on the current mode of the paint
          canvas.  *)
      (begin match paint.mode with 
          | LineStartMode ->
            (* The paint_canvas was waiting for the first click of a line,
              so change it to LineEndMode, recording the starting point of
              the line. *)
            paint.mode <- LineEndMode p
          | LineEndMode p1 ->
            (* The paint_canvas was waiting for the second click of a line,
              so create the line and add it to the deque of shapes. Go back
              to waiting for the first click. *)
            Deque.insert_tail
              (Line {color=paint.color; p1=p1; p2=p; 
              thicknessint=paint.thicknessint}) paint.shapes;
            paint.mode <- LineStartMode;
            paint.preview <- None
          | PointMode -> 
          (*add each successive point to the preview*)
            paint.preview <- Some (Points {color= paint.color; 
            points= [p]; thicknessint=paint.thicknessint});
          | EllipseStartMode -> paint.mode <- EllipseEndMode p
          (*change to end mode*)
          | EllipseEndMode p1 -> ()
          end)
    | MouseDrag ->
      (* In this case, the mouse has been clicked, and it's being dragged
         with the button down. Initially there is nothing to do, but you'll
         need to update this part for Task 2, 3, 4 and maybe 6. *)
      begin match paint.mode with 
      | LineStartMode -> 
      (*Update a line to be displayed in preview*)
      paint.preview <- Some 
      (Line {color=paint.color; p1=p; p2=p; thicknessint=paint.thicknessint})
      | LineEndMode p1 -> paint.preview <- Some 
      (*Show the final line*)
      (Line {color=paint.color; p1=p1; p2=p; thicknessint=paint.thicknessint})
      | PointMode -> 
      (*Create and impelmetn a list of points to add to the preview*)
      let points_list =
      (begin match paint.preview with
      | Some (Points ps) -> ps.points
      | _ -> []
      end) in 
      paint.preview <- Some (Points {color= paint.color; 
      points= p::points_list; thicknessint=paint.thicknessint})
      | EllipseStartMode -> paint.preview <- None (*Nothing to show*)
      | EllipseEndMode p1 -> 
      (*call the function from earlier to create the ellipse*)
      let (center, rx, ry) = ellipse_creator p1 p in
      paint.preview <- Some (Ellipse {color= paint.color; 
      p=center; rx=rx; ry=ry; thicknessint=paint.thicknessint}) 
       end
    | MouseUp ->
      (* In this case there was a mouse button release event. TODO: Tasks 2,
         3, 4, and possibly 6 need to do something different here. *)
      (begin match paint.mode with 
          | LineStartMode ->
            (* The paint_canvas was waiting for the first click of a line,
              so change it to LineEndMode, recording the starting point of
              the line. *)
            paint.mode <- LineEndMode p
          | LineEndMode p1 ->
            (* The paint_canvas was waiting for the second click of a line,
              so create the line and add it to the deque of shapes. Go back
              to waiting for the first click. *)
            Deque.insert_tail
              (Line {color=paint.color; p1=p1; p2=p; 
              thicknessint=paint.thicknessint}) paint.shapes;
            paint.mode <- LineStartMode;
            paint.preview <- None
          | PointMode ->
          let points_list =
            (begin match paint.preview with
            | Some (Points ps) -> ps.points
            | _ -> []
            end) in
          Deque.insert_tail
              (Points {color=paint.color; points = points_list; 
              thicknessint=paint.thicknessint}) paint.shapes;
            paint.preview <- None
          | EllipseStartMode -> paint.preview <- None
          | EllipseEndMode p1 -> 
          let (center, rx, ry) = ellipse_creator p1 p in
          Deque.insert_tail
              (Ellipse {color= paint.color; p=center; rx=rx; ry=ry; 
              thicknessint=paint.thicknessint}) paint.shapes;
          paint.preview <- None;
          paint.mode <- EllipseStartMode
       end)
    | _ -> ()
    (* This catches the MouseMove event (where the user moved the mouse over
       the canvas without pushing any buttons) and the KeyPress event (where
       the user typed a key when the mouse was over the canvas). *)
  end

(** Add the paint_action function as a listener to the paint_canvas *)
;; paint_canvas_controller.add_event_listener paint_action


(**************************************)
(** TOOLBARS AND PAINT PROGRAM LAYOUT *)
(**************************************)

(** This part of the program creates the other widgets for the paint
    program -- the buttons, color selectors, etc., and lays them out
    in the top - level window. *)
(* TODO: Tasks 1, 4, 5, and 6 involve adding new buttons or changing
   the layout of the Paint GUI. Initially the layout is ugly because
   we use only the hpair widget demonstrated in Lecture. Task 1 is to
   make improvements to make the layout more appealing. You may choose
   to arrange the buttons and other GUI elements of the paint program
   however you like (so long as it is easily apparent how to use the
   interface).  The sample screenshot of our solution shows one
   possible design.  Also, feel free to improve the visual components
   of the GUI; for example, our solution puts borders around the
   buttons and uses a custom "color button" that changes its
   appearance based on whether or not the color is currently
   selected. *)

(** Create the Undo/Line/Point buttons and the Thickness checkbox and Slider*)
let (w_undo, lc_undo, nc_undo) = button "Undo"
let undo () : unit =
  if Deque.is_empty paint.shapes then () else
    ignore (Deque.remove_tail paint.shapes)
;; nc_undo.add_event_listener (mouseclick_listener undo)

let (w_line, lc_line, nc_line) = button "Line"
let line (): unit =
  paint.mode <- LineStartMode
;; nc_line.add_event_listener (mouseclick_listener line)

let (w_points, lc_points, nc_points) = button "Point"
let points (): unit =
  paint.mode <- PointMode
;; nc_points.add_event_listener (mouseclick_listener points)

let (w_ellipse, lc_ellipse, nc_ellipse) = button "Ellipse"
let ellipse () : unit =
  paint.mode <- EllipseStartMode
;; nc_ellipse.add_event_listener (mouseclick_listener ellipse)

(*Setting paint thickness to whatever the slider value is at*)
let (w_slide, c_slide) = slider 50 "Thickness Slider"
let slide () : unit =
  paint.thicknessint <- c_slide.get_value()/3;
;; c_slide.add_change_listener (fun _ -> slide ())

(*Setting thickness to slider val when the box is checked and 1 when it is not*)
let (w_thick, c_thick) = checkbox false "Thick"
let thick () : unit =
  if c_thick.get_value() then paint.thicknessint <- c_slide.get_value() else 
  paint.thicknessint <- 1;
;; c_thick.add_change_listener (fun _ -> thick ())

(** This function runs when the Undo button is clicked.
    It simply removes the last shape from the shapes deque. *)
(* TODO: You need to modify this in Task 3 and 4, and potentially 2
   (depending on your implementation). *)

(** A spacer widget *)
let spacer : widget = space (10,10)


(** The mode toolbar, initially containing just the Undo button.
    TODO: you will need to modify this widget to add more buttons
    to the toolbar in Task 1, Tasks 5, and possibly 6. *)
let mode_toolbar : widget =  hlist [w_undo; spacer; w_line; spacer; w_points; 
spacer; w_ellipse; spacer; w_thick]

(* The color selection toolbar. *)
(* This toolbar contains an indicator for the currently selected color
   and some buttons for changing it. Both the indicator and the buttons
   are small square widgets built from this higher-order function. *)
(** Create a widget that displays itself as colored square with the given
    width and color specified by the [get_color] function. *)
let colored_square (width:int) (get_color:unit -> color)
  : widget * notifier_controller =
  let repaint_square (gc:gctx) =
    let c = get_color () in
    fill_rect (with_color gc c) (0, 0) (width-1, width-1) in
  canvas (width,width) repaint_square

(** The color_indicator repaints itself with the currently selected
    color of the paint application. *)
let color_indicator =
  let indicator,_ = colored_square 24 (fun () -> paint.color) in
  let lab, _ = label "Current Color" in
  border (hpair lab indicator)

(** color_buttons repaint themselves with whatever color they were created
    with. They are also installed with a mouseclick listener
    that changes the selected color of the paint app to their color. *)
let color_button (c: color) : widget =
  let w,nc = colored_square 10 (fun () -> c) in
  nc.add_event_listener (mouseclick_listener (fun () ->
      paint.color <- c ));
  w

(** The color selection toolbar. Contains the color indicator and
    buttons for several different colors. *)
(* TODO: Task 1 - This code contains a great deal of boilerplate.  You
     should come up with a better, more elegant, more concise solution... *)
   let color_toolbar : widget =
   hlist 
   [color_indicator; color_button black; color_button white; color_button red; 
   color_button green; color_button blue; color_button yellow; 
   color_button cyan; color_button magenta]

(** The top-level paint program widget: a combination of the
    mode_toolbar, the color_toolbar and the paint_canvas widgets. *)
(* TODO: Task 1 (and others) involve modifing this layout to add new
   buttons and make the layout more aesthetically appealing. *)
let paint_widget =
  let (l, _) = label "Thickness Slider" in
   vlist [paint_canvas; mode_toolbar; spacer; color_toolbar; spacer; 
   border (hpair (w_slide) (l))]


(**************************************)
(**      Start the application        *)
(**************************************)

(** Run the event loop to process user events. *)
;; Eventloop.run paint_widget
