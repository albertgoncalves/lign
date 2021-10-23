type note = C | D | E | F | G | A | B

type accidental = Flat | Natural | Sharp

type pitch = (note * accidental)

type octave = int

type abs_pitch = (pitch * octave)

type duration = Quarter | Eighth

let pitch_to_int ((n, a) : pitch) : int =
  let x : int =
    match n with
    | C -> 0
    | D -> 2
    | E -> 4
    | F -> 5
    | G -> 7
    | A -> 9
    | B -> 11 in
  match a with
  | Flat -> x - 1
  | Natural -> x
  | Sharp -> x + 1

let abs_pitch_to_int ((p, o): abs_pitch) : int =
  (pitch_to_int p) + (o * 12)

let get_durations () : duration Queue.t =
  let ds : duration Queue.t = Queue.create () in
  for _ = 0 to 3 do
    if Random.bool () then (
      Queue.add Eighth ds;
      Queue.add Eighth ds;
    ) else (
      Queue.add Quarter ds;
    )
  done;
  ds

let nearest_pitch (p0 : pitch) (ps : pitch list) : pitch =
  let n : int = pitch_to_int p0 in
  ps
  |> List.map (fun p1 -> (abs ((pitch_to_int p1) - n), p1))
  |> List.filter (fun (d, _) -> d <> 0)
  |> List.sort (fun (a, _) (b, _) -> compare a b)
  |> List.hd
  |> snd

let rec nearest_abs_pitch
    (n : int)
    (p : pitch)
    (o : octave) : abs_pitch =
  let d : int = n - (abs_pitch_to_int (p, o)) in
  if (abs d) < 7 then
    (p, o)
  else if d < 0 then
    nearest_abs_pitch n p (o - 1)
  else
    nearest_abs_pitch n p (o + 1)

let get_first ((p, o) as ap : abs_pitch) (ps : pitch list) : abs_pitch =
  nearest_abs_pitch (abs_pitch_to_int ap) (nearest_pitch p ps) o

let lookup (x : 'a) (xs : 'a array) : int =
  let i : int ref = ref 0 in
  while xs.(!i) <> x do
    incr i;
  done;
  !i

let modulo (x : int) (n : int) : int =
  if x < 0 then
    (x mod n) + n
  else if n <= x then
    x mod n
  else
    x

let step_up (from : int) (xs : 'a array) : int =
  modulo (from + 1) (Array.length xs)

let step_down (from : int) (xs : 'a array) : int =
  modulo (from - 1) (Array.length xs)

let get_arpeggio
    (l : int)
    (u : int)
    (ps : pitch array)
    (ap0 : abs_pitch) : (abs_pitch * (abs_pitch * duration) Queue.t) =
  let ((p1, o1) as ap1) : abs_pitch = get_first ap0 (Array.to_list ps) in
  let xs : (abs_pitch * duration) Queue.t = Queue.create () in
  let ds : duration Queue.t = get_durations () in
  Queue.add (ap1, Queue.take ds) xs;
  let i : int ref = ref (lookup p1 ps) in
  let ap2 : abs_pitch ref = ref ap1 in
  let o2 : octave ref = ref o1 in
  let f (d : duration) : unit =
    let j : int =
      if Random.bool () then
        step_up (!i) ps
      else
        step_down (!i) ps in
    let ((p3, o3) as ap3) : abs_pitch =
      nearest_abs_pitch (abs_pitch_to_int (!ap2)) ps.(j) (!o2) in
    let ap3 : abs_pitch =
      let n : int = abs_pitch_to_int ap3 in
      if n < l then
        (p3, o3 + 1)
      else if u < n then
        (p3, o3 - 1)
      else
        ap3 in
    (
      let n : int = abs_pitch_to_int ap3 in
      assert (l <= n);
      assert (n <= u);
    );
    (
      Queue.add (ap3, d) xs;
      ap2 := ap3;
      o2 := o3;
      i := j;
    ) in
  Queue.iter f ds;
  (!ap2, xs)

let rec render_octave (buffer : Buffer.t) (o : octave) : unit =
  if 0 < o then (
    Buffer.add_char buffer '\'';
    render_octave buffer (o - 1)
  ) else if o < 0 then (
    Buffer.add_char buffer ',';
    render_octave buffer (o + 1)
  ) else (
    ()
  )

let render
    (buffer : Buffer.t)
    ((((n, a), o), d) : (abs_pitch * duration)) : unit =
  Buffer.add_char buffer ' ';
  Buffer.add_char buffer
    (match n with
     | C -> 'c'
     | D -> 'd'
     | E -> 'e'
     | F -> 'f'
     | G -> 'g'
     | A -> 'a'
     | B -> 'b');
  (match a with
   | Flat -> Buffer.add_string buffer "es"
   | Natural -> ()
   | Sharp -> Buffer.add_string buffer "is");
  render_octave buffer o;
  Buffer.add_char buffer
    (match d with
     | Quarter -> '4'
     | Eighth -> '8')

let set_arpeggios
    (l : int)
    (u : int)
    (buffer : Buffer.t)
    (xs : pitch array list) : unit =
  let f
      ((ap0, qs0) : (abs_pitch * (abs_pitch * duration) Queue.t))
      (ps : pitch array) : (abs_pitch * (abs_pitch * duration) Queue.t) =
    let (ap1, qs1) : (abs_pitch * (abs_pitch * duration) Queue.t) =
      get_arpeggio l u ps ap0 in
    Queue.transfer qs1 qs0;
    (ap1, qs0) in
  xs
  |> List.fold_left f (((C, Natural), 1), Queue.create ())
  |> snd
  |> Queue.iter (render buffer)

let tests () : unit =
  assert
    ((nearest_pitch (D, Flat) [(C, Natural); (E, Natural)]) = (C, Natural));
  assert
    ((nearest_pitch (D, Sharp) [(C, Natural); (E, Natural)]) = (E, Natural));
  assert
    ((nearest_pitch (D, Natural) [(B, Flat); (C, Natural); (D, Natural)]) =
     (C, Natural));
  assert
    (((nearest_abs_pitch (abs_pitch_to_int ((B, Flat), 1)) (C, Natural) 0)) =
     ((C, Natural), 2));
  assert
    (((nearest_abs_pitch (abs_pitch_to_int ((F, Sharp), -1)) (C, Natural) 1)) =
     ((C, Natural), 0))

let () : unit =
  Random.self_init ();
  tests ();
  let buffer : Buffer.t = Buffer.create (1 lsl 10) in
  let xs : pitch array list =
    [
      [|(B, Flat); (D, Natural); (F, Natural)|];
      [|(D, Natural); (F, Sharp); (A, Natural)|];
      [|(F, Sharp); (A, Sharp); (C, Sharp)|];
      [|(B, Flat); (D, Flat); (F, Natural)|];
      [|(E, Flat); (G, Natural); (B, Flat)|];
      [|(G, Natural); (B, Natural); (D, Natural)|];
      [|(B, Natural); (D, Sharp); (F, Sharp)|];
    ] in
  Buffer.add_string buffer {|\version "2.22.1"
#(set-global-staff-size 28)
\paper {
  indent = 0\mm
  line-width = 140\mm
  oddFooterMarkup = ##f
  oddHeaderMarkup = ##f
  bookTitleMarkup = ##f
  scoreTitleMarkup = ##f
}
melody = {
  \clef treble
  \time 4/4
  \tempo 4 = 120
  |};
  set_arpeggios 4 33 buffer xs;
  Buffer.add_string buffer {|
}
\score {
  \new Staff \melody
  \layout { }
  \midi { }
}
|};
  if (Array.length Sys.argv) < 2 then (
    Buffer.output_buffer stdout buffer;
    flush stdout
  ) else (
    let channel : out_channel = open_out Sys.argv.(1) in
    Buffer.output_buffer channel buffer;
    close_out channel
  )
