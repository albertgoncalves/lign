type note = C | D | E | F | G | A | B

type accidental = DoubleFlat | Flat | Natural | Sharp

type pitch = (note * accidental)

type octave = int

type abs_pitch = (pitch * octave)

type duration = Quarter | Eighth

type chord = pitch array

type sound = (abs_pitch * duration)

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
  | DoubleFlat -> x - 2
  | Flat -> x - 1
  | Natural -> x
  | Sharp -> x + 1

let abs_pitch_to_int ((p, o): abs_pitch) : int =
  (pitch_to_int p) + ((o - 1) * 12)

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
  let f (a : int) (b : int) : int =
    let n : int = abs (a - b) in
    if n < 7 then
      n
    else
      12 - n in
  let n : int = pitch_to_int p0 in
  ps
  |> List.map (fun p1 -> (f (pitch_to_int p1) n, p1))
  |> List.filter (fun (d, _) -> d <> 0)
  |> List.sort (fun (a, _) (b, _) -> compare a b)
  |> List.hd
  |> snd

let rec nearest_abs_pitch (n : int) (p : pitch) (o : octave) : abs_pitch =
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

let step_by (i : int) (n : int) (xs : 'a array) : int =
  modulo (i + n) (Array.length xs)

let get_arpeggio
    (l : int)
    (u : int)
    (c : chord)
    (ap0 : abs_pitch) : (abs_pitch * sound Queue.t) =
  let ((p1, o1) as ap1) : abs_pitch = get_first ap0 (Array.to_list c) in
  let o1 : octave =
    let n : int = abs_pitch_to_int ap1 in
    if n < l then
      o1 + 1
    else if u < n then
      o1 - 1
    else
      o1 in
  let ap1 : abs_pitch = (p1, o1) in
  (
    let n : int = abs_pitch_to_int ap1 in
    assert (l <= n);
    assert (n <= u);
  );
  let xs : sound Queue.t = Queue.create () in
  let ds : duration Queue.t = get_durations () in
  Queue.add (ap1, Queue.take ds) xs;
  let i : int ref = ref (lookup p1 c) in
  let ap2 : abs_pitch ref = ref ap1 in
  let o2 : octave ref = ref o1 in
  let f (d : duration) : unit =
    let j : int =
      if Random.bool () then
        step_by (!i) 1 c
      else
        step_by (!i) (-1) c in
    let ((p3, o3) as ap3) : abs_pitch =
      nearest_abs_pitch (abs_pitch_to_int (!ap2)) c.(j) (!o2) in
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

let render_octave (b : Buffer.t) (o : octave) : unit =
  if o < 0 then
    for _ = -1 downto o do
      Buffer.add_char b ','
    done
  else
    for _ = 1 to o do
      Buffer.add_char b '\''
    done

let render_sound (b : Buffer.t) ((((n, a), o), d) : sound) : unit =
  Buffer.add_char b ' ';
  Buffer.add_char b
    (match n with
     | C -> 'c'
     | D -> 'd'
     | E -> 'e'
     | F -> 'f'
     | G -> 'g'
     | A -> 'a'
     | B -> 'b');
  (match a with
   | DoubleFlat -> Buffer.add_string b "eses"
   | Flat -> Buffer.add_string b "es"
   | Natural -> ()
   | Sharp -> Buffer.add_string b "is");
  render_octave b o;
  Buffer.add_char b
    (match d with
     | Quarter -> '4'
     | Eighth -> '8')

let set_arpeggios
    (ap0 : abs_pitch)
    (l : int)
    (u : int)
    (b : Buffer.t)
    (cs : chord list) : unit =
  let f
      ((ap1, ps1) : (abs_pitch * sound Queue.t))
      (c : chord) : (abs_pitch * sound Queue.t) =
    let (ap2, ps2) : (abs_pitch * sound Queue.t) = get_arpeggio l u c ap1 in
    Queue.transfer ps2 ps1;
    (ap2, ps1) in
  cs
  |> List.fold_left f (ap0, Queue.create ())
  |> snd
  |> Queue.iter (render_sound b)

let tests () : unit =
  assert
    ((nearest_pitch (D, Flat) [(C, Natural); (E, Natural)]) = (C, Natural));
  assert
    ((nearest_pitch (D, Sharp) [(C, Natural); (E, Natural)]) = (E, Natural));
  assert
    ((nearest_pitch (B, Natural) [(A, Flat); (C, Natural)]) = (C, Natural));
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
  tests ();
  let b : Buffer.t = Buffer.create (1 lsl 10) in

  Random.self_init ();
  let d_m : pitch array =
    [|(D, Natural); (F, Natural); (A, Natural)|] in
  let g_m7 : pitch array =
    [|(G, Natural); (B, Flat); (D, Natural); (F, Natural)|] in
  let c_7 : pitch array =
    [|(C, Natural); (E, Natural); (G, Natural); (B, Flat)|] in
  let ees_m7b5 : pitch array =
    [|(E, Flat); (G, Flat); (B, DoubleFlat); (D, Flat)|] in
  let a_7s5s9 : pitch array =
    [|(A, Natural); (C, Sharp); (E, Sharp); (G, Natural); (B, Sharp)|] in
  let f_m7 : pitch array =
    [|(F, Natural); (A, Flat); (C, Natural); (E, Flat)|] in
  let bes_7 : pitch array =
    [|(B, Flat); (D, Natural); (F, Natural); (A, Flat)|] in
  let ees_maj7 : pitch array =
    [|(E, Flat); (G, Natural); (B, Flat); (D, Natural)|] in
  let a_7s5 : pitch array =
    [|(A, Natural); (C, Sharp); (E, Sharp); (G, Natural)|] in
  let cs : chord list =
    [
      d_m;
      d_m;
      g_m7;
      c_7;
      ees_m7b5;
      a_7s5s9;
      d_m;
      d_m;
      f_m7;
      bes_7;
      ees_maj7;
      ees_m7b5;
      a_7s5s9;
      d_m;
      a_7s5;
    ] in

  Buffer.add_string b (Printf.sprintf {|\version "2.24.1"
\include "swing.ly"
\paper {
  indent = 0\mm
}
\header {
  tagline = ##f
}
global = {
  \time 4/4
  \tempo 4 = 200
}
count_in = \new DrumStaff {
  \global
  \drummode
  {
    cl4 cl  cl  cl  |
  }
}
pulse = \new DrumStaff {
  \global
  \drummode
  {
    \repeat unfold %d {
      r4  hh  r   hh  |
    }
  }
}
melody = \new Staff \with { midiInstrument = "electric guitar (jazz)" } {
  \global
  \clef "treble_8"
  {
|} (List.length cs));

  set_arpeggios ((G, Natural), 0) (-12) 7 b cs;

  Buffer.add_string b {|
  }
}
\score {
  {
    \melody
  }
  \layout { }
}
\score {
  \applySwing 8 #'(4 3)
  {
    \count_in
    <<
      \melody
      \pulse
    >>
  }
  \midi { }
}
|};

  if (Array.length Sys.argv) < 2 then (
    Buffer.output_buffer stdout b;
    flush stdout
  ) else (
    let channel : out_channel = open_out Sys.argv.(1) in
    Buffer.output_buffer channel b;
    close_out channel
  )
