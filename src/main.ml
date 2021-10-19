let modulo (x : int) (n : int) =
  if x < 0 then
    (x mod n) + n
  else if n <= x then
    x mod n
  else
    x

module Tone = struct
  type t = A | B | C | D | E | F | G

  let render (buffer : Buffer.t) (tone : t) : unit =
    Buffer.add_char
      buffer
      (match tone with
       | A -> 'a'
       | B -> 'b'
       | C -> 'c'
       | D -> 'd'
       | E -> 'e'
       | F -> 'f'
       | G -> 'g')

  let to_int : t -> int =
    function
    | A -> 0
    | B -> 1
    | C -> 2
    | D -> 3
    | E -> 4
    | F -> 5
    | G -> 6

  let from_int (x : int) : t =
    match modulo x 7 with
    | 0 -> A
    | 1 -> B
    | 2 -> C
    | 3 -> D
    | 4 -> E
    | 5 -> F
    | 6 -> G
    | _ -> raise Exit

  let step (x : t) (n : int) : t =
    from_int ((to_int x) + n)
end

module Accidental = struct
  type t = Flat | Natural | Sharp

  let render (buffer : Buffer.t) : t -> unit =
    function
    | Flat -> Buffer.add_string buffer "es"
    | Natural -> ()
    | Sharp -> Buffer.add_string buffer "is"
end

module Pitch = struct
  type t = (Tone.t * Accidental.t)

  let to_int : t -> int =
    function
    | (C, Natural) -> 0
    | (C, Sharp) | (D, Flat) -> 1
    | (D, Natural) -> 2
    | (D, Sharp) | (E, Flat) -> 3
    | (E, Natural) | (F, Flat) -> 4
    | (E, Sharp) | (F, Natural) -> 5
    | (F, Sharp) | (G, Flat) -> 6
    | (G, Natural) -> 7
    | (G, Sharp) | (A, Flat) -> 8
    | (A, Natural) -> 9
    | (A, Sharp) | (B, Flat) -> 10
    | (B, Natural) | (C, Flat) -> 11
    | _ -> raise Exit

  let interval (steps : int) (semitones : int) ((tone0, _) as pitch0 : t) : t =
    let semitones : int = modulo semitones 12 in
    let tone1 : Tone.t = Tone.step tone0 steps in
    let pitch1 : t = (tone1, Natural) in
    let n : int = modulo ((to_int pitch1) - (to_int pitch0)) 12 in
    if semitones = n then
      pitch1
    else if (modulo (semitones + 1) 12) = n then
      (tone1, Flat)
    else if (modulo (semitones - 1) 12) = n then
      (tone1, Sharp)
    else
      raise Exit

  let minor_third_below : t -> t =
    interval (-2) (-3)

  let minor_third_above : t -> t =
    interval 2 3

  let major_third_below : t -> t =
    interval (-2) (-4)

  let major_third_above : t -> t =
    interval 2 4

  let perfect_fifth_below : t -> t =
    interval (-4) (-7)

  let perfect_fifth_above : t -> t =
    interval 4 7

  let diminished_fifth_above : t -> t =
    interval 4 6

  let augmented_fifth_above : t -> t =
    interval 4 8

  let test () : unit =
    assert ((interval 0 (-1) (C, Natural)) = (C, Flat));
    assert ((interval 0 0 (C, Natural)) = (C, Natural));
    assert ((interval 0 1 (C, Natural)) = (C, Sharp));
    assert ((interval 1 1 (B, Natural)) = (C, Natural));
    assert ((interval (-1) (-1) (B, Natural)) = (A, Sharp));
    assert ((minor_third_above (C, Natural)) = (E, Flat));
    assert ((major_third_above (C, Natural)) = (E, Natural));
    assert ((minor_third_below (C, Natural)) = (A, Natural));
    assert ((major_third_below (C, Natural)) = (A, Flat));
    assert ((major_third_below (D, Natural)) = (B, Flat));
    assert ((minor_third_below (D, Natural)) = (B, Natural));
    assert ((minor_third_above (E, Flat)) = (G, Flat));
    assert ((major_third_above (E, Flat)) = (G, Natural));
    assert ((minor_third_above (E, Natural)) = (G, Natural));
    assert ((major_third_above (E, Natural)) = (G, Sharp));
    assert ((minor_third_above (A, Flat)) = (C, Flat));
    assert ((major_third_above (A, Flat)) = (C, Natural));
    assert ((minor_third_above (A, Natural)) = (C, Natural));
    assert ((major_third_above (A, Natural)) = (C, Sharp));
    assert ((minor_third_above (A, Sharp)) = (C, Sharp));
    assert ((perfect_fifth_below (B, Flat)) = (E, Flat));
    assert ((perfect_fifth_above (B, Natural)) = (F, Sharp));
    assert ((diminished_fifth_above (B, Natural)) = (F, Natural));
    assert ((augmented_fifth_above (C, Natural)) = (G, Sharp));
end

module Octave = struct
  type t = int

  let render (buffer : Buffer.t) (octave : t) : unit =
    if octave < 0 then (
      for _ = -1 downto octave do
        Buffer.add_char buffer ','
      done
    ) else if 0 < octave then (
      for _ = 1 to octave do
        Buffer.add_char buffer '\''
      done
    ) else (
      ()
    )
end

module Duration = struct
  type t = Quarter | Eighth

  let render (buffer : Buffer.t) (duration : t) : unit =
    Buffer.add_char
      buffer
      (match duration with
       | Quarter -> '4'
       | Eighth -> '8')
end

module Note = struct
  type t = (Pitch.t * Octave.t * Duration.t)

  let render
      (buffer : Buffer.t)
      (((tone, accidental), octave, duration) : t) : unit =
    Tone.render buffer tone;
    Accidental.render buffer accidental;
    Octave.render buffer octave;
    Duration.render buffer duration
end

module Triad = struct
  type t = (Pitch.t * Pitch.t * Pitch.t)

  let minor (pitch : Pitch.t) : t =
    (pitch, Pitch.minor_third_above pitch, Pitch.perfect_fifth_above pitch)

  let major (pitch : Pitch.t) : t =
    (pitch, Pitch.major_third_above pitch, Pitch.perfect_fifth_above pitch)

  let diminished (pitch : Pitch.t) : t =
    (pitch, Pitch.minor_third_above pitch, Pitch.diminished_fifth_above pitch)

  let augmented (pitch : Pitch.t) : t =
    (pitch, Pitch.major_third_above pitch, Pitch.augmented_fifth_above pitch)
end

module Arpeggio = struct
  type t = Pitch.t array

  let from_triad ((root, third, fifth) : Triad.t) : t =
    Array.init 3
      (function
        | 0 -> root
        | 1 -> third
        | 2 -> fifth
        | _ -> raise Exit)

  let cycle (n : int) (x : t) (i : int) : int =
    modulo (i + n) (Array.length x)

  let random_walk () : t -> int -> int =
    if Random.bool () then
      cycle (-1)
    else
      cycle 1
end

let generate
    (notes : Note.t Queue.t)
    (arpeggio : Arpeggio.t)
    (n : int) : unit =
  let i : int ref = ref (Random.int (Array.length arpeggio)) in
  for _ = 1 to n do
    Queue.add (arpeggio.(!i), 1, Duration.Eighth) notes;
    i := Arpeggio.random_walk () arpeggio (!i)
  done

let render (buffer : Buffer.t) (notes : Note.t Queue.t) : unit =
  Note.render buffer (Queue.take notes);
  Queue.iter (fun x -> Buffer.add_char buffer ' '; Note.render buffer x) notes

let () : unit =
  Pitch.test ();
  let buffer : Buffer.t = Buffer.create (1 lsl 7) in
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
  |};
  (
    Random.self_init ();
    let notes : Note.t Queue.t = Queue.create () in
    generate notes (Arpeggio.from_triad (Triad.minor (F, Sharp))) 16;
    render buffer notes;
  );
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
