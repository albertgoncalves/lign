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
    match (
      if x < 0 then (
        (x mod 7) + 7
      ) else if 6 < x then (
        x mod 7
      ) else (
        x
      )
    ) with
    | 0 -> A
    | 1 -> B
    | 2 -> C
    | 3 -> D
    | 4 -> E
    | 5 -> F
    | 6 -> G
    | _ -> exit 1

  let interval (n : int) (x : t) : t =
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
  type t = (Pitch.t * Duration.t)

  let render (buffer : Buffer.t) (((tone, accidental), duration) : t) : unit =
    Tone.render buffer tone;
    Accidental.render buffer accidental;
    Duration.render buffer duration
end

module Arpeggio = struct
  type t = (Pitch.t * Pitch.t * Pitch.t)
end

let render (buffer : Buffer.t) (notes : Note.t Queue.t) : unit =
  Note.render buffer (Queue.take notes);
  Queue.iter (fun x -> Buffer.add_char buffer ' '; Note.render buffer x) notes

let () : unit =
  let notes : Note.t Queue.t = Queue.create () in
  List.iter
    (fun x -> Queue.add x notes)
    [
      ((G, Sharp), Quarter);
      ((F, Natural), Quarter);
      ((G, Natural), Quarter);
      ((A, Sharp), Eighth);
      ((B, Natural), Eighth);
      ((C, Sharp), Quarter);
      ((E, Natural), Quarter);
      ((F, Natural), Eighth);
      ((E, Flat), Eighth);
      ((E, Natural), Quarter);
    ];
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
melody = \relative c'' {
  \clef treble
  \time 4/4
  |};
  render buffer notes;
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
