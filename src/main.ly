\version "2.22.1"

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

  a4 g4 ais4 r8 b8 cis4 e4 f4 ees16 e8.
}

\score {
  \new Staff \melody
  \layout { }
  \midi { }
}
