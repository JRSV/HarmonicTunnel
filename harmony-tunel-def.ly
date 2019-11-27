\include "english.ly"

\paper { 
  #(set-paper-size "A4")
  page-limit-inter-system-space = ##t
  top-margin = 10\mm
  bottom-margin = 10\mm
  left-margin = 20\mm
  line-width = 17\cm
}

#(set-global-staff-size 14)


\include "/Users/admin/slipperychicken/sc/src/lilypond.ly"

global = {
\key c \major 
  \numericTimeSignature
}


playerone = \new Voice  { 
  \set Staff.instrumentName = \markup { piano }
  \set Staff.shortInstrumentName = \markup { pno } 
  \compressFullBarRests
  \override Score.BarLine #'hair-thickness = #0.5
  \include "harmony-tunel-playerone.ly"
}

music = {
  <<
  \new StaffGroup <<
    \tag #'score \tag #'playerone \new Staff = "playerone"
    { << \global #(set-accidental-style 'modern) \playerone >> }
  >>
  >>
}
written = {
  <<
  >>
}