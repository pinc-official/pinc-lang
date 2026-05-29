  $ NO_COLOR="1" print . C
  <section>
  Base_String.uppercase_ascii("abcàäöüß"): ABCàäöüß
  Base_String.lowercase_ascii("ABCÁÄÖÜß"): abcÁÄÖÜß
  Base_String.capitalize_ascii("abc"): Abc
  Base_String.capitalize_ascii("äöü"): äöü
  
  char: c
  char + 1: d
  char < 32: false
  char > 32: true
  
  !
  !
  \
  
  
  '
  	
  ©
  ǿ
  😡
  </section>

  $ pincfmt ./data.pi
  component C {
    let char = 'c';
  
    <section>
      Base_String.uppercase_ascii("abcàäöüß"): {Base_String.uppercase_ascii("abcàäöüß")}
      Base_String.lowercase_ascii("ABCÁÄÖÜß"): {Base_String.lowercase_ascii("ABCÁÄÖÜß")}
      Base_String.capitalize_ascii("abc"): {Base_String.capitalize_ascii("abc")}
      Base_String.capitalize_ascii("äöü"): {Base_String.capitalize_ascii("äöü")}
  
      char: {char}
      char + 1: {char + 1}
      char < 32: {char < 32}
      char > 32: {char > 32}
  
      {' ' + 1}
      {' ' + 1}
      {'\\'}
      {'\n'}
      {'\''}
      {'\t'}
      {'\xA9'}
      {'\o777'}
      {'\033' + 128512}
    </section>
  }
  
