  $ NO_COLOR="1" print . C
  <section>
  String.uppercase_ascii("abcàäöüß"): ABCàäöüß
  String.lowercase_ascii("ABCÁÄÖÜß"): abcÁÄÖÜß
  String.capitalize_ascii("abc"): Abc
  String.capitalize_ascii("äöü"): äöü
  
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
  component C() {
    use Base;
  
    let char = 'c';
  
    <section>
      String.uppercase_ascii("abcàäöüß"): {String.uppercase_ascii("abcàäöüß")}
      String.lowercase_ascii("ABCÁÄÖÜß"): {String.lowercase_ascii("ABCÁÄÖÜß")}
      String.capitalize_ascii("abc"): {String.capitalize_ascii("abc")}
      String.capitalize_ascii("äöü"): {String.capitalize_ascii("äöü")}
  
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
