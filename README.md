# lisp-in-small-pieces
Working through the Book Lisp In Small Pieces...

In general, I'm trying to write interpreters and exercises in my favorite programming languages: Rust, Python, and Scheme. The language chosen for for particular tasks depends mostly on whim.


Random notes:
  - Of course it's easiest to use Scheme because one can simply rely on the host language for certain features. In particular, guaranteed tail call optimization, S-expression parsing, dynamic types, and garbage collection are super useful. Python shares many of these features but it really lacks tail calls. Rust requires most boilerplate code, mostly because dynamic types and memory management must be made explicit. Once the boilerplate is written and tucked away into modules the resulting interpreters look pretty similar in all languages.
