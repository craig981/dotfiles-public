c++-mode c-mode

(main
 "#include <iostream>" n n
 "using std::cerr;" n n
 "int main(int argc, const char *argv[]) {" n n
 > q n n
 > "return 0;" n "}" n)

(io
 "#include <iostream>" n n
  "using std::cerr;" n)

(inc_user
  "#include \"" q "\"")

(inc_sys
  "#include <" q ">")

(for_int
  "for ("
  (p "int") " "
  (p "i" var) "=0; "
  var "<"
  (p "n") "; ++" var ") {" n
  > q n
  > "}" n)

(for_stl
  "for (\t"
  (p "std::vector<int>") "::"
  (p "const_iterator") n
  "\t\t" (s it) "=" (s vec) ".begin(); " it "!=" vec ".end(); ++" it n
  "\t) {" n
  > q n
  > "}" n)

(print_vec
  "<<" n
  > (s var) "[0] << \" \" <<" n
  > var "[1] << \" \" <<" n
  > var "[2] << \" \" << " q)
