program

var len, n, i : integer ;
t : array of integer ;

begin
  len := 42 ;
  writeln(len) ;
  t := new array of integer [len] ;
  n := length(t) ;
  writeln(n) ;
  i := 0 ;
  while i < len do begin
    t[i] := 13 ;
    i := i+1
  end ;
  n := length(t) ;
  writeln(n)
end.