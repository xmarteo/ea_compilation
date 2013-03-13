program

var len, n, i : integer ;
t : array of array of integer ;

begin
  len := 42 ;
  writeln(len) ;
  t := new array of array of integer [len] ;
  n := length(t) ;
  writeln(n) ;
  i := 0 ;
  while i < len do begin
    t[i] := new array of integer [i] ;
    n := length(t[i]) ;
    i := i+1 ;
    writeln(n)
  end ;
  n := length(t) ;
  writeln(n)
end.