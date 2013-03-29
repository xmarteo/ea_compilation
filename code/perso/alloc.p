program

var len : integer ;
t : array of integer ;

begin
  len := 42 ;
  writeln(len) ;
  t := new array of integer [len] ;
  len := cast(t, integer) ;
  writeln(len) ;
  t[1] := 1
end.
