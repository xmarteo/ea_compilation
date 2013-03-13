program

var n : integer ;
t : array of integer ;

begin

n := 42 ;
writeln(n) ;
t := new array of integer [6] ;
n := length(t) ;
writeln(n) ;
n := cast(t, integer) ;
writeln(n)

end.