program

var x : integer;

{ factorial }
function f (n : integer) : integer;
var y : integer;
begin		       
   if n <= 0 then
      f := 1
   else
    begin
      writeln(n);
      y := f (n - 1);
      f := n * y
    end
end; { factorial }

begin
   x := readln();
   writeln (f(x))
end.
