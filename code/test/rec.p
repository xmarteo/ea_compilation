program

procedure count (x : integer);
begin
   writeln(x);
   if x > 0 then
      count (x - 1)
end; { count }

begin
   count(10)
end.
