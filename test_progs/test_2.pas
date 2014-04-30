program program2;
var I,N : integer;

procedure Fred(A: integer; var B: integer);
var j : integer;

   function Max(A :integer):integer;
   var i,result : integer;
   begin
      i := 5;
      if (i > A) then
         result := i
      else
         result := A;
      Max := result;
   end;

begin
   A := 10;

   if (A > 10) and (A < 15) then
      writeln('10 < A < 15')
   else
      writeln('A <= 10 or A >= 15');

   write('Enter value for A >> ');
   readln(A);
   while (A > 5) do
   begin
      write('A > 5');
      write('Enter value for A >> ');
      readln(A);
      writeln('');
   end;
   writeln('A <= 5');

   B := 2;
   for j := 0 to 10 do
   begin
      writeln(j);
      B := B + j;
   end;
   writeln('B = ',B,'\n');

   for j := 10 downto 0 do
   begin
      writeln(j);
      B := B - j;
   end;
   writeln('B = ',B,'\n');

   B := 4;
   A := Max(B);
   writeln('the max of ',B,' and 5 is ',A,'\n');

   I := (I+A)*B;
   write('the procedure''s result is: ',I,' ',A,' ',B,'\n');
end;

begin
   write('Enter a number >> ');
   readln(I);
   writeln('Results:');
   N := 1;
   write(I,' ',N,'\n');
   Fred(I,N);
   write(I,' ',N,'\n')
end.
