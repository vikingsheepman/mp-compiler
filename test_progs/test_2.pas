program program2;
var I,N : integer;
      
procedure Fred(A: integer; var B: integer);
var j : integer;
begin 
   A := 10;       

   if (A = 10) and (I > 5) then
      writeln('A is >= to 10')
   else
      writeln('statement was false');

   while (A > 5) do
   begin
      write('Enter value for A >> ');
      readln(A);
      writeln('');
   end;
   
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