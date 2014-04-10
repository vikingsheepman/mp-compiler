program program2;
  var I,N: integer;
      
  procedure Fred(A: integer; var B: integer);
    var I: integer;
    begin
      I := 5;       
      A := 10;       
      B := 20;       
      write(I,' ' ,A,' ' , B ,'\n' );     
      end;
      
  begin    
    I := 0;     
    N := 1;     
    write(I,' ' ,N,'\n' );     
    Fred(I,N);     
    write(I,' ' ,N, '\n' )   
    end.