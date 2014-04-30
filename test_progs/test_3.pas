program CompareSums;
var val1,val2,sum1,sum2 : integer;

    function Sum(i:integer):integer;
    var count,sum:integer;
    begin
        for count := 0 to i do
        begin
            sum := sum + count;
        end;
    end;

    function getPosNum(void:integer):integer;
    var result:integer;
    begin
        readln(result);
        writeln('');
        while (result < 0) do
        begin
            writeln('Number is not positive. Try again >> ');
            readln(result);
        end;
        getPosNum := result;
    end;

    procedure Compare(a,b:integer);
    var max,min:integer;

        function getMax(x,y:integer):integer;
        var result:integer;
        begin
            if x > y then
                result := x
            else
                result := y;
            getMax := result;
        end;

    begin
        max := getMax(a,b);
        if max = a then
            min := b
        else
            min := a;
        writeln(max,' is greater than ',min);
    end;

begin
    writeln('Enter a positive number for val1 >> ');
    val1 := getPosNum(0);
    sum1 := Sum(val1);

    writeln('Enter a positive number for val2 >> ');
    val2 := getPosNum(0);
    sum2 := Sum(val2);

    writeln('The sum of numbers from 0 to ',val1,' is ',sum1);
    writeln('The sum of numbers from 0 to ',val2,' is ',sum2);

    Compare(sum1,sum2);

    writeln('End of test program.');
end.
