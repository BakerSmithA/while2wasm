begin var y := 1; (
    x := 1;
    begin var x := 2;
        y := x + 1
    end;
    x := y + x;
    export x
)
end
