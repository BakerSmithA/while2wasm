x := 1;
begin
    proc f is (
        while !x=5 do x := x + 1
    );
    call f;
    export x
end
