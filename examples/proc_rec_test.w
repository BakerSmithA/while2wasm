x := 1;
begin
    proc f is (
        if x = 10 then (
            skip
        ) else (
            x := x + 1;
            call f
        )
    );
    call f;
    export x
end
