/* x := 5;
begin
    proc f is (
        if !(x = 0) then (
            x := x - 1;
            call f
        ) else (
            skip
        )
    );
    call f;
    export x
end */

x := 5;
r := 10;
begin
    proc f is (
        if x = 5 then (
            r := 1
        ) else (
            r := 0
        )
    );
    call f;
    export r
end
