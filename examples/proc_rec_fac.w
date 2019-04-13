begin
    var n := 6; var r := 1;
    proc fac is (
        if !n=1 then (
            r := r * n;
            n := n - 1;
            call fac
        ) else (
            skip
        )
    );
    call fac;
    export r
end
