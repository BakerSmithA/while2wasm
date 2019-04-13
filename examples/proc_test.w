begin
    proc f is (
        // y should be local to f
        begin var y := 2;
            // x should be global
            x := y
        end
    );
    call f;
    export x
end
