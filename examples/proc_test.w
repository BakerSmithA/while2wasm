begin
    proc f is (
        begin var y:=2;
            x:=y
        end
    );
    proc h is (
        begin var y:=3;
            z := 100
        end
    );
    call f;
    call h;
    export x
end
