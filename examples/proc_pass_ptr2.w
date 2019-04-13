// Tests passing x as a pointer into f, i.e. passing through a function into another.
x := 0;
begin
    proc f is (
        begin
            proc g is (
                x := 1
            );
            call g
        end
    );
    call f;
    export x
end
