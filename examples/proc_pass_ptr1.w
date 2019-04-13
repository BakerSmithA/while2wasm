// Tests passing x as a pointer into f, i.e. passing into one function.
x := 0;
begin
    proc f is x := 1;
    call f;
    export x
end
