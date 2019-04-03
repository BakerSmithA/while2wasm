n := 20;
a := 1;
b := 1;
while !n=1 do (
    t := b;
    b := a + b;
    a := t;
    n := n - 1
);
result := a;
export result
