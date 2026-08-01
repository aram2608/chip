if "h" == "h" then
    print "yes";
else
    print "no";

a := 0;
b := 1;
r := 10;
for i := 2; i <= r; i = i + 1 {
    c := a + b;
    a = b;
    b = c;
}

print b;
