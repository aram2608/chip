proc fib(r) {
   if r <= 1 then return r;

   a := 0;
   b := 1;
   for i := 2; i <= r; i = i + 1 {
        c := a + b;
        a = b;
        b = c;
   }

   return b;
}

if 0 then print (1 + 4) * 2; else print "nope again";
