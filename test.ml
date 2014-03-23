open Batteries
open Printf
open OUnit
open Value

let echo env = function
  | [_,arg] -> env,arg
  | _     -> invalid_arg "env"


let exec str expect () =
  (* ignore(Parsing.set_trace true); *)
  let ast = Parser.statement Lexer.tokens (Lexing.from_string str) in
  let env = Env.empty
  |> Break.init
  |> LibStd.load in

  let _,res = Executor.run env ast in
  let msg = sprintf "%s <> %s\n"
    (Value.to_string res) (Value.to_string expect) in
  assert_equal ~msg res expect


let suite =
  "Expr" >::: [

    "ar1" >:: exec "echo (2 * 2 + (2 * 3 - 5));" (number.inj 5.);
    "ar2" >:: exec "echo (2 * 2 = 2 + 2);" (boolean.inj true);
    "ar3" >:: exec "echo (2 * 2 - 2);" (number.inj 2.);
    "floats" >:: exec "echo ((pi / 2)**2 < 1e6 / 1e3); " (boolean.inj true);
    "as1" >:: exec
"{
    a <- 1;
    b <- 2;
    echo ($a + $b);
}" (number.inj 3.);

    "quoted symbols" >:: exec
"{
   \"hello world\" <- bingo;
   echo $\"hello world\";
}" (symbol.inj "bingo");

    "nesting" >:: exec
"{
    a <- 1;
    b <- 2;
    {
      a <- 3;
      b <- 4;
      echo ($a + $b);
    }
}" (number.inj 7.);

    "app1" >:: exec
"{
  def sum (x,y) {
    return $x + $y;
  }
  b <- 3;
  a <- 4;
  echo ($(sum $b $a) - 2);
}" (number.inj 5.);

    "recursion" >:: exec
"{
  def acc (init,n) {
    if ($n > 0)
      return $(acc ($init + 1) ($n - 1));
    else
      return $init;
  }

  acc 0 4;
}" (number.inj 4.);
    "symbolic-compare" >:: exec
"{
  d <- antenna.circular;
  b <- antenna;

  echo (not ($d :>? $b));
  }" (boolean.inj false);

    "symbolic-compare" >:: exec
"{
  d <- circular;
  b <- antenna;

  echo ($b :: $d = antenna.circular);
}" (boolean.inj true) ;


    "diff function" >:: exec
"{
  echo (8 - 7);
}" (number.inj 1.);

    "diff function" >:: exec
"{
   def square_diff (a,b) {
     return $a**2 - $b**2;
   }

   square_diff 5 4;
  
}" (number.inj 9.);


    "definition with switches" >:: exec
"{
   def f (-s, x, y) {
     if ($s)
       return $x + $y;
     else
       return $x - $y;
   }

   echo ($(f 3 2) + $(f -s 3 2));
}" (number.inj 6.) ;

   "definition with optionals" >:: exec
"{
   def f (-v=4, x, y) {
     return $x + $y - $v;

   }
   echo ($(f 3 2) + $(f 3 2));
}" (number.inj 2.) ;

   "definition with optionals/override" >:: exec
"{
   def f (-v=4, x, y) {
     return $x + $y - $v;

   }
   f -v=1 3 2 ;
}" (number.inj 4.) ;

   "definition with several interleaved optionals" >:: exec
"{
   def f (x, -a=1, y, -b=2, z, -c=3) {
       return ($x - $y - $z) * $a / ($b - $c);
   }
   echo ($(f 3 2 1) = 0 and $(f -a=2 3 2 1) = 0 and
   $(f 1 2 3) = 4 and $(f -a=2 1 2 3) = 8 and
   $(f -a=2 1 -c=0 2 3) = -4 and
   $(f -a=2 1 -c=0 2 -b=(-4) 3) = 2);
}" (boolean.inj true) ;


   "side effects" >:: exec
"{
   init <- 0;
   def create(amount) {
       init <-- $amount;
   }

   def withdraw(amount) {
       init <-- $init - $amount;
   }

   def deposit(amount) {
       init <-- $init + $amount;
   }

   create 100;
   withdraw 20;
   deposit 10;
   echo ($init = 90);

}" (boolean.inj true) ;

   "while cycle" >:: exec
"{
   n <-0; nmax <- 100; i <- 0;
   while ($n < $nmax) {
     i <-- $nmax;
     n <-- $n + 1;
   }
   echo ($i / $nmax);
}" (number.inj 1.) ;

   "while cycle with break" >:: exec
"{
   n <-0; nmax <- 100; i <- 0;
   while (true) {
     i <-- $nmax;
     n <-- $n + 1;
     when ($n >= $nmax)
       break;
   }
   echo ($i / $nmax);
}" (number.inj 1.) ;


   "nested while cycle with break" >:: exec
"{
   n <-0; nmax <- 1e3; i <- 0; j <- 0;
   while (not ($j = 1e3)) {
     while (true) {
       i <-- $nmax;
       n <-- $n + 1;
       when ($n >= $nmax)
         break;
     }
     j <-- $j + 1;
   }
   echo ($j * $i / $nmax);
}" (number.inj 1e3) ;

   "command w/o arguments" >:: exec
"{
  def rng() {
     return 42;
  }

  rng;
}" (number.inj 42.) ;

   "return/simple" >:: exec
"{
  def rng() {
     return 42;
     return 15;
  }

  rng;
}" (number.inj 42.) ;

   "return/nested" >:: exec
"{
  def rng() {
     {
       return 42;
     }
     return 15;
  }

  rng;
}" (number.inj 42.) ;

   "return/cycle" >:: exec
"{
  def rng() {
    n <- 0;
     while (true) {
       when ($n = 42)
         return $n;
       n <-- $n + 1;
     }
     return 15;
  }

  rng;
}" (number.inj 42.) ;

    "return/recursion" >:: exec
"{
  def acc (init,n) {
    if ($n > 0)
      return $(acc ($init + 1) ($n - 1));
    else
      return $init;
  }

  echo ($(acc 0 4) + $(acc 0 10));
}" (number.inj 14.);

   "return/nested function" >:: exec
"{
  def rng() {
    def urng() {
      return 0.42;
      return shit;
    }
    return $(urng) * 100;
    return 0;
  }

  rng;
}" (number.inj 42.) ;


   "array/create-access1" >:: exec
"{
  echo {1,2,3}[0];
}" (number.inj 1.) ;

   "array/create-access2" >:: exec
"{
  echo {1,2,3}[2];
}" (number.inj 3.) ;

   "array/create-access2" >:: exec
"{
  echo {1,2,3}[2];
}" (number.inj 3.) ;


   "array/access-expr" >:: exec
"{
  a <- {1,2,3};

  echo $a[2];
}" (number.inj 3.) ;


   "array/set" >:: exec
"{
  a <- {1,2,3};
  $a[2] <- 4;
  echo $a[2];
}" (number.inj 4.) ;


   "array/multi" >:: exec
"{
  a <- {{1,2,3},{4,5,6},{7,8,9}};
  $a[2][2] <- 0;
  echo $a[2][2];
}" (number.inj 0.) ;

   "array/size1" >:: exec
"{
  a <- {1,2,3,4};
  echo $(size $a);
}" (number.inj 4.) ;


   "array/size3" >:: exec
"{
  echo $(size {});
}" (number.inj 0.) ;

   "array/size4" >:: exec
"{
  a <- {};
  echo $(size $a);
}" (number.inj 0.) ;


   "array/cat1" >:: exec
"{
  a <- {1,2,3,4};
  b <- {5,6,7,8};
  echo $(size ($a @ $b));
}" (number.inj 8.) ;

   "array/cat2" >:: exec
"{
  a <- {1,2,3,4};
  b <- {5,6,7,8};
  c <- $a @ $b;
  echo $(size $c);
}" (number.inj 8.) ;

   "array/cat-and-modify" >:: exec
"{
  a <- {1,2,3,4};
  b <- {5,6,7,8};
  c <- $a @ $b;
  $c[3] <- 10;
  $c[5] <- 20;
  echo (($c[3] + $c[5]) - ($a[3] + $b[1]));
}" (number.inj 20.) ;


   "array/push" >:: exec
"{
  a <- {1,2,3};
  push $a 2;
  echo $a[3];
}" (number.inj 2.) ;

   "array/pop" >:: exec
"{
  a <- {1,3,2};
  n <- $(pop $a);
  echo ($(size $a) = $n);
}" (boolean.inj true) ;

   "array/idx expression get" >:: exec
"{
  a <- {1,3,2};
  echo $a[1+1];
}" (number.inj 2.) ;

   "array/idx expression get" >:: exec
"{
  a <- {1,3,2};
  $a[1+1] <- 4;
  echo $a[1+1];
}" (number.inj 4.) ;

   "seq" >:: exec
"{
  a <- {0:2:10};
  echo $(size $a);
}" (number.inj 6.) ;

   "seq/default" >:: exec
"{
  a <- {0:9};
  echo $(size $a);
}" (number.inj 10.) ;
   "seq/uneven" >:: exec
"{
  a <- {0:0.3:9};
  last <- $(size $a) - 1;
  v <- $a[$last];
  echo $v;
}" (number.inj 9.) ;

   "seq/negative" >:: exec
"{
  a <- {0:-1:-9};
  echo $(size $a);
}" (number.inj 10.) ;

   "seq/full-negative" >:: exec
"{
  a <- {-10:-1:-19};
  echo $(size $a);
}" (number.inj 10.) ;

   "seq/access" >:: exec
"{
  echo ({-10:-1:-19}[0] + 10);
}" (number.inj 0.) ;

   "for/sum" >:: exec
"{
  acc <- 0;
  for (i = {1,2,3}) {
    acc <-- $acc + $i;
  }

  echo $acc;
}" (number.inj 6.) ;

   "for/seq-sum1" >:: exec
"{
  acc <- 0;
  for (i = {1:7}) {
    acc <-- $acc + $i;
  }

  echo $acc;
}" (number.inj 28.) ;

   "for/seq-sum2" >:: exec
"{
  acc <- 0;
  for (i = {1:0.5:7}) {
    acc <-- $acc + $i;
  }

  echo $acc;
}" (number.inj 52.) ;


   "fold" >:: exec
"{
  def fold (f,v,seq) {
    v <- $v;
    for (e = $seq) {
       v <-- $($f $v $e);
    }
    return $v;
  }

  def sum(x,y) {return $x + $y;}
  fold sum 0 ({1:7});
  def sum(x,y) {return $x + $y;}
  fold sum 0 ({1:7});

}" (number.inj 28.) ;

   "closure" >:: exec
"{
  # this is a comment
  def create (init) {
     def withdraw (n) {
     # this is another comment
        init <-- $init - $n;
        return $init;
     }
     return $withdraw;
  }

  get <- $(create 100);
  cash <- $(get 42);
  cash <- $(get 16);
  echo $cash;

}" (number.inj 42.) ;

   "closure/with side effects" >:: exec
"{
  overdraft <- 100;
 
  def create (init) {
     def withdraw (n) {
        if ($overdraft + ($init - $n) < 0)
          return 0;
        else {
          init <-- $init - $n;
          return $n;
        }
     }
     return $withdraw;
  }

  get <- $(create 0);
  cash <- $(get 42);
  cash <- $(get 18);
  overdraft <- 50;
  cash <- $(get 20);
  echo $cash;

}" (number.inj 0.) ;


   "closure/object" >:: exec
"{
  overdraft <- 100;
 
  def create (init) {
     def withdraw (n) {
        if ($overdraft + ($init - $n) < 0)
          return 0;
        else {
          init <-- $init - $n;
          return $n;
        }
     }

     def deposit(n) {
       init <-- $init + $n;
     }

     def amount() {
       return $init;
     }

     def dispatch(op) {
         return ${$op};
     }
     return $dispatch;
  }

  account <- $(create 0);
  put <- $(account deposit);
  get <- $(account withdraw);
  amount <- $(account amount);
  cash <- $(get 42);
  cash <- $(get 18);
  put 100;
  cash <- $(get 100);
  dep <- $(amount);
  echo ($cash + $dep);
}" (number.inj 40.) ;

   "copy function" >:: exec
"{
  a <- {1,2,3,4,5};
  b <- $(copy $a);
  $b[0] <- 0;
  echo $a[0];
}" (number.inj 1.) ;



   "some function" >:: exec
"{
    echo (7-7);
}" (number.inj 0.) ;

   "currying" >:: exec
"{
   def f(x) {
       def g(x') {
           return $x + $x';
       }
 }

 $(f 2) 3;
}" (number.inj 5.) ;

   "currying with side effect" >:: exec
"{
   def f(x) {
       def g(x') {
           x <-  $x;
           x <-- $x';
           return $x;
       }
       return $g;
 }
 h <- $(f 2);
 a <- $(h 3);
 b <- $(h 4);
 echo ($a + $b);
}" (number.inj 5.) ;

    "struct" >:: exec
"{
  def person (first, last, age) {
      def get(field) {
          return ${$field};
      }
      return $get;
  }

  marry <- $(person \"Marry\" \"Poppins\" 24);
  echo $(marry age);
}" (number.inj 24.);


  ]
