
open Printf

(* question 4.1 *)
let rec fibo n = 
  if n<0 then failwith "n est neg"
  else if n=0 then 0 else if n=1 then 1 
  else ((fibo (n-1))+(fibo (n-2)))

(* question 4.2
count(fibo(0)) = 0    with count = 0 
count(fibo(1)) = 1    with count = 1 
count(fibo(2)) = 1    with count = 2 
count(fibo(3)) = 2    with count = 5 
count(fibo(4)) = 3    with count = 10 
count(fibo(5)) = 5    with count = 19 
count(fibo(6)) = 8    with count = 34 
count(fibo(7)) = 13   with count = 59 
count(fibo(8)) = 21   with count = 100 
count(fibo(9)) = 34   with count = 167 
count(fibo(10)) = 55  with count = 276 
count(fibo(11)) = 89  with count = 453 
count(fibo(12)) = 144 with count = 740 
count(fibo(13)) = 233 with count = 1205 
count(fibo(14)) = 377 with count = 1958 
count(fibo(15)) = 610 with count = 3177 


la croissance de fibo est : O(2^n) 
  complixite exponentielle car deux appel recursive  
avec C(n) le nombre d'appel a fibo pour n on a : 
C(n)= C(n-1)+ C(n-2)

*)
let count = ref 0 ;;
let rec fibocount n = 
  incr count;
  if n<0 then failwith "n est neg"
  else if n=0 then 0 else if n=1 then 1 
  else ((fibocount (n-1))+(fibocount (n-2)))


(* quesiton 4.3*)
let t = Array.make 1024 (-1);;
let memocount = ref 0 ;;
let rec memofibocount n = 
  incr memocount;
  if n<0 then failwith "n est neg"
  else if n=0 then 0 else if n=1 then 1 
  else (fibocheck n t)
and fibocheck n t = 
  if t.(n)<0 then begin 
    t.(n) <- (memofibocount (n-2)+memofibocount (n-1)) ; 
    t.(n)
  end 
  else t.(n) 



(* question 4.4 
   
count(memofibo(58)) = 591286729879 with count = 170 
count(memofibo(59)) = 956722026041 with count = 173 
count(memofibo(60)) = 1548008755920 with count = 176 
count(memofibo(61)) = 2504730781961 with count = 179 
count(memofibo(62)) = 4052739537881 with count = 182 
count(memofibo(63)) = 6557470319842 with count = 185 
count(memofibo(64)) = 10610209857723 with count = 188 


on C(n)= C(n-1)+3 
alors C(n)= 3n-4 alors O(n)
c'est lineaire 



*)
  


(* tests*)
let ()=
  for i=0 to 25 do 
    printf "count(fibo(%d)) = %d with count = %d \n" (i) (fibocount i) (!count);
  done;
  printf " avec memo \n";
  for i=0 to 25 do 
    printf "count(memofibo(%d)) = %d with count = %d \n" (i) (memofibocount i) (!memocount);
  done
