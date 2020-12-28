// Михалев Сергей, ИУ5Ц-52Б
open System


let Func1 (a:int, b:int, c:int) =
   let s = a+b+c
   let p = a*b*c
   (s, p)

let Func2 (a:int)(b:int)(c:int) = 
   let s = a+b+c
   let p = a*b*c
   (s, p)

let rec Factorial(n:int):int =
   if n<=1 then 1
   else n*Factorial(n-1)

let rec Factorial2(n:int, acc:int):int =
   if n=1 then acc
   else Factorial2(n-1, n*acc)

let rec FactorialNew n = Factorial2(n,1)

let rec State1(x:int) =
   printfn "%i - (+1) %i" x (x+1)
   let x_next = x+1
   if x_next>5 then State2(x_next)
   else State1(x_next)

and State2(x:int) =
   printfn "%i - (+10) %i" x (x+5)
   let x_next = x+1
   if x_next>10 then State3(x_next)
   else State2(x_next)

and State3(x:int) =
   printfn "%i - (+100) %i" x (x+10)
   let x_next = x+1
   if x_next<=15 then State3(x_next)

let sum (a:int, b:int, c:int, func1: int*int*int->int) = func1 (a, b, c)

let sumK (a:int, b:int, c:int, func1: int->int->int->int) = func1 a b c




[<EntryPoint>]
let main argv =
   let resultKor = Func1(2, 4, 3)
   let resultCar = Func2(2)(4)(3)
   printfn "%s" ("Результаты функции, реализованной через кортеж и в каррированном виде, соответсвенно: " + resultKor.ToString() + " и " + resultCar.ToString())

   let resRec = Factorial(6)
   printfn "%s" ("\nРезультат рекурсивной функции - вычисление факториала (6): " + resRec.ToString())

   let resRecX = FactorialNew(4)
   printfn "%s" ("\nРезультат хвостовой рекурсивной функции - вычисление факториала (4): " + resRecX.ToString())

   printfn "%s" ("\nПример автомата из трех состояний:")
   State1(1)


   let primer1 = sum(2, 3, 5, fun(a, b, c)->a+b+c)
   let primer2 = sumK(2, 3, 5, fun a b c -> a+b+c)
   printfn "%s" ("\nПример результата работы функции, которая принимает 3 целых числа и лямбда-выражение для их суммирования в виде кортежа и в каррированном виде соответсвенно: " + primer1.ToString() + " и " + primer2.ToString())

   0 
