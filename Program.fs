// Learn more about F# at http://fsharp.org

open System

/// Tail recursive Fibonacci
let fib (n: int) =
    let rec rFib (n: int) acc =
        if n>1 then 
            //printfn "n: %i acc: %i" n acc
            rFib (n-1) (acc+n)
        else
            acc
    rFib n 1

// 1. function that determines whether a number is even or odd
let IsEven n =
    if n % 2  = 0 then
        true
    else
        false
    
// 2. Filter a list of numbers to find all even ones.
let list = [1..100]
let FindEvenNumbers list = [| for a in list do if a % 2 = 0 then a |] |> Array.toList


// 3. Calculate the sum of the first 100 even numbers.
let rec CalcSum list =
    match list with
    | head :: tail -> 
        //printfn "%d " head 
        head + CalcSum tail
    | [] -> 0 
    

// 4. 
let sqrlist = [1..100]
let s (n: int) =
    let rec ss (n: int) acc =
        if n > 1 then 
            ss (n-1) 
                (
                if n % 2 = 0 then
                   acc+(n*n)
                else
                   acc-(n*n)
                )
        else
            acc
    ss n 1

// 4. Listával
let DiffSumSqr list = List.fold (fun acc n -> if n % 2 = 0 then acc + (n*n) else acc - (n*n)) 0 list

// 5a. Write a function that computes every other character in a string. Example: "Hello" -> ["H"; "l"; "o"]. (2 points)
// With all characters
let seq5a string = seq { for a in string do yield a}  

// 5b. 
let mutable c = 0
let seq5b string = seq { for a in string do 
                            c <- c + 1    
                            if c % 2 = 1 then yield a }    

// 6. Use the Sieve of Erathostenes to find all prime numbers from 2 to a given number. Example: FindAllPrimesUpTo 10
//  Írjuk fel a számokat 1-től n-ig, (itt például 100-ig) egyesével.
//  Keressük meg az első olyan 1-től nagyobbat, amelyik még nincs sem kihúzva, sem megjelölve. Elsőként ez a 2.
//  Ezután húzzuk ki ennek többszöröseit, de őt pedig jelöljük meg.
//  Ismételd meg a második lépéstől újra az eljárást. Természetesen egy összetett szám többször is kihúzásra kerülhet.
//  Az algoritmus akkor álljon le, ha a második lépésnél talált szám négyzete már nagyobb, mint n.        

// This is not my solution, but i understand at least

let IsPrimeMultipleTest n x =
    x = n || x % n <> 0

let rec RemoveAllMultiples listn listx =
    match listn with
    | head :: tail -> RemoveAllMultiples tail (List.filter (IsPrimeMultipleTest head) listx)
    | [] -> listx

let FindAllPrimesUpTo n =
    let max = int (sqrt (float n))
    RemoveAllMultiples [ 2 .. max ] [ 1 .. n ]

printfn "Primes Up To %d:\n %A" 100 (FindAllPrimesUpTo 100)



type Person =
    {
        firstName: string
        lastName: string
        age: int
    }

// 9. Find the sum of all people's ages. 
let people1 : Person = { firstName = "László" ; lastName = "Kincses" ; age = 50 }
let people2 : Person = { firstName = "Viktória" ; lastName = "Kincses" ; age = 24 }
let people3 : Person = { firstName = "Angéla" ; lastName = "Kincses" ; age = 21 }
let people4 : Person = { firstName = "Márton" ; lastName = "Kincses" ; age = 20 }
let people = [ people1; people2; people3; people4 ]

let sumage = List.fold ( 
                fun acc people -> match people.age with
                                | _ -> acc + people.age
                                ) 0 people
