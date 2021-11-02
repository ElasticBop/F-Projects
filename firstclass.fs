
//Start of HW3
type AccountStatus =
| Empty
| Balance of int
| Overdrawn of int

type BankAccount = { name: string; account: AccountStatus; creditLimit: int option}

let withdraw ba wa =
    match ba.account with
    | Overdrawn o -> (0, {name = ba.name; account = ba.account; creditLimit = ba.creditLimit})
    | Empty -> 
        match ba.creditLimit with
        | None -> (wa, {name = ba.name; account = Overdrawn wa; creditLimit = None})
        | Some cl when wa > cl -> (cl, {name = ba.name; account = Overdrawn cl; creditLimit = ba.creditLimit})
        | _ -> (wa, {name = ba.name; account = Overdrawn wa; creditLimit = ba.creditLimit})
    | Balance b when b > 0 ->
        match b with
        | b1 when b1 > wa -> (wa, {name = ba.name; account = Balance (b1 - wa); creditLimit = ba.creditLimit})
        | b2 when b2 = wa -> (wa, {name = ba.name; account = Empty; creditLimit = ba.creditLimit})
        | b3 when b3 < wa ->
            match ba.creditLimit with 
            | None -> (wa ,{name = ba.name; account = Overdrawn (wa - b3); creditLimit = None})
            | Some cl1 when (wa-b3) > cl1 -> ( cl1 + b3,{name = ba.name; account = Overdrawn cl1; creditLimit =  ba.creditLimit})
            | _ -> (wa ,{name = ba.name; account = Overdrawn (wa - b3); creditLimit = ba.creditLimit})
        | _ -> (0, {name = ba.name; account = ba.account; creditLimit = ba.creditLimit})
    | _ -> (0, {name = ba.name; account = ba.account; creditLimit = ba.creditLimit})


//Start of HW4
let isWealthy ba =
    match ba.account with
    | Balance b when b > 100000 -> true
    | _ -> false

//helper function to return true if account is overdrawn
let findOverdrawnHelper ba =
    match ba.account with
    | Overdrawn o -> true
    | _ -> false

let findOverdrawn baList = 
    List.filter findOverdrawnHelper baList

//helper function to convert bank account to int
let baToInt ba = 
    match ba.account with
    | Balance b -> b
    | Overdrawn o -> -o
    | Empty -> 0

let largerAmount ba1 ba2 =
    if baToInt ba1 < baToInt ba2 then
        ba2
    else
        //return ba1 if greater than or equal
        ba1
    
let accountAmounts baList =
    List.map baToInt baList

let amountsWhere func baList = 
    List.map baToInt (List.filter func baList)

//helper function to convert accountstatus to int
let accToInt acc = 
    match acc with
    | Balance b -> b
    | Overdrawn o -> -o
    | Empty -> 0

//helper function to combine two accounts and return new account
let combineHelper acc1 acc2 =
    let sum = (accToInt acc1) + (accToInt acc2)
    if sum > 0 then 
        Balance sum
    else if sum < 0 then
        Overdrawn -sum
    else 
        Empty

let combineAccounts asList = 
    List.reduce combineHelper asList

let wealthiestAccount baList = 
    List.reduce largerAmount baList



let neal = {name = "Neal Terrell"; account = Balance 100; creditLimit = None}
let dave = {name = "Dave Davidson"; account = Overdrawn 200; creditLimit = None}
let tom = {name = "Tom Thompson"; account = Balance 200000; creditLimit = Some(500)}
let jackie = {name = "Jackie Jackson"; account = Empty; creditLimit = None}

printf "%O\n" (isWealthy neal)
printf "%O\n" (isWealthy tom)
printf "%O\n" (findOverdrawn [neal; dave; tom;])
printf "%O\n" (largerAmount neal dave )
printf "%A\n" (accountAmounts [neal; dave; tom; jackie])
printf "%O\n" (amountsWhere isWealthy [neal; dave; tom; jackie])
printf "%O\n" (combineAccounts [Balance 100; Overdrawn 200; Empty; Balance 1100; Balance 100; Overdrawn 300])
printf "%O\n" (combineAccounts [Balance 100; Overdrawn 100; Empty])
printf "%O\n" (combineAccounts [Balance 200; Overdrawn 100; Empty; Overdrawn 300])
printf "%O\n" (wealthiestAccount [neal; dave; tom; jackie])


