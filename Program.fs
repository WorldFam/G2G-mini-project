module Sprint02
// Sprint01

type DrinkType = |Coffee| Tea| Juice
                 
type DrinkSize = |Small| Medium| Large

type Drink = {drinkType : DrinkType ; size : DrinkSize; price : float}

let coffee1 = { drinkType =  Coffee; size = Small; price = 1.5}
let coffee2 = { drinkType =  Coffee; size = Medium; price = 2.0}  
let coffee3 = { drinkType =  Coffee; size = Large; price = 3.0}

let tea1 = { drinkType = Tea ; size = Small; price = 1.0}
let tea2 = { drinkType = Tea; size = Medium; price = 1.5}  
let tea3 = { drinkType = Tea ; size = Large; price = 2.0}

let juice1 = { drinkType = Juice ; size = Small; price = 1.5}
let juice2 = { drinkType =Juice ; size = Medium; price = 2.0}  
let juice3 = { drinkType = Juice ; size = Large; price = 2.5}

let drinkOrder = [coffee1, juice3, tea2, juice1]

type Order = | Order of Drink list

let rec totalOrderPrice (order : Order) =
    match order with
    | Order(items) -> items |> List.sumBy (fun product -> product.price)

//Sprint02

//float -> float 
let VAT = 25.0
let gtgVAT x =  VAT / 100.0 * x

//int -> int
let gtgVATInt x = VAT / 100.0 * (float x)

type OrderDrinkMsg = | OrderDrink of Drink * int //Drink, qty
                     | LeaveAComment of string  // ”Comment-super!”

let orderDrink = OrderDrink(coffee1, 4)
let orderDrink1 = OrderDrink(juice1, 2)

let orderComment = LeaveAComment("Greate coffe!")

//function for price
let calculateVATPrice (drink:Drink)=
    match drink.drinkType with
    |Coffee -> gtgVAT drink.price
    |_ -> 0.0

let gtgAgent = 
    MailboxProcessor.Start(fun inbox -> 
        let rec msgLoop = async{
            let! msg = inbox.Receive()
            match msg with
            | OrderDrink(drink, qty) -> 
            let VATPrice  = calculateVATPrice(drink) * float qty
            printfn "%i %A is %0.2f of which %0.2f is VAT. Grand Total = %0.2fDKK" 
                        qty
                        drink.drinkType
                        (drink.price * float qty)
                        VATPrice
                        (VATPrice + (drink.price * float qty))
            | LeaveAComment(comment) -> printfn "%A" comment
            }
        msgLoop)

let orderDrinks =
    [ 
      (OrderDrink({ drinkType = Coffee; size = Medium ; price = 1.0}, 4))
      (OrderDrink({ drinkType = Juice; size = Large; price = 2.5 }, 5)) ]

orderDrinks |> List.map (fun order -> gtgAgent.Post(order))
