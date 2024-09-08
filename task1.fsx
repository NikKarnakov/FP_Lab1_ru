open System //Подключаем, чтобы пользоваться библиотекой Math для вычисления шинуса

let var6 = fun (x: float) -> Math.Sinh(x)

//Инициализируем имена, так как в F# нет понятия переменных
let a = 0.0  
let b = 1.0
let n = 10
let epsilon = 1e-10  //небольшое значение eps

//Итеративная функция возведения в степень (не использую хвостовую рекурсию, так как возникло переполнение стека (спасибо stackoverflow за помощь),
//но не знаю насколько это верно, но проблему на удивление решило)
let pow (number: float) (grade: int) = 
    let mutable result = 1.0
    for i = 1 to grade do
        result <- result * number
    result

//p.s. mutable - тип имени в F#, который может менять своё значение после присвоения начального значения

//Итеративная функция для вычисления факториала (опять же, отказался от идеи реализации в виде хвостовой рекурсии)
let iterative_factorial n = 
    let mutable result = 1
    for i = 1 to n do
        result <- result * i
    result

//sh x = x^(2n-1)/(2n-1)!
let scric (x: float) (n: int)  =  
    let chisl = pow x (2 * n + 1)
    let znamel = float (iterative_factorial (2 * n + 1))
    chisl / znamel

//Наивный способ
let taylorN x =
    let mutable sum = 0.0
    let mutable curr_elem = scric x 0
    let mutable n = 0
    while abs(curr_elem) > epsilon do
        sum <- sum + curr_elem
        n <- n + 1
        curr_elem <- scric x n
    (sum, n)

//Разность элементов для умного метода
let raznres (x : float) (n : int) = x ** 2 / float((2 * n + 1) * (2 * n + 2))

//Умный способ
let taylor x = 
    let mutable sum = 0.0
    let mutable curr_elem = scric x 0
    let mutable n = 0
    while abs(curr_elem) > epsilon do
        sum <- sum + curr_elem
        n <- n + 1
        curr_elem <- curr_elem * raznres x (n - 1)
    (sum, n)

//Нужно для того, чтобы вычленить результаты из кортежа (типа (sum, n))
let first (left, right) = left
let second (left, right) = right

//Выводим красивую таблицу
let main =
    printfn "---------------------------------------------------------------------------------"
    printfn "|  x  |    Builtin    |  Smart Taylor  |    #terms    |   Dumb Taylor  | #terms |"
    printfn "---------------------------------------------------------------------------------"
    for i = 0 to n do
        let x = a + (float i) / (float n) * (b - a)
        let resN = taylorN x
        let res = taylor x
        printfn "|%5.2f|  %10.6f  |  %10.6f  |   %10d  |  %10.6f  |   %10d  |" 
            x (var6 x) (first res) (second res) (first resN) (second resN)
    printfn "----------------------------------------------------------------------------"

main
