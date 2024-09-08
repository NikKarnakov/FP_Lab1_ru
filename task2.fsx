//Уравнения (начиная с варианта №6)
let sixth = fun (x: float) -> x + cos (x ** 0.52 + 2.)
let seventh = fun (x: float) -> 3. * (log x) ** 2. + 6. * log x - 5.
let eigth = fun (x: float) -> 0.6 * 3. ** x - 2.3 * x - 3.

//Дифференциалы для метода Ньютона (вычислены с помощью - https://math.semestr.ru/math/differential.php)
let sixthDiff = fun (x: float) -> 1. - 0.52 * sin (x ** 0.52 + 2.) * (x ** (-0.48))
let seventhDiff = fun (x: float) -> 6. * log x / x + 6. / x
let eigthDiff = fun (x: float) -> 0.6 * log 3. * 3. ** x - 2.3

//Фи-функции для метода простых итераций  (вычислены с помощью - https://math.semestr.ru/optim/iteration_method.php)
let sixthPhi = fun (x: float) -> -cos (x ** 0.52 + 2.)
let seventhPhi = fun (x: float) -> exp ((5. - 6. * log x) / 3.) 
let eigthPhi = fun (x: float) -> (0.6 * 3. ** x - 3.) / 2.3

//епсилон (заданная точность)
let epsilon = 0.1 ** 5

//Метод бисекции (источник - https://en.wikipedia.org/wiki/Bisection_method, дата обращение - 07.09.2024)
let bisection func (left : float) (right : float) = 
    let rec iter left right = 
        let mid = (left + right) / 2.
        if (func mid = 0.) || ((mid - left) < epsilon) then mid
        elif (func left) * (func mid) < 0. then iter left mid 
        else iter mid right
    iter left right

    
//Метод простых итераций (источник - https://www.simumath.com/library/book.html?code=Alg_Equations_Iterations, дата обращения - 06.09.2024)
let iterations phi x0 = 
    let rec iter x =
        let x' = phi x
        if abs(x - x') < epsilon then x'
        else iter x'
    iter x0

//Метод Ньютона (истоничк - https://en.wikipedia.org/wiki/Newton%27s_method, дата обращения - 08.09.2024)
let newton func diff_func (x0: float) =
    let rec iter x =
        let x' = x - (func x) / (diff_func x)
        if abs(x - x') < epsilon then x'
        else iter x'
    iter x0 

//Создание строк для вывода красивой таблицы
let create_table_row equation_num interval method_name root_value =
    sprintf "| %-16d | %-13s | %-10s | %-10.7f |" equation_num interval method_name root_value

//Решаем уравнения численными методами
let bisection_results = 
    [ (1, "0.5-1", "Áèñåêöèÿ", bisection sixth 0.5 1.)
      (2, "1-3", "Áèñåêöèÿ", bisection seventh 1. 3.)
      (3, "2-3", "Áèñåêöèÿ", bisection eigth 2. 3.) ]

let iterations_results =
    [ (1, "0.5", "Èòåðàöèè", iterations sixth sixthPhi 0.5)
      (2, "1.5", "Èòåðàöèè", iterations seventh seventhPhi 1.5)
      (3, "2.5", "Èòåðàöèè", iterations eigth eigthPhi 2.5) ]

let newton_results =
    [ (1, "0.75", "Íüþòîí", newton sixth sixthDiff 0.75)
      (2, "2", "Íüþòîí", newton seventh seventhDiff 2.)
      (3, "2.5", "Íüþòîí", newton eigth eigthDiff 2.5) ]

//Формируем красивую таблицу
let title = "| Номер уравнения  | x0            | Метод       | Корень     |\n|------------------|---------------|------------|------------|"
let tableBisection = bisection_results |> List.map (fun (num, interval, method_name, root) -> create_table_row num interval method_name root)
let tableIteraytions = iterations_results |> List.map (fun (num, interval, method_name, root) -> create_table_row num interval method_name root)
let tableNewton = newton_results |> List.map (fun (num, interval, method_name, root) -> create_table_row num interval method_name root)

let full_table = title + "\n" + String.concat "\n" (tableBisection @ tableIteraytions @ tableNewton)

//Выводим красивую таблицу
printfn "%s" full_table
