# FP: Lab 1. Вычисляем функционально

## Учимся решать вычислительные задачи в функциональном стиле

**Цель работы:** Учимся работать с системой программирования и мыслить в функциональном стиле  

## Задание 0: Установите окружение для работы

Научитесь выполнять код на F#, преимущественно в интерактивном режиме. Все возможные варианты использования F# описаны [на официальном сайте](https://fsharp.org/) [смотрите **USE** в меню]. Рекомендованные варианты (в порядке предпочтения):
* Установите [Visual Studio Code](https://code.visualstudio.com/) с [Ionide Extension](http://ionide.io/)
* Установите [полную Visual Studio](http://visualstudio.com) с поддержкой F# (и затем используйте окно **F# Interactive**)
* Установите окружение [Jupyter Notebook с поддержкой .NET](https://github.com/dotnet/interactive/blob/main/docs/NotebooksLocalExperience.md)
* [Используйте онлайн-окружение через браузер](https://fsharp.org/use/browser/) (например, [Repl.it](http://repl.it))

После того, как вы научились выполнять код, выполните задания ниже.

## Задание 1: Ряд Тейлора 

Напишите программу на F#, которая распечатает значения некоторой математической функции **f(x)**, заданной вариантом задания в соответствии с [таблицей](Lab1.pdf), на заданном (в таблице) интервале **[a,b]**. Функцию необходимо вычислить тремя способами:

 * С помощью встроенных функций F#
 * Используя **наивный способ** вычисления ряда Тейлора, где каждый член ряда вычисляется по формуле
 * Используя **умный способ** вычисления ряда Тейлора, где каждый следующий член вычисляется на основе предыдущего.

В двух последних случаях, вам необходимо осуществлять суммирование членов до тех пор, пока их абсолютное значение не станет меньше некоторого заданного небольшого значения **eps**. Необходимое число членов суммы также необходимо напечатать в таблице.

Вот как будет выглядеть таблица:

| x | Builtin | Smart Taylor | # terms | Dumb Taylor | # terms |
|---|---------|--------------|---------|-------------|---------|
| a | ... | ... | ... | ... |
| ... |
| b | ... | ... | ... | ... |

> Используйте формат вывода в функции `printfn`, чтобы напечатать таблицу красиво.

Функции и их разложения в ряд Тейлора определены в [таблице](Lab1.pdf). Используйте свой вариант задания в соответствии с номером в списке группы.

Для высокой оценки задания необходимо хорошо осуществить функциональную декомпозицию задачи. Например, функции для реализации аналогов цикла `while` имеет смысл реализовать отдельно, и затем использовать при вычислении как наивного, так и умного ряда.

## Задание 2: Численное решение трансцендентных уравнений

Реализуйте функцию для численного решения трансцендентных алгебраических уравнений, используя следующие [алгоритмы нахождения корней](https://en.wikipedia.org/wiki/Root-finding_algorithms): 

 * [Дихотомия](https://en.wikipedia.org/wiki/Bisection_method)
 * [Метод итераций](http://www.simumath.com/library/book.html?code=Alg_Equations_Iterations)
 * [Метод Ньютона](https://en.wikipedia.org/wiki/Newton%27s_method)

Вам необходимо разработать функции для решения произвольных уравнений, которые передаются как параметр-функция. Используйте также тот факт, что метод Ньютона является частным случаем метода итераций. 

Примените три функции решения к трем последовательным уравнениям [из таблицы](Lab1.pdf), начиная со своего варианта задания. У вас должна получиться табличка из 9 решений (3 уравнения по 3 метода на каждое).

Таблица для уравнений 6-7-8

| Номер уравнения  | x0            | Метод      | Корень     |
|------------------|---------------|------------|------------|
| 1                | 0.5-1         | Бисекция   | 0.9891739  |
| 2                | 1-3           | Бисекция   | 1.8832321  |
| 3                | 2-3           | Бисекция   | 2.4199753  |
| 1                | 0.5           | Итерции    | 0.9892321  |
| 2                | 1.5           | Итерции    | 1.8923948  |
| 3                | 2.5           | Итерции    | 2.5274793  |
| 1                | 0.5           | Итерации   | 0.9892321  |
| 2                | 1.5           | Итерации   | 1.8923948  |
| 3                | 2.5           | Итерации   | 2.5274793  |
| 1                | 0.75          | Ньютон     | 0.9891807  |
| 2                | 2             | Ньютон     | 1.8832390  |
| 3                | 2.5           | Ньютон     | 2.4199816  |
