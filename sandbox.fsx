#load """..\.paket\load\netcoreapp3.1\main.group.fsx"""

open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double

let ones = Vector<float>.Build.Dense(3, 1.0)
let diagonal = Matrix<float>.Build.DiagonalIdentity(3, 3)
let antiDiagonal = Matrix.Build.Dense(3,3, fun i j -> if 2 - i = j then 1.0 else 0.0 )
let rotateIndex i = match i with 0 -> 6 | 1 -> 3 | 2 -> 0 | 3 -> 7 | 4 -> 4 | 5 -> 1 | 6 -> 8 | 7 -> 5 | 8 -> 2 | _ -> -1
let rotateIndex2 = rotateIndex >> rotateIndex
let rotateIndex3 = rotateIndex >> rotateIndex >> rotateIndex
let rotate (board: _ []) = Array.init 9 (fun i -> board.[rotateIndex i])
let rotate2 (board: _ []) = Array.init 9 (fun i -> board.[rotateIndex2 i])
let rotate3 (board: _ []) = Array.init 9 (fun i -> board.[rotateIndex3 i])

let antiDiagonal = Matrix.Build.Dense(3,3, fun i j -> if 2 - i = j then 1.0 else 0.0 )

let repr = Array.init 9 id
rotateIndex 3
3 = (rotate repr).[rotateIndex3 3]
7 = (rotate repr).[rotateIndex3 7]
3 = (rotate2 repr).[rotateIndex2 3]
7 = (rotate2 repr).[rotateIndex2 7]
3 = (rotate3 repr).[rotateIndex 3]
7 = (rotate3 repr).[rotateIndex 7]
repr |> rotate3 |> rotate 

[0 .. 8] |> List.map(fun x -> (rotate repr).[rotateIndex3 x])
[0 .. 8] |> List.map(fun x -> (rotate2 repr).[rotateIndex2 x])
[0 .. 8] |> List.map(fun x -> (rotate3 repr).[rotateIndex x])
[0 .. 8] |> List.map(fun x -> (repr).[x])
let numbers' = numbers.Transpose()
numbers.Diagonal().Sum()
diagonal * ones
let rotation m = m * antiDiagonal |> Matrix.transpose

numbers
numbers |> rotation
numbers |> rotation |> rotation
numbers |> rotation |> rotation |> rotation
numbers |> rotation |> rotation |> rotation |> numbers


numbers * antiDiagonal 
(numbers * antiDiagonal).Transpose()