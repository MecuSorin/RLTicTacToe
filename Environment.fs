// fsharplint:disable RecordFieldNames
// fsharplint:disable FunctionNames
module Environment

open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double

type Action = Action of int
type Reward = Reward of float
type AgentBoard = AgentBoard of string
type EnvironmentBoard = EnvironmentBoard of string
type GameStage =
    | Ilegal
    | Won of Reward * AgentBoard
    | Lost of Reward * AgentBoard
    | Draw of Reward * AgentBoard
    | EnemyTurn of reward: Reward * game: AgentBoard
type EndGameNotification = 
    | DrawNotification of Reward
    | LostNotification of Reward
type Cell =
        | Empty
        | Own
        | Enemy
    with override xx.ToString() = 
            match xx with
            | Empty -> " "
            | Own -> "X"
            | Enemy -> "0"

type Player = PlayerX | PlayerO
type Turn = 
    | PlayerToMove of Player
    | GameWonBy of Player
    | GameDraw
type BoardCell = 
    | EmptyCell
    | PlayerCell of Player

type Board = BoardCell []
type Game = {
        board: Board
        turn: Turn
        notifyEndGame: EndGameNotification -> unit
    }

[<AutoOpen>]
module Utils =
    let ones = Vector<float>.Build.Dense(3, 1.0)
    let diagonal = Matrix<float>.Build.DiagonalIdentity(3, 3)
    let antiDiagonal = Matrix.Build.Dense(3,3, fun i j -> if 2 - i = j then 1.0 else 0.0 )
    // let rotateIndex i = match i with 0 -> 6 | 1 -> 3 | 2 -> 0 | 3 -> 7 | 4 -> 4 | 5 -> 1 | 6 -> 8 | 7 -> 5 | 8 -> 2 | _ -> -1
    // let rotateIndex2 = rotateIndex >> rotateIndex
    // let rotateIndex3 = rotateIndex >> rotateIndex >> rotateIndex
    // let rotate (board: _ []) = Array.init 9 (fun i -> board.[rotateIndex i])
    // let rotate2 (board: _ []) = Array.init 9 (fun i -> board.[rotateIndex2 i])
    // let rotate3 (board: _ []) = Array.init 9 (fun i -> board.[rotateIndex3 i])

    // a way for the agent to reduce the problem space is to change the representation from matrix to a single string where empty cell is 0, Enemy is E and Own is M
    // then take all 4 possible rotations of the matrix, represent them as strings and take the first string in sorted list
    // the action should not be translated because the Engine is not actually rotate the board
    // let sampleBoard = "123456789".ToCharArray() |> Array.map string
    //     [
    //         sampleBoard |> String.concat ""
    //         sampleBoard |> rotate  |> String.concat ""
    //         sampleBoard |> rotate2 |> String.concat ""
    //         sampleBoard |> rotate3 |> String.concat ""
    //     ]
    //         |> List.distin
    //         |> AgentBoard
    let boardRepresentationForAgent (baseBoard: string []): AgentBoard =
        // [
            baseBoard |> String.concat ""
        //     baseBoard |> rotate  |> String.concat ""
        //     baseBoard |> rotate2 |> String.concat ""
        //     baseBoard |> rotate3 |> String.concat ""
        // ]
        //     |> List.max
            |> AgentBoard

    let showEnvironmentBoard (EnvironmentBoard board) =
        board.ToCharArray()
        |> Array.map string
        |> Array.chunkBySize 3
        |> Array.map (fun x -> " " + String.concat " │ " x)
        |> String.concat "\n───┼───┼───\n"


let private observeBoard (player: Player) board =
    board 
        |> Array.map((function
                        | EmptyCell -> Empty
                        | PlayerCell p when p = player -> Own
                        | PlayerCell _ -> Enemy)
                        >> string)

/// Human user ask for environment observation
let observeEnvironmentBoard (player: Player) board = observeBoard player board |> String.concat "" |> EnvironmentBoard
/// Agent ask for environment observation
let observeAgentBoard (player: Player) board = observeBoard player board |> boardRepresentationForAgent

/// Environment action
let updateGame player (Action cellNo) notifyEndGame game =
    let moveReward  = Reward 0.0
    let winReward   = Reward 10.0
    let drawReward  = Reward 0.0
    let loseReward  = Reward -10.0

    let getOtherPlayer = function PlayerX -> PlayerO | PlayerO -> PlayerX
    let getTurn player (board: Board): Turn =
        let distinctRepresentation = 
            Matrix<float>.Build.Dense(3,3,fun i j -> match board.[i * 3 + j] with
                                                        | EmptyCell -> 0.
                                                        | PlayerCell p when p = player -> 1.
                                                        | PlayerCell _ -> 0.)
        if distinctRepresentation * ones |> Vector.exists((=)3.0)                       // horizontal check for win
            || distinctRepresentation.Transpose() * ones |> Vector.exists((=)3.0)       // vertical check for win
            || distinctRepresentation.Diagonal().Sum() = 3.0                            // main diagonal check for win
            || (distinctRepresentation * antiDiagonal).Diagonal().Sum() = 3.0           // antidiagonal check for win
            then GameWonBy player
            else if 0 = (board |> Array.sumBy (function EmptyCell -> 1 | _ -> 0))
                    then GameDraw
                    else getOtherPlayer player |> PlayerToMove

    match game.turn with
    | GameDraw 
    | GameWonBy _ -> 
        printfn "Game ended already. Nobody should attemp a move after the end"
        Ilegal, game
    | PlayerToMove p when p <> player -> 
        printfn "Not %A turn to move" player
        Ilegal, game
    | PlayerToMove _player ->
        match game.board.[cellNo] with
        | PlayerCell _ ->
            printfn "Cell already played"
            Ilegal, game // the cell was played before
        | EmptyCell ->
            let updatedBoard = Array.init 9 (fun i -> if i = cellNo then PlayerCell player else game.board.[i])
            let newGameState = {
                turn = getTurn player updatedBoard
                board = updatedBoard
                notifyEndGame = notifyEndGame
            }
            match newGameState.turn with
                | GameDraw -> 
                    game.notifyEndGame (DrawNotification drawReward)
                    Draw (drawReward, newGameState.board |> observeAgentBoard player), newGameState
                | GameWonBy p when p = player -> 
                    game.notifyEndGame (LostNotification loseReward)
                    Won (winReward, newGameState.board |> observeAgentBoard player), newGameState
                | GameWonBy _ -> 
                    printfn "How the other won on my move???"
                    Ilegal, game
                | PlayerToMove _ -> EnemyTurn (moveReward, newGameState.board |> observeAgentBoard player), newGameState

/// Agent ask for actions
let availableActions (player: Player) (game: Game) =
    match game.turn with
    | GameWonBy _
    | GameDraw -> None
    | PlayerToMove p when p = player ->
        game.board
            |> Array.mapi(fun i v ->
                match v with
                | EmptyCell ->
                    let moveOn_i_Cell cellNo =
                        if i = cellNo then Own
                        else match game.board.[cellNo] with
                                | EmptyCell -> Empty
                                | PlayerCell p when p = player -> Own
                                | PlayerCell _ -> Enemy
                        |> string
                    let newBoard = Array.init 9 moveOn_i_Cell |> boardRepresentationForAgent
                    Some (Action i, newBoard)
                | _ -> None)
            |> Array.choose id
                |> Some
        | PlayerToMove _ -> None
