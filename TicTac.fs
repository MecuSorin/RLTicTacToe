// Learn more about F# at http://fsharp.org
open System
open Environment
let rand = Random()
type RequestActions = unit -> (Action * AgentBoard)[]
type ShowBoard = unit -> string
type LearnFromOutcome = GameStage -> unit
type Agent = RequestActions -> ShowBoard -> Action * LearnFromOutcome

let randomAgent (getActions: RequestActions) (_: ShowBoard) : chosenAction: Action * actionOutcome: (GameStage -> unit) =
    let actions = getActions()
    let (chosenAction, _) = actions.[rand.Next(actions.Length)]
    chosenAction, ignore

let userAgent (getActions: RequestActions) (showBoard: ShowBoard) : chosenAction: Action * actionOutcome: LearnFromOutcome =
    let actions = getActions() |> Array.map fst
    let mutable matchingAction = None
    while matchingAction |> Option.isNone do
        showBoard()
            |> printfn "You are X, choose your action (%s):\n%s" (actions |> Array.map(fun (Action a) -> string a) |> String.concat ", ")
        let chosenAction = 
            match Int32.TryParse (Console.ReadLine()) with
                | false, _ -> Action 100
                | true, n -> Action n
        matchingAction <- actions |> Array.tryFind ((=)chosenAction)
    let checkOutcome = function
        | Ilegal -> printfn "Invalid move"
        | Draw _ -> printfn "Draw"
        | Won _ -> printfn "Win"
        | Lost _ -> printfn "Lose"
        | EnemyTurn _ -> ()
    Option.get matchingAction, checkOutcome

let playGame (agentX: Agent) (agentO: Agent) =
    let newGame = {
        turn = PlayerToMove PlayerX
        board = Array.init 9 (fun _ -> EmptyCell)
    }
    let rec gameLoop (state: Game) =
        match state.turn with
        | PlayerToMove PlayerX ->
            let getActions () = availableActions PlayerX state |> Option.defaultValue [||]
            let showBoard () = observeBoard PlayerX state |> showAgentBoard
            let (chosenAction, agentLearnOutcome) = agentX getActions showBoard
            let newState = updateGame PlayerX chosenAction state
            agentLearnOutcome newState
            match newState with
            | Ilegal -> gameLoop state
            | Won -> "PlayerX"
            | Lost -> "PlayerO"
            | Draw -> "="
            | EnemyTurn (_, newGameState) -> gameLoop newGameState
        | PlayerToMove PlayerO ->
            let getActions () = availableActions PlayerO state |> Option.defaultValue [||]
            let showBoard () = observeBoard PlayerO state |> showAgentBoard
            let (chosenAction, agentLearnOutcome) = agentO getActions showBoard
            let newState = updateGame PlayerO chosenAction state
            agentLearnOutcome newState
            match newState with
            | Ilegal -> gameLoop state
            | Won -> "PlayerO"
            | Lost -> "PlayerX"
            | Draw -> "="
            | EnemyTurn (_, newGameState) -> gameLoop newGameState
        | GameWonBy _
        | GameDraw -> "?"

    gameLoop newGame

open System

[<EntryPoint>]
let main argv =


    [1 .. 2]
    |> List.map(fun _ -> playGame userAgent randomAgent)
    |> List.groupBy id
    |> List.iter(fun sameGroup ->
        printfn "%A: %i" (fst sameGroup) (snd sameGroup |> List.length)
    )
    0 // return an integer exit code
