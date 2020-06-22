// Learn more about F# at http://fsharp.org
open System
open Environment
let rand = Random()
type RequestActions = unit -> (Action * AgentBoard)[]
type ShowBoard = unit -> string
type GetAgentBoard = unit -> AgentBoard
type Outcome = GameStage -> unit
type Agent = RequestActions -> GetAgentBoard -> ShowBoard -> (Action * Outcome)

let randomAgent (getActions: RequestActions) (_ : GetAgentBoard) (_: ShowBoard) : chosenAction: Action * actionOutcome: Outcome =
    let actions = getActions()
    let (chosenAction, _) = actions.[rand.Next(actions.Length)]
    chosenAction, ignore

let userAgent (getActions: RequestActions) (_ : GetAgentBoard) (showBoard: ShowBoard) : chosenAction: Action * actionOutcome: Outcome =
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

// let learningAgent () (shouldEplore: bool) (getActions: RequestActions) (getBoard: GetAgentBoard) (_: ShowBoard) : chosenAction: Action * actionOutcome: LearnFromOutcome =
//     let stateValue = System.Collections.Generic.Dictionary<Action * AgentBoard, float>()
//     let mutable currentGameHistory = []
//     let learnigRate = .3
//     // let exploring = .5
//     let pickAnAction 
//     let fromState = getBoard()
//     let actions = getActions () |> Array.map fst 
//     let chosenAction = actions.[rand.]
//     let rec learnFromHistory (Reward nextActionReward)= function
//         | [] -> ()
//         | (reward, action, fromState) :: tail ->
//             let oldValueForState = stateValue.TryGetValue()
//     let learnFromOutcome (toBoard, gameStage) = 
//         match gameStage with
//         | Ilegal -> ()
//         | Draw reward
//         | Won reward 
//         | Lost reward -> 
//             currentGameHistory <- (fromState, tookAction, toState, reward) :: currentGameHistory
//             learnFromHistory currentGameHistory
//             currentGameHistory <- []

//         | EnemyTurn (reward, _) -> currentGameHistory <- (reward, chosenAction, fromState) :: currentGameHistory



let playGame (agentX: Agent) (agentO: Agent) =
    let newGame = {
        turn = PlayerToMove PlayerX
        board = Array.init 9 (fun _ -> EmptyCell)
    }
    let rec gameLoop (state: Game) =
        match state.turn with
        | PlayerToMove PlayerX ->
            let getActions () = availableActions PlayerX state |> Option.defaultValue [||]
            let showBoard () = state.board |> observeEnvironmentBoard PlayerX  |> showEnvironmentBoard
            let showAgentBoard () = state.board |> observeAgentBoard PlayerX
            let (chosenAction, agentLearnOutcome) = agentX getActions showAgentBoard showBoard
            let (newStage, newState) = updateGame PlayerX chosenAction state
            agentLearnOutcome  newStage
            match newStage with
            | Ilegal _ -> gameLoop state
            | Won _ -> "PlayerX"
            | Lost _ -> "PlayerO"
            | Draw _ -> "="
            | EnemyTurn _ -> gameLoop newState
        | PlayerToMove PlayerO ->
            let getActions () = availableActions PlayerO state |> Option.defaultValue [||]
            let showBoard () = state.board |> observeEnvironmentBoard PlayerO |> showEnvironmentBoard
            let showAgentBoard () = state.board |> observeAgentBoard PlayerO
            let (chosenAction, agentLearnOutcome) = agentO getActions showAgentBoard showBoard
            let (newStage, newState) = updateGame PlayerO chosenAction state
            agentLearnOutcome newStage
            match newStage with
            | Ilegal _-> gameLoop state
            | Won _ -> "PlayerO"
            | Lost _ -> "PlayerX"
            | Draw _ -> "="
            | EnemyTurn _ -> gameLoop newState
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
