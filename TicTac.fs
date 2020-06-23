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
            |> printfn "You are X, choose your action (%s):\n%s" (actions |> Array.map(fun (Action a) -> string (a + 1)) |> String.concat ", ")
        let chosenAction = 
            match Int32.TryParse (Console.ReadLine()) with
                | false, _ -> Action 100
                | true, n -> Action (n - 1)
        matchingAction <- actions |> Array.tryFind ((=)chosenAction)
    let checkOutcome = function
        | Ilegal -> printfn "Invalid move"
        | Draw _ -> printfn "Draw"
        | Won _ -> printfn "Win"
        | Lost _ -> printfn "Lose"
        | EnemyTurn _ -> ()
    Option.get matchingAction, checkOutcome

let learningAgent () (shouldEplore: bool)  =
    let stateValue = System.Collections.Generic.Dictionary<AgentBoard * Action, float>()
    let mutable currentGameHistory = []
    let learningRate = 0.3
    let decaying = 0.8
    // let exploring = .5
    let pickAnAction (getActions: RequestActions) (getBoard: GetAgentBoard) (_: ShowBoard) : chosenAction: Action * actionOutcome: Outcome =
        let fromState = getBoard()
        let actions = getActions()
        let tookAction = 
            if shouldEplore 
            then actions.[rand.Next(actions.Length)] |> fst
            else
                actions
                |> Array.maxBy (fun (action, board) ->
                    match stateValue.TryGetValue <| (board, action) with
                    | false, _ -> 0.0
                    | true, v -> v )
                |> fst
        let rec learnFromHistory (Reward nextActionReward) = function
            | [] -> ()
            | (fromState, tookAction, toState, Reward reward) :: tail ->
                let oldValueForState = 
                    match stateValue.TryGetValue <| (fromState, tookAction) with
                    | false, _ -> 0.0
                    | true, v -> v
                let newReward = reward + nextActionReward
                let newValue = oldValueForState + learningRate * (decaying * newReward - oldValueForState)
                stateValue.[(fromState, tookAction)] <- newValue
                learnFromHistory (Reward newValue) tail
        let learnFromOutcome (gameStage: GameStage) : unit =
            match gameStage with
            | Ilegal -> ()
            | Draw (reward, toState)
            | Won (reward, toState) 
            | Lost (reward, toState) -> 
                currentGameHistory <- (fromState, tookAction, toState, reward) :: currentGameHistory
                learnFromHistory (Reward 0.0) currentGameHistory
                currentGameHistory <- []
            | EnemyTurn (reward, toState) -> currentGameHistory <- (fromState, tookAction, toState, reward) :: currentGameHistory
        tookAction, learnFromOutcome
    pickAnAction


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
    let learner = learningAgent ()
    printfn "Teaching phase:"
    // exploring
    [1 .. 50000]
    |> List.map(fun _ -> playGame (learner true) (learner true))
    |> List.groupBy id
    |> List.iter(fun sameGroup ->
        printfn "%A: %i" (fst sameGroup) (snd sameGroup |> List.length)
    )
    printfn "Testing phase:"
    // testing
    [1..500]
    |> List.map(fun _ ->  playGame (learner false) (learner false))
    |> List.groupBy id
    |> List.iter(fun sameGroup ->
        printfn "%A: %i" (fst sameGroup) (snd sameGroup |> List.length)
    )
    printfn "Hammer time:"
    
    // experimenting the pain :))
    [1..10]
    |> List.map(fun _ -> playGame (learner false) userAgent)
    |> List.groupBy id
    |> List.iter(fun sameGroup ->
        printfn "%A: %i" (fst sameGroup) (snd sameGroup |> List.length)
    )
    0 // return an integer exit code
