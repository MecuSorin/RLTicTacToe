open System
open Environment
open MBrace.FsPickler

type RequestActions = unit -> (Action * AgentBoard)[]
type ShowBoard = unit -> string
type GetAgentBoard = unit -> AgentBoard
type Outcome = GameStage -> unit
type EndGameUpdate = EndGameNotification -> unit
type Agent = RequestActions -> GetAgentBoard -> ShowBoard -> (Action * Outcome * EndGameUpdate)

let rand = Random(DateTime.Now.Second)
/// pure random action chooser
let randomAgent (getActions: RequestActions) (_ : GetAgentBoard) (_: ShowBoard) : chosenAction: Action * actionOutcome: Outcome * endGameNotification: EndGameUpdate =
    let actions = getActions()
    let (chosenAction, _) = actions.[rand.Next(actions.Length)]
    chosenAction, ignore, ignore
/// human player
let userAgent (getActions: RequestActions) (_ : GetAgentBoard) (showBoard: ShowBoard) : chosenAction: Action * actionOutcome: Outcome * endGameNotification: EndGameUpdate =
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
    let acceptEndGame = function
        | DrawNotification _ -> printfn "Draw"
        | LostNotification _ -> printfn "Lose"
    Option.get matchingAction, checkOutcome, acceptEndGame


type Exploration = 
    | NoExploration
    | FullExploration
    | LimitedExploration of float
let (|ShouldExplore|ShouldNotExplore|) = function
    | NoExploration -> ShouldNotExplore
    | FullExploration -> ShouldExplore
    | LimitedExploration v ->
        if rand.NextDouble() < v 
            then ShouldExplore
            else ShouldNotExplore

/// state value learner agent
let learningAgent fileNameToSave =
    let binarySerializer = FsPickler.CreateBinarySerializer()
    let stateValue = System.Collections.Generic.Dictionary<AgentBoard, float>()
    let mutable currentGameHistory = []
    let learningRate = 0.03
    let decaying = 0.8
    let loadPreviousTrainedExperience () =
        if System.IO.File.Exists(fileNameToSave)
            then 
                let fileContent = System.IO.File.ReadAllBytes(fileNameToSave)
                let data = binarySerializer.UnPickle<(AgentBoard * float) list>(fileContent)
                data
                    |> List.sumBy (fun (ab, v)-> stateValue.Add(ab, v); 1)
                    |> printfn "Loaded previous agent knowledge (%i positions)"
            else ()
    let saveTrainedExperience (): unit =
        let dataToStore =
            stateValue
            |> Seq.map (fun kvp -> (kvp.Key, kvp.Value))
            |> Seq.toList
            |> binarySerializer.Pickle
        System.IO.File.WriteAllBytes(fileNameToSave, dataToStore)
        printfn "Saved the Agent experience (%i positions) to %s:" stateValue.Count fileNameToSave
    
    loadPreviousTrainedExperience ()
    let learner(shouldExplore: Exploration)  =
        let pickAnAction (getActions: RequestActions) (getBoard: GetAgentBoard) (_: ShowBoard) : chosenAction: Action * actionOutcome: Outcome * endGameNotification: EndGameUpdate =
            let fromState = getBoard()
            let actions = getActions()
            let tookAction = 
                match shouldExplore with
                | ShouldExplore -> actions.[rand.Next(actions.Length)] |> fst
                | ShouldNotExplore ->
                    actions
                    |> Array.maxBy (fun (_action, board) ->
                        match stateValue.TryGetValue board with
                            | false, _ -> 0.0
                            | true, v -> v )
                    |> fst
            let rec learnFromHistory (Reward nextActionReward) = function
                | [] -> ()
                | (fromState, tookAction, toState, Reward reward) :: tail ->
                    let oldValueForState = 
                        match stateValue.TryGetValue <| toState with
                        | false, _ -> 0.0
                        | true, v -> v
                    let newReward = reward + nextActionReward
                    let newValue = oldValueForState + learningRate * (decaying * newReward - oldValueForState)
                    stateValue.[toState] <- newValue
                    learnFromHistory (Reward newValue) tail
            let recordOutcomeOfTheMove (gameStage: GameStage) : unit =
                match gameStage with
                | Ilegal -> ()
                | Draw (reward, toState)
                | Won (reward, toState) 
                | Lost (reward, toState) -> 
                    currentGameHistory <- (fromState, tookAction, toState, reward) :: currentGameHistory
                    learnFromHistory (Reward 0.0) currentGameHistory
                    currentGameHistory <- []
                | EnemyTurn (reward, toState) -> currentGameHistory <- (fromState, tookAction, toState, reward) :: currentGameHistory
            let acceptEndGame = function
                | DrawNotification reward
                | LostNotification reward -> 
                    learnFromHistory reward currentGameHistory
                    currentGameHistory <- []
            tookAction, recordOutcomeOfTheMove, acceptEndGame
        pickAnAction
    learner, saveTrainedExperience

/// game loop
let playGame (agentX: Agent) (agentO: Agent) =
    let newGame = {
        turn = PlayerToMove PlayerX
        board = Array.init 9 (fun _ -> EmptyCell)
        notifyEndGame = ignore
    }
    let rec gameLoop (state: Game) =
        match state.turn with
        | PlayerToMove PlayerX ->
            let getActions () = availableActions PlayerX state |> Option.defaultValue [||]
            let showBoard () = state.board |> observeEnvironmentBoard PlayerX |> showEnvironmentBoard
            let showAgentBoard () = state.board |> observeAgentBoard PlayerX
            let (chosenAction, agentLearnOutcome, notifyEndGame) = agentX getActions showAgentBoard showBoard
            let (newStage, newState) = updateGame PlayerX chosenAction notifyEndGame state
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
            let (chosenAction, agentLearnOutcome, notifyEndGame) = agentO getActions showAgentBoard showBoard
            let (newStage, newState) = updateGame PlayerO chosenAction notifyEndGame state
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

/// fancy console spinner to please the eye whyle waiting
let rangeAnimation mapper totalRuns =
    let (oldLeft, oldTop) = Console.CursorLeft, Console.CursorTop
    Console.Write(" ")
    let divides = (totalRuns / 80 / 4) + 1
    let writeOverPreviousChar (t: string) = 
        Console.SetCursorPosition(Console.CursorLeft - 1, Console.CursorTop)
        Console.Write(t)
    let getNextItem = 
        let fromSymbols = [|"/"; "-"; "\\"; ".|" |]
        let totalSymbols = fromSymbols.Length
        fun x -> fromSymbols.[(x / divides) % totalSymbols]
    let result = 
        [1 .. totalRuns]
            |> List.map(fun x ->
                if 0 = x % divides
                    then
                        getNextItem x |> writeOverPreviousChar
                    else  ()
                mapper x)
    writeOverPreviousChar " "
    Console.SetCursorPosition(oldLeft, oldTop)
    Console.Write(String(' ', 81))
    Console.SetCursorPosition(oldLeft, oldTop)
    result

let showCumulatedResults winners =
    winners
        |> List.groupBy id
        |> List.sortBy fst
        |> List.iter(fun sameGroup ->
            printfn "\t%A: %i" (fst sameGroup) (snd sameGroup |> List.length)
        )

[<EntryPoint>]
let main argv =
    let (learner, saveAgentExperience) = learningAgent "Player.txt"
    use autoSave = { new IDisposable with
                        member __.Dispose () = saveAgentExperience () }
    // exploring
    printfn "Teaching phase:"
    100000
        |> rangeAnimation (fun _ -> playGame (learner FullExploration) (learner FullExploration))
        |> showCumulatedResults
    saveAgentExperience ()

    50000
            |> rangeAnimation (fun _ ->  playGame (learner (LimitedExploration 0.3)) (learner (LimitedExploration 0.3)))
            |> showCumulatedResults
    saveAgentExperience ()

    // testing
    printfn "Testing phase:"
    20000
        |> rangeAnimation(fun _ -> playGame (learner NoExploration) randomAgent)
        |> showCumulatedResults
    20000
        |> rangeAnimation(fun _ -> playGame randomAgent (learner NoExploration))
        |> showCumulatedResults

    printfn "Hammer time:"
    // experimenting the pain :))
    [1 .. 5]
        |> List.map(fun _ -> playGame userAgent (learner NoExploration))
        |> showCumulatedResults
    [1 .. 5]
        |> List.map(fun _ -> playGame (learner NoExploration) userAgent)
        |> showCumulatedResults
    0 // return an integer exit code
