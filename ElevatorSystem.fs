module ElevatorSimulation.System

open ElevatorSimulation.Types
open ElevatorSimulation.Logic
open ElevatorSimulation.Config
open System
open System.Threading

/// Simulation state
type SimulationState = {
    System: ElevatorSystem
    IsRunning: bool
    AutoTick: bool
    Config: SimulationConfig
}

/// Creates a new simulation with the given configuration
let createSimulationWithConfig config =
    {
        System = createElevatorSystem config.ElevatorCount config.FloorCount
        IsRunning = true
        AutoTick = false
        Config = config
    }

/// Creates a new simulation with the default configuration
let createSimulation elevatorCount floorCount =
    let config = {
        defaultConfig with 
            ElevatorCount = elevatorCount
            FloorCount = floorCount
    }
    createSimulationWithConfig config

/// Enhanced error reporting type for input validation
type ValidationError =
    | InvalidFloorNumber of int * int  // floor, max floor
    | InvalidElevatorId of int * int   // elevator id, max id
    | InvalidDirection of string       // invalid direction string
    | SameFloorRequest of int          // requesting same floor
    | ParseError of string             // general parsing error
    | InvalidCommand                   // unrecognized command
    override this.ToString() =
        match this with
        | InvalidFloorNumber (floor, maxFloor) -> 
            $"Invalid floor number {floor}. Valid range: 1-{maxFloor}"
        | InvalidElevatorId (id, maxId) -> 
            $"Invalid elevator ID {id}. Valid range: 1-{maxId}"
        | InvalidDirection dir -> 
            $"Invalid direction '{dir}'. Use 'up' or 'down'"
        | SameFloorRequest floor -> 
            $"Elevator is already at floor {floor}"
        | ParseError msg -> 
            $"Invalid input: {msg}"
        | InvalidCommand -> 
            "Invalid command. Try: call <floor> <up|down>, request <elevator> <floor>, tick, auto, stop, exit"

/// Validates a floor call request
let validateFloorCall floorNum dirString (system: ElevatorSystem) =
    try
        // Check floor number
        if floorNum < 1 || floorNum > system.FloorCount then
            Error (InvalidFloorNumber (floorNum, system.FloorCount))
        else
            // Parse direction (explicitly specify type info for string methods)
            let dirLower = (dirString:string).ToLower()
            match dirLower with
            | "up" -> Ok (floorNum, Up)
            | "down" -> Ok (floorNum, Down)
            | _ -> Error (InvalidDirection dirString)
    with
    | ex -> Error (ParseError ex.Message)

/// Validates an elevator request
let validateElevatorRequest elevatorId floorNum (system: ElevatorSystem) =
    try
        // Check if elevator exists
        let elevator = system.Elevators |> List.tryFind (fun e -> e.Id = elevatorId)
        
        match elevator with
        | None -> 
            Error (InvalidElevatorId (elevatorId, system.Elevators.Length))
        | Some e when e.CurrentFloor = floorNum && e.DoorStatus = Open -> 
            // Elevator is already at requested floor with doors open
            Error (SameFloorRequest floorNum)
        | Some _ when floorNum < 1 || floorNum > system.FloorCount ->
            Error (InvalidFloorNumber (floorNum, system.FloorCount))
        | Some _ -> 
            Ok (elevatorId, floorNum)
    with
    | ex -> Error (ParseError ex.Message)

/// Processes user input and returns the corresponding event
/// Uses F# 8's enhanced pattern matching and Result type for error handling
let processInput (input: string) (system: ElevatorSystem) =
    // Using F# 7/8's pipeline operator with improved Array operations
    let tokens = input.Trim().Split(' ') |> Array.map (fun s -> s.Trim())
    
    // Using F# 8's enhanced pattern matching for arrays
    match tokens with
    | [| "exit" |] -> 
        Ok Exit
    | [| "tick" |] -> 
        Ok Tick
    | [| "auto" |] -> 
        // Special cases handled separately - not errors but not regular events
        Error InvalidCommand  
    | [| "stop" |] -> 
        Error InvalidCommand  
    | [| "call"; floor; direction |] ->
        try
            let floorNum = int floor
            match validateFloorCall floorNum direction system with
            | Ok (floor, dir) -> Ok (CallElevator (floor, dir))
            | Error e -> 
                printfn "%s" (e.ToString())
                Error e
        with
        | ex -> 
            let error = ParseError ex.Message
            printfn "%s" (error.ToString())
            Error error
    | [| "request"; elevator; floor |] ->
        try
            let elevatorId = int elevator
            let floorNum = int floor
            
            match validateElevatorRequest elevatorId floorNum system with
            | Ok (id, floor) -> Ok (RequestFloor (id, floor))
            | Error e -> 
                printfn "%s" (e.ToString())
                Error e
        with
        | ex -> 
            let error = ParseError ex.Message
            printfn "%s" (error.ToString())
            Error error
    | _ ->
        printfn "%s" (InvalidCommand.ToString())
        Error InvalidCommand

/// Runs the simulation
let rec runSimulation state =
    if not state.IsRunning then
        state
    else
        // Auto-tick logic
        // Using F# 7/8's improved option handling and thread creation
        let autoTickThread = 
            if state.AutoTick then
                // Using F# 7's more concise lambda expressions
                let thread = Thread(fun () ->
                    Thread.Sleep(state.Config.TickIntervalMs)
                    // Using string interpolation from F# 7
                    if state.AutoTick then  // Check again in case it was changed
                        Console.WriteLine($"\nAuto tick...")
                )
                // Using F# 8's enhanced property assignment
                thread.IsBackground <- true
                thread.Start()
                Some thread
            else
                None
        
        // Process user input
        Console.Write("\nEnter command (type 'help' for commands): ")
        let input = Console.ReadLine()
        
        // Guard against null input (when piping commands)
        if isNull input then
            printfn "Exiting simulation due to end of input..."
            { state with IsRunning = false }
        else
            match input.Trim().ToLower() with
            | "help" ->
                printfn "\nAvailable commands:"
                printfn "  call <floor> <up|down> - Call an elevator to a floor"
                printfn "  request <elevator> <floor> - Request an elevator to go to a floor"
                printfn "  tick - Advance the simulation by one time step"
                printfn "  auto - Enable automatic ticking"
                printfn "  stop - Disable automatic ticking"
                printfn "  exit - Exit the simulation"
                runSimulation state
            | "auto" ->
                printfn "Auto-tick enabled"
                runSimulation { state with AutoTick = true }
            | "stop" ->
                printfn "Auto-tick disabled"
                runSimulation { state with AutoTick = false }
            | _ ->
                // Using F# 8's enhanced pattern matching with Result type
                match processInput input state.System with
                | Ok Exit ->
                    printfn "Exiting simulation..."
                    { state with IsRunning = false }
                | Ok Tick ->
                    // Using F# 8's improved record expressions
                    let updatedSystem = processTick state.System
                    ElevatorSimulation.UI.displayElevatorSystem updatedSystem
                    runSimulation { state with System = updatedSystem }
                | Ok event ->
                    // Using function composition and record update
                    let updatedSystem = processEvent event state.System
                    ElevatorSimulation.UI.displayElevatorSystem updatedSystem
                    runSimulation { state with System = updatedSystem }
                | Error InvalidCommand when input.Trim().ToLower() = "auto" || input.Trim().ToLower() = "stop" ->
                    // These are special commands handled above, not errors
                    // Just continue the simulation
                    runSimulation state
                | Error _ ->
                    // An error occurred and was already printed - just continue
                    
                    // Wait for auto-tick thread if it's running
                    match autoTickThread with
                    | Some thread -> thread.Join()
                    | None -> ()
                    
                    if state.AutoTick then
                        let updatedSystem = processTick state.System
                        ElevatorSimulation.UI.displayElevatorSystem updatedSystem
                        runSimulation { state with System = updatedSystem }
                    else
                        runSimulation state
