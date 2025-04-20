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

/// <summary>
/// Enhanced error reporting types for input validation using a domain-specific approach
/// </summary>
/// <remarks>
/// This implements a more structured error handling system using F#'s discriminated unions
/// to provide type-safe and specific error cases that describe what went wrong during parsing.
/// </remarks>

/// Common parsing errors
type ParseError =
    | InvalidFormat
    | InvalidFloorNumber of int * int    // floor, max floor
    | InvalidElevatorId of int * int     // elevator id, max id
    | InvalidDirection of string         // invalid direction string
    | SameFloorRequest of int            // requesting same floor
    | GeneralError of string             // unexpected errors
    | InvalidCommand                     // unrecognized command

    /// <summary>
    /// Converts the error to a user-friendly string message
    /// </summary>
    override this.ToString() =
        match this with
        | InvalidFormat -> 
            "Invalid command format. Type 'help' for examples of correctly formatted commands."
        | InvalidFloorNumber (floor, maxFloor) -> 
            $"Invalid floor number {floor}. Valid range: 1-{maxFloor}"
        | InvalidElevatorId (id, maxId) -> 
            $"Invalid elevator ID {id}. Valid range: 1-{maxId}"
        | InvalidDirection dir -> 
            $"Invalid direction '{dir}'. Use 'up' or 'down'"
        | SameFloorRequest floor -> 
            $"Elevator is already at floor {floor}"
        | GeneralError msg -> 
            $"Error: {msg}"
        | InvalidCommand -> 
            "Invalid command. Try: call <floor> <up|down>, request <elevator> <floor>, tick, auto, stop, exit"

/// <summary>
/// Structure for a floor call request
/// </summary>
type FloorCallRequest = {
    Floor: int
    Direction: Direction
}

/// <summary>
/// Structure for an elevator floor request
/// </summary>
type ElevatorRequest = {
    ElevatorId: int
    TargetFloor: int
}

/// <summary>
/// Parses a floor call request from string inputs using pure functional error handling
/// </summary>
/// <param name="floorStr">String representing the floor number</param>
/// <param name="dirStr">String representing the direction ("up" or "down")</param>
/// <param name="system">The elevator system for validation</param>
/// <returns>A Result containing either the valid request or a specific error</returns>
let parseFloorCall (floorStr: string) (dirStr: string) (system: ElevatorSystem) : Result<FloorCallRequest, ParseError> =
    // Parse floor number using TryParse
    let parseFloorResult = 
        match System.Int32.TryParse(floorStr) with
        | (true, floorNum) -> Ok floorNum
        | _ -> Error (InvalidFloorNumber (-1, system.FloorCount))
    
    // Validate floor number is within range
    let validateFloorResult = 
        parseFloorResult 
        |> Result.bind (fun floorNum ->
            if floorNum < 1 || floorNum > system.FloorCount then
                Error (InvalidFloorNumber (floorNum, system.FloorCount))
            else
                Ok floorNum)
    
    // Parse direction
    let parseDirectionResult =
        match dirStr.ToLower() with
        | "up" -> Ok Up
        | "down" -> Ok Down
        | _ -> Error (InvalidDirection dirStr)
    
    // Combine results to create the final request
    match validateFloorResult, parseDirectionResult with
    | Ok floorNum, Ok direction -> 
        Ok { Floor = floorNum; Direction = direction }
    | Error e, _ -> Error e
    | _, Error e -> Error e

/// <summary>
/// Parses an elevator request from string inputs using pure functional error handling
/// </summary>
/// <param name="elevatorStr">String representing the elevator ID</param>
/// <param name="floorStr">String representing the target floor</param>
/// <param name="system">The elevator system for validation</param>
/// <returns>A Result containing either the valid request or a specific error</returns>
let parseElevatorRequest (elevatorStr: string) (floorStr: string) (system: ElevatorSystem) : Result<ElevatorRequest, ParseError> =
    // Parse elevator ID
    let parseElevatorResult = 
        match System.Int32.TryParse(elevatorStr) with
        | (true, elevatorId) -> Ok elevatorId
        | _ -> Error (InvalidElevatorId (-1, system.Elevators.Length))
    
    // Parse floor number
    let parseFloorResult = 
        match System.Int32.TryParse(floorStr) with
        | (true, floorNum) -> Ok floorNum
        | _ -> Error (InvalidFloorNumber (-1, system.FloorCount))
    
    // Validate elevator and floor in sequence
    match parseElevatorResult, parseFloorResult with
    | Ok elevatorId, Ok floorNum ->
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
            Ok { ElevatorId = elevatorId; TargetFloor = floorNum }
    | Error e, _ -> Error e
    | _, Error e -> Error e

/// <summary>
/// Validates a floor call request (for backward compatibility)
/// </summary>
let validateFloorCall floorNum dirString (system: ElevatorSystem) =
    // This uses the new parsing function but maintains the old return type
    // for backward compatibility
    match parseFloorCall (string floorNum) dirString system with
    | Ok request -> Ok (request.Floor, request.Direction)
    | Error err -> Error err

/// <summary>
/// Validates an elevator request (for backward compatibility)
/// </summary>
let validateElevatorRequest elevatorId floorNum (system: ElevatorSystem) =
    // This uses the new parsing function but maintains the old return type
    // for backward compatibility
    match parseElevatorRequest (string elevatorId) (string floorNum) system with
    | Ok request -> Ok (request.ElevatorId, request.TargetFloor)
    | Error err -> Error err

/// <summary>
/// Processes user input and returns the corresponding event
/// </summary>
/// <param name="input">The command string entered by the user</param>
/// <param name="system">The current elevator system state</param>
/// <returns>A Result containing either the valid event or a specific error</returns>
/// <remarks>
/// This function uses F# 8's enhanced pattern matching and Result type for robust error handling.
/// Each command type has specialized parsing to catch and report specific errors.
/// </remarks>
let processInput (input: string) (system: ElevatorSystem) =
    // Split the input into tokens
    let tokens = input.Trim().Split(' ', StringSplitOptions.RemoveEmptyEntries) 
                |> Array.map (fun s -> s.Trim())
    
    // Process based on command type
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
    | [| "call"; floorStr; directionStr |] ->
        // Parse and validate floor call command
        match parseFloorCall floorStr directionStr system with
        | Ok request -> 
            // Valid request - convert to elevator event
            Ok (CallElevator (request.Floor, request.Direction))
        | Error error -> 
            // Report the specific error and return
            printfn "%s" (error.ToString())
            Error error
    | [| "call"; _; _; _ |] | [| "call"; _; _; _; _ |] ->
        // Too many arguments for call command
        let error = InvalidFormat
        printfn "Error: Too many arguments for call command. Format: call <floor> <up|down>"
        Error error
    | [| "call"; _ |] ->
        // Too few arguments for call command
        let error = InvalidFormat
        printfn "Error: Call command requires a floor number and direction (up/down)"
        Error error
    | [| "request"; elevatorStr; floorStr |] ->
        // Parse and validate elevator request command
        match parseElevatorRequest elevatorStr floorStr system with
        | Ok request -> 
            // Valid request - convert to elevator event
            Ok (RequestFloor (request.ElevatorId, request.TargetFloor))
        | Error error -> 
            // Report the specific error and return
            printfn "%s" (error.ToString())
            Error error
    | [| "request"; _; _; _ |] | [| "request"; _; _; _; _ |] ->
        // Too many arguments for request command
        let error = InvalidFormat 
        printfn "Error: Too many arguments for request command. Format: request <elevator> <floor>"
        Error error
    | [| "request"; _ |] ->
        // Too few arguments for request command
        let error = InvalidFormat
        printfn "Error: Request command requires an elevator ID and a floor number"
        Error error
    | [| "help" |] ->
        // Help command - handled separately in runSimulation
        Error InvalidCommand
    | _ ->
        // Unknown command
        let error = InvalidCommand
        printfn "%s" (error.ToString())
        Error error

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
                | Error InvalidFormat ->
                    // Format error - the error message was already displayed
                    // Just wait for the auto-tick thread and continue
                    match autoTickThread with
                    | Some thread -> thread.Join()
                    | None -> ()
                    
                    if state.AutoTick then
                        let updatedSystem = processTick state.System
                        ElevatorSimulation.UI.displayElevatorSystem updatedSystem
                        runSimulation { state with System = updatedSystem }
                    else
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
