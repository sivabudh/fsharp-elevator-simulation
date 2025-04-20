module ElevatorSimulation.System

open ElevatorSimulation.Types
open ElevatorSimulation.Logic
open System
open System.Threading

/// Simulation state
type SimulationState = {
    System: ElevatorSystem
    IsRunning: bool
    AutoTick: bool
    TickInterval: int  // milliseconds
}

/// Creates a new simulation with the given number of elevators and floors
let createSimulation elevatorCount floorCount =
    {
        System = createElevatorSystem elevatorCount floorCount
        IsRunning = true
        AutoTick = false
        TickInterval = 1000  // 1 second
    }

/// Processes user input and returns the corresponding event
let processInput (input: string) (system: ElevatorSystem) =
    let tokens = input.Trim().Split(' ') |> Array.map (fun s -> s.Trim())
    
    match tokens with
    | [| "exit" |] -> Some Exit
    | [| "tick" |] -> Some Tick
    | [| "auto" |] -> None  // Special case handled separately
    | [| "stop" |] -> None  // Special case handled separately
    | [| "call"; floor; direction |] ->
        try
            let floorNum = int floor
            let dir = 
                match direction.ToLower() with
                | "up" -> Up
                | "down" -> Down
                | _ -> failwith "Invalid direction"
            
            if floorNum < 1 || floorNum > system.FloorCount then
                printfn "Invalid floor number. Valid range: 1-%d" system.FloorCount
                None
            else
                Some (CallElevator (floorNum, dir))
        with
        | ex -> 
            printfn "Invalid input: %s" ex.Message
            None
    | [| "request"; elevator; floor |] ->
        try
            let elevatorId = int elevator
            let floorNum = int floor
            
            let elevatorExists = system.Elevators |> List.exists (fun e -> e.Id = elevatorId)
            
            if not elevatorExists then
                printfn "Invalid elevator ID. Valid range: 1-%d" (system.Elevators.Length)
                None
            elif floorNum < 1 || floorNum > system.FloorCount then
                printfn "Invalid floor number. Valid range: 1-%d" system.FloorCount
                None
            else
                Some (RequestFloor (elevatorId, floorNum))
        with
        | ex -> 
            printfn "Invalid input: %s" ex.Message
            None
    | _ ->
        printfn "Invalid command. Try: call <floor> <up|down>, request <elevator> <floor>, tick, auto, stop, exit"
        None

/// Runs the simulation
let rec runSimulation state =
    if not state.IsRunning then
        state
    else
        // Auto-tick logic
        let autoTickThread = 
            if state.AutoTick then
                let thread = Thread(fun () ->
                    Thread.Sleep(state.TickInterval)
                    if state.AutoTick then  // Check again in case it was changed
                        Console.WriteLine("\nAuto tick...")
                )
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
                match processInput input state.System with
                | Some Exit ->
                    printfn "Exiting simulation..."
                    { state with IsRunning = false }
                | Some Tick ->
                    let updatedSystem = processTick state.System
                    ElevatorSimulation.UI.displayElevatorSystem updatedSystem
                    runSimulation { state with System = updatedSystem }
                | Some event ->
                    let updatedSystem = processEvent event state.System
                    ElevatorSimulation.UI.displayElevatorSystem updatedSystem
                    runSimulation { state with System = updatedSystem }
                | None ->
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
