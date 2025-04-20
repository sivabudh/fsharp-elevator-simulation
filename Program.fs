module ElevatorSimulation.Program

open ElevatorSimulation.Types
open ElevatorSimulation.Logic
open ElevatorSimulation.System
open ElevatorSimulation.UI
open ElevatorSimulation.Config
open System

/// Demonstrates the elevator simulation with a pre-programmed scenario
let runDemoScenario () =
    Console.Clear()
    printfn "Elevator Simulation - DEMO MODE"
    printfn "============================="
    
    // Use custom configuration for demo
    let config = {
        defaultConfig with 
            ElevatorCount = 3
            FloorCount = 10
            TickIntervalMs = 1000
            DoorOpenTicks = 3
    }
    
    printfn "Configuration: %d elevators, %d floors, door open time: %d ticks" 
        config.ElevatorCount config.FloorCount config.DoorOpenTicks
    
    // Create initial simulation state
    let mutable state = createSimulationWithConfig config
    
    // Display initial state
    displayElevatorSystem state.System
    printfn "\nDEMO SCENARIO STARTING..."
    System.Threading.Thread.Sleep(2000)
    
    // Process a series of commands to demonstrate functionality
    // Using F# 7/8's concise array literal with implicit yields
    let demoCommands = [
        // Call elevators to different floors
        "call 5 up"       // Call elevator to floor 5 going up
        "tick"; "tick"    // Watch elevator movement
        "call 8 down"     // Another call from a different floor
        "tick"; "tick"    // More movement
        
        // Request specific elevators to go to floors
        "request 1 10"    // Request elevator 1 to go to floor 10
        "tick"; "tick"; "tick"; "tick" // More movement
        "call 3 up"       // Add another call
        "request 2 7"     // Request elevator 2 to go to floor 7
        
        // Automatic operation
        "auto"            // Enable auto-tick to see continuous movement
        "stop"            // Stop auto-tick after 5 seconds
    ]
    
    // Execute each command with a slight delay
    for cmd in demoCommands do
        printfn "\nExecuting command: %s" cmd
        System.Threading.Thread.Sleep(1000)

        // Handle each command directly for the demo scenario
        // This ensures we don't depend on the Result-based processInput
        match cmd with 
        | "exit" -> 
            printfn "Exiting demo..."
            state <- { state with IsRunning = false }
        | "tick" ->
            // Using processTickWithConfig with configuration
            state <- { state with System = processTickWithConfig state.System config.DoorOpenTicks }
            displayElevatorSystem state.System
        | "auto" ->
            printfn "Auto-tick enabled (will run for 5 seconds)"
            state <- { state with AutoTick = true }
            
            // Run auto-tick for a few seconds
            for _ in 1..5 do
                System.Threading.Thread.Sleep(1000)
                state <- { state with System = processTickWithConfig state.System config.DoorOpenTicks }
                displayElevatorSystem state.System
        | "stop" ->
            printfn "Auto-tick disabled"
            state <- { state with AutoTick = false }
        | s when s.StartsWith("call ") ->
            // Parse call command manually: "call 5 up"
            let parts = s.Split(' ')
            if parts.Length = 3 then
                let floorNum = int parts.[1]
                let direction = 
                    match (parts.[2]:string).ToLower() with
                    | "up" -> Up
                    | "down" -> Down
                    | _ -> Up // Default to up for demo
                
                let event = CallElevator(floorNum, direction)
                state <- { state with System = processEventWithConfig event config.DoorOpenTicks state.System }
                displayElevatorSystem state.System
        | s when s.StartsWith("request ") ->
            // Parse request command manually: "request 1 10"
            let parts = s.Split(' ')
            if parts.Length = 3 then
                let elevatorId = int parts.[1]
                let floorNum = int parts.[2]
                
                let event = RequestFloor(elevatorId, floorNum)
                state <- { state with System = processEventWithConfig event config.DoorOpenTicks state.System }
                displayElevatorSystem state.System
        | _ ->
            // Unknown command, just continue with the demo
            ()
    
    printfn "\nDEMO COMPLETE!"
    printfn "To run interactive mode, restart the simulation without the --demo flag"

[<EntryPoint>]
let main argv =
    // Check if demo mode is requested
    if argv.Length > 0 && argv.[0] = "--demo" then
        runDemoScenario()
        0
    else
        Console.Clear()
        printfn "Elevator Simulation"
        printfn "==================="
        
        // Configure the simulation
        let config = {
            defaultConfig with
                ElevatorCount = 3
                FloorCount = 10
                TickIntervalMs = 1000
                DoorOpenTicks = 3
        }
        
        printfn "Using configuration: %d elevators, %d floors, door open time: %d ticks" 
            config.ElevatorCount config.FloorCount config.DoorOpenTicks
        
        // Create and run simulation
        let initialState = createSimulationWithConfig config
        displayElevatorSystem initialState.System
        
        printfn "\nSimulation started!"
        printfn "Type 'help' for available commands."
        printfn "Example commands:"
        printfn "  call 5 up    - Call an elevator to floor 5 going up"
        printfn "  request 1 8  - Request elevator 1 to go to floor 8"
        printfn "  tick         - Advance simulation by one time step"
        printfn "  auto         - Enable automatic advancement"
        printfn "  stop         - Stop automatic advancement"
        printfn "  exit         - Exit the simulation"
        printfn "\nTip: Run with '--demo' flag to see an automated demonstration"
        
        let finalState = runSimulation initialState
        
        0 // return an integer exit code
