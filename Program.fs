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
        
        match processInput cmd state.System with
        | Some Exit -> 
            printfn "Exiting demo..."
            state <- { state with IsRunning = false }
        | Some Tick ->
            state <- { state with System = processTick state.System }
            displayElevatorSystem state.System
        | Some event ->
            state <- { state with System = processEvent event state.System }
            displayElevatorSystem state.System
        | None ->
            if cmd = "auto" then
                printfn "Auto-tick enabled (will run for 5 seconds)"
                state <- { state with AutoTick = true }
                
                // Run auto-tick for a few seconds
                // Using F# 7/8's enhanced for loop with range
                for _ in 1..5 do
                    System.Threading.Thread.Sleep(1000)
                    // Using F# 8's enhanced record update
                    state <- { state with System = processTick state.System }
                    displayElevatorSystem state.System
            elif cmd = "stop" then
                printfn "Auto-tick disabled"
                state <- { state with AutoTick = false }
    
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
