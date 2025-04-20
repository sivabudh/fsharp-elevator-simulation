module ElevatorSimulation.UI

open ElevatorSimulation.Types
open System

/// Validates if a floor number is within the valid range for the system
let isValidFloor system floor =
    floor >= 1 && floor <= system.FloorCount

/// Creates a string representation of a floor in the elevator display
/// Uses F# 8's list comprehension with implicit yields
let displayFloor floor elevators floorCount =
    let floorNum = floorCount - floor + 1
    let elevatorSymbols = 
        [ for e in elevators -> 
            if e.CurrentFloor = floorNum then
                match e.DoorStatus with
                | Open -> $"[{e.Id}]"  // Open door
                | Closed -> $"|{e.Id}|"  // Closed door
            else
                "   "  // No elevator at this floor
        ]
        |> String.concat " "
    
    $"{floorNum,2} |{elevatorSymbols}|"

/// Displays the current state of the elevator system
let displayElevatorSystem system =
    Console.Clear()
    
    printfn "Elevator Simulation"
    printfn "==================="
    
    // Display header with elevator IDs
    // Using F# 8's list comprehension syntax
    let elevatorHeader = 
        [ for e in system.Elevators -> $" {e.Id} " ]
        |> String.concat " "
    
    printfn "   |%s|" elevatorHeader
    printfn "---+%s+" (String.replicate (elevatorHeader.Length + 2) "-")
    
    // Display each floor
    // Using F# 7/8 for loop with enhanced range operator
    for floor in 1..system.FloorCount do
        printfn "%s" (displayFloor floor system.Elevators system.FloorCount)
    
    printfn "---+%s+" (String.replicate (elevatorHeader.Length + 2) "-")
    
    // Display elevator status
    printfn "\nElevator Status:"
    for elevator in system.Elevators do
        // Using F# 7/8 pattern matching and list comprehension
        let requestedFloors = 
            match elevator.RequestedFloors with
            | s when Set.isEmpty s -> "None"
            | s -> [ for floor in s -> string floor ] 
                   |> String.concat ", "
        
        printfn "Elevator %d: Floor %d, Direction %O, Door %O, Target: %s, Requests: %s" 
            elevator.Id 
            elevator.CurrentFloor 
            elevator.Direction 
            elevator.DoorStatus
            (match elevator.TargetFloor with 
            | Some f -> $"{f}" // Using F# 7's string interpolation
            | None -> "None")
            requestedFloors
    
    // Display pending external requests
    if not (List.isEmpty system.ExternalRequests) then
        printfn "\nPending external requests:"
        for (floor, direction) in system.ExternalRequests do
            printfn "Floor %d: %O" floor direction
