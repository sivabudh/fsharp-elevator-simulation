module ElevatorSimulation.UI

open ElevatorSimulation.Types
open System

/// Creates a string representation of a floor in the elevator display
let displayFloor floor elevators floorCount =
    let floorNum = floorCount - floor + 1
    let elevatorSymbols = 
        elevators
        |> List.map (fun e -> 
            if e.CurrentFloor = floorNum then
                match e.DoorStatus with
                | Open -> $"[{e.Id}]"  // Open door
                | Closed -> $"|{e.Id}|"  // Closed door
            else
                "   "  // No elevator at this floor
        )
        |> String.concat " "
    
    $"{floorNum,2} |{elevatorSymbols}|"

/// Displays the current state of the elevator system
let displayElevatorSystem system =
    Console.Clear()
    
    printfn "Elevator Simulation"
    printfn "==================="
    
    // Display header with elevator IDs
    let elevatorHeader = 
        system.Elevators 
        |> List.map (fun e -> $" {e.Id} ") 
        |> String.concat " "
    
    printfn "   |%s|" elevatorHeader
    printfn "---+%s+" (String.replicate (elevatorHeader.Length + 2) "-")
    
    // Display each floor
    for floor in [1..system.FloorCount] do
        printfn "%s" (displayFloor floor system.Elevators system.FloorCount)
    
    printfn "---+%s+" (String.replicate (elevatorHeader.Length + 2) "-")
    
    // Display elevator status
    printfn "\nElevator Status:"
    for elevator in system.Elevators do
        let requestedFloors = 
            if Set.isEmpty elevator.RequestedFloors then
                "None"
            else
                elevator.RequestedFloors 
                |> Set.toList 
                |> List.map string 
                |> String.concat ", "
        
        printfn "Elevator %d: Floor %d, Direction %O, Door %O, Target: %s, Requests: %s" 
            elevator.Id 
            elevator.CurrentFloor 
            elevator.Direction 
            elevator.DoorStatus
            (match elevator.TargetFloor with | Some f -> string f | None -> "None")
            requestedFloors
    
    // Display pending external requests
    if not (List.isEmpty system.ExternalRequests) then
        printfn "\nPending external requests:"
        for (floor, direction) in system.ExternalRequests do
            printfn "Floor %d: %O" floor direction
