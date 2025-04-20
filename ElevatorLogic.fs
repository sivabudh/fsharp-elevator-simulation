module ElevatorSimulation.Logic

open ElevatorSimulation.Types
open System

/// Creates a new elevator with the given ID
let createElevator id =
    {
        Id = id
        CurrentFloor = 1
        TargetFloor = None
        Direction = Idle
        DoorStatus = Closed
        DoorOpenTimeRemaining = None
        RequestedFloors = Set.empty
    }

/// Creates a new elevator system with the given number of elevators and floors
let createElevatorSystem elevatorCount floorCount =
    {
        Elevators = [1..elevatorCount] |> List.map createElevator
        ExternalRequests = []
        FloorCount = floorCount
    }

/// Calculates the direction an elevator should move to reach a target floor
let calculateDirection currentFloor targetFloor =
    match currentFloor, targetFloor with
    | current, target when current < target -> Up
    | current, target when current > target -> Down
    | _ -> Idle

/// Determines if an elevator is available to service a request
let isElevatorAvailable elevator =
    elevator.Direction = Idle || elevator.DoorStatus = Open

/// Finds the closest available elevator to a floor
let findClosestElevator floor elevators =
    elevators
    |> List.filter isElevatorAvailable
    |> List.sortBy (fun e -> abs (e.CurrentFloor - floor))
    |> List.tryHead

/// Finds the best elevator to handle a request
let findBestElevator floor direction elevators =
    // First try to find an elevator already going in the right direction
    let elevatorMovingInDirection =
        elevators
        |> List.filter (fun e -> 
            match direction, e.Direction with
            | Up, Up when e.CurrentFloor <= floor -> true
            | Down, Down when e.CurrentFloor >= floor -> true
            | _ -> false)
        |> List.sortBy (fun e -> abs (e.CurrentFloor - floor))
        |> List.tryHead
    
    // If no elevator is moving in the right direction, find the closest available one
    match elevatorMovingInDirection with
    | Some e -> Some e
    | None -> findClosestElevator floor elevators

/// Adds a floor to the elevator's requested floors
let addRequestedFloor floor elevator =
    { elevator with 
        RequestedFloors = Set.add floor elevator.RequestedFloors
        TargetFloor = 
            match elevator.TargetFloor with
            | None -> Some floor
            | Some current -> Some current // Keep existing target
    }

/// Processes an internal request (from inside the elevator)
let processInternalRequest elevatorId floor system =
    let elevators = 
        system.Elevators 
        |> List.map (fun e -> 
            if e.Id = elevatorId then 
                let updatedElevator = addRequestedFloor floor e
                match updatedElevator.Direction, updatedElevator.TargetFloor with
                | Idle, Some target -> 
                    { updatedElevator with 
                        Direction = calculateDirection updatedElevator.CurrentFloor target }
                | _ -> updatedElevator
            else e)
    
    { system with Elevators = elevators }

/// Processes an external request (from a floor button)
let processExternalRequest floor direction system =
    // Check if this request is already in the queue
    if system.ExternalRequests |> List.contains (floor, direction) then
        system
    else
        // Try to find the best elevator to handle this request
        let bestElevator = findBestElevator floor direction system.Elevators
        
        match bestElevator with
        | Some elevator ->
            // Assign request to this elevator
            let updatedElevators =
                system.Elevators
                |> List.map (fun e ->
                    if e.Id = elevator.Id then
                        let updatedElevator = addRequestedFloor floor e
                        match updatedElevator.Direction with
                        | Idle -> 
                            { updatedElevator with 
                                Direction = calculateDirection updatedElevator.CurrentFloor floor }
                        | _ -> updatedElevator
                    else e)
            
            { system with Elevators = updatedElevators }
        | None ->
            // No elevator available, add to external requests
            { system with 
                ExternalRequests = (floor, direction) :: system.ExternalRequests }

/// Moves an elevator one floor in its current direction
let moveElevator elevator =
    match elevator.Direction, elevator.TargetFloor with
    | Idle, _ -> elevator
    | _, None -> { elevator with Direction = Idle }
    | Up, Some _ -> { elevator with CurrentFloor = elevator.CurrentFloor + 1 }
    | Down, Some _ -> { elevator with CurrentFloor = elevator.CurrentFloor - 1 }

/// Checks if the elevator should stop at the current floor
let shouldStopAtFloor elevator =
    Set.contains elevator.CurrentFloor elevator.RequestedFloors ||
    (match elevator.TargetFloor with
     | Some target -> target = elevator.CurrentFloor
     | None -> false)

/// Processes the arrival of an elevator at a floor
let processArrival elevator =
    if shouldStopAtFloor elevator then
        // Remove this floor from requested floors
        let newRequestedFloors = Set.remove elevator.CurrentFloor elevator.RequestedFloors
        
        // Determine next target floor
        let nextTarget = 
            if Set.isEmpty newRequestedFloors then
                None
            else
                match elevator.Direction with
                | Up -> 
                    let floorsAbove = newRequestedFloors |> Set.filter (fun f -> f > elevator.CurrentFloor)
                    if Set.isEmpty floorsAbove then
                        // No floors above, go to lowest floor
                        Some (Set.minElement newRequestedFloors)
                    else
                        // Go to nearest floor above
                        Some (Set.minElement floorsAbove)
                | Down -> 
                    let floorsBelow = newRequestedFloors |> Set.filter (fun f -> f < elevator.CurrentFloor)
                    if Set.isEmpty floorsBelow then
                        // No floors below, go to highest floor
                        Some (Set.maxElement newRequestedFloors)
                    else
                        // Go to nearest floor below
                        Some (Set.maxElement floorsBelow)
                | Idle -> 
                    if Set.isEmpty newRequestedFloors then None
                    else Some (Set.minElement newRequestedFloors)
        
        // Calculate new direction
        let newDirection =
            match nextTarget with
            | None -> Idle
            | Some target -> calculateDirection elevator.CurrentFloor target
        
        // Using a default value here, will be configurable 
        // when called through processArrivalWithConfig
        { elevator with 
            DoorStatus = Open
            DoorOpenTimeRemaining = Some 3  // Default to 3 ticks
            RequestedFloors = newRequestedFloors
            TargetFloor = nextTarget
            Direction = newDirection }
    else
        elevator

/// Opens the door of an elevator with the specified door open time
let openDoor elevator doorOpenTime =
    { elevator with 
        DoorStatus = Open
        DoorOpenTimeRemaining = Some doorOpenTime }

/// Closes the door of an elevator
let closeDoor elevator =
    { elevator with 
        DoorStatus = Closed
        DoorOpenTimeRemaining = None }

/// Reconsiders pending external requests
let reconsiderExternalRequests system =
    let mutable updatedSystem = system
    let mutable remainingRequests = []
    
    // Try to assign each external request
    for (floor, direction) in system.ExternalRequests do
        let bestElevator = findBestElevator floor direction updatedSystem.Elevators
        
        match bestElevator with
        | Some elevator ->
            // Assign request to this elevator
            updatedSystem <- 
                { updatedSystem with 
                    Elevators = 
                        updatedSystem.Elevators
                        |> List.map (fun e ->
                            if e.Id = elevator.Id then
                                let updatedElevator = addRequestedFloor floor e
                                match updatedElevator.Direction with
                                | Idle -> 
                                    { updatedElevator with 
                                        Direction = calculateDirection updatedElevator.CurrentFloor floor }
                                | _ -> updatedElevator
                            else e) }
        | None ->
            // Keep the request in the queue
            remainingRequests <- (floor, direction) :: remainingRequests
    
    { updatedSystem with ExternalRequests = remainingRequests }

/// Helper function to handle door timer
let handleDoorTimer elevator =
    match elevator.DoorStatus, elevator.DoorOpenTimeRemaining with
    | Open, Some time when time > 1 -> 
        // Door is open and timer is still running, decrement timer
        { elevator with DoorOpenTimeRemaining = Some (time - 1) }
    | Open, Some 1 -> 
        // Door timer has expired, close the door
        closeDoor elevator
    | _ -> elevator

/// Process a simulation tick, updating all elevators
let processTick system =
    let updatedElevators =
        system.Elevators
        |> List.map (fun elevator ->
            // First handle door timer if door is open
            let elevatorWithDoorHandled = handleDoorTimer elevator
            
            match elevatorWithDoorHandled.DoorStatus, elevatorWithDoorHandled.Direction with
            | Open, _ -> 
                // Don't move when doors are open
                elevatorWithDoorHandled  
            | Closed, Idle -> 
                // Don't move when idle
                elevatorWithDoorHandled  
            | Closed, _ -> 
                // Move elevator and process arrival
                let movedElevator = moveElevator elevatorWithDoorHandled
                processArrival movedElevator)
    
    { system with Elevators = updatedElevators }
    |> reconsiderExternalRequests

/// Process an elevator event
let processEvent event system =
    match event with
    | RequestFloor (elevatorId, floor) ->
        processInternalRequest elevatorId floor system
    | CallElevator (floor, direction) ->
        processExternalRequest floor direction system
    | MoveElevator elevatorId ->
        { system with
            Elevators = 
                system.Elevators
                |> List.map (fun e -> if e.Id = elevatorId then moveElevator e else e) }
    | OpenDoor elevatorId ->
        { system with
            Elevators = 
                system.Elevators
                |> List.map (fun e -> if e.Id = elevatorId then openDoor e 3 else e) }
    | CloseDoor elevatorId ->
        { system with
            Elevators = 
                system.Elevators
                |> List.map (fun e -> if e.Id = elevatorId then closeDoor e else e) }
    | Tick ->
        processTick system
    | Exit ->
        system

/// Process an elevator event with configuration
let processEventWithConfig event doorOpenTicks system =
    match event with
    | OpenDoor elevatorId ->
        { system with
            Elevators = 
                system.Elevators
                |> List.map (fun e -> if e.Id = elevatorId then openDoor e doorOpenTicks else e) }
    | _ -> 
        processEvent event system
