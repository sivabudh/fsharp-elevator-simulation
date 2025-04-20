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
/// Uses F# 7's enhanced collection expressions with implicit mapping
let createElevatorSystem elevatorCount floorCount =
    {
        Elevators = [ for id in 1..elevatorCount -> createElevator id ]
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

/// <summary>
/// Finds the next stop for an elevator based on its current position, 
/// direction, and remaining requested floors
/// </summary>
/// <param name="currentFloor">The current floor of the elevator</param>
/// <param name="currentDirection">The current direction of the elevator (Up, Down, or Idle)</param>
/// <param name="requestedFloors">Set of floors the elevator needs to visit</param>
/// <returns>Tuple of (nextTargetFloor option, newDirection)</returns>
/// <remarks>
/// Priority rules:
/// 1. Continue in current direction if there are requests in that direction
/// 2. Reverse direction when reaching the last request in current direction
/// 3. For idle elevators, select the nearest floor (defaulting to the lowest if equidistant)
/// </remarks>
let findNextStop currentFloor currentDirection requestedFloors =
    if Set.isEmpty requestedFloors then
        (None, Idle)
    else
        match currentDirection with
        | Up -> 
            // First priority: Continue upward if there are floors above
            let floorsAbove = requestedFloors |> Set.filter (fun f -> f > currentFloor)
            if not (Set.isEmpty floorsAbove) then
                // Continue going up to the next floor above
                let target = Set.minElement floorsAbove
                (Some target, Up)
            else
                // Reverse direction to handle floors below
                let target = Set.maxElement requestedFloors // Highest of remaining floors
                (Some target, Down)
                
        | Down -> 
            // First priority: Continue downward if there are floors below
            let floorsBelow = requestedFloors |> Set.filter (fun f -> f < currentFloor)
            if not (Set.isEmpty floorsBelow) then
                // Continue going down to the next floor below
                let target = Set.maxElement floorsBelow
                (Some target, Down)
            else
                // Reverse direction to handle floors above
                let target = Set.minElement requestedFloors // Lowest of remaining floors
                (Some target, Up)
                
        | Idle -> 
            // For idle elevators, find the nearest requested floor
            let distanceToFloor f = abs (f - currentFloor)
            // F# 8 doesn't have Set.groupBy, so using a different approach
            let minDistance = 
                requestedFloors 
                |> Set.map distanceToFloor
                |> Set.minElement
            
            // Get all floors at this minimum distance
            let nearestFloors = 
                requestedFloors 
                |> Set.filter (fun f -> distanceToFloor f = minDistance)
            
            // If there are multiple equidistant floors, choose the lowest by default
            let target = Set.minElement nearestFloors
            let newDirection = calculateDirection currentFloor target
            (Some target, newDirection)

/// Processes the arrival of an elevator at a floor
let processArrival elevator =
    if shouldStopAtFloor elevator then
        // Remove this floor from requested floors
        let newRequestedFloors = Set.remove elevator.CurrentFloor elevator.RequestedFloors
        
        // Determine next target floor and direction using the extracted logic
        let (nextTarget, newDirection) = findNextStop elevator.CurrentFloor elevator.Direction newRequestedFloors
        
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

/// <summary>
/// Validates whether a door state transition is allowed based on elevator state
/// </summary>
/// <param name="elevator">The current elevator state</param>
/// <param name="newDoorStatus">The new door status to transition to (Open or Closed)</param>
/// <returns>Some error message if the transition is invalid, None if valid</returns>
/// <remarks>
/// Safety and validation rules:
/// 1. Cannot open an already open door
/// 2. Cannot close an already closed door
/// 3. Cannot open doors while elevator is moving between floors
/// 4. Cannot close doors while the door timer is still active (safety feature)
/// </remarks>
let validateDoorTransition elevator newDoorStatus =
    match elevator.DoorStatus, newDoorStatus with
    | Open, Open -> 
        Some "Door is already open"
    | Closed, Closed -> 
        Some "Door is already closed"
    | Closed, Open when elevator.Direction <> Idle && 
                        not (shouldStopAtFloor elevator) -> 
        // Cannot open doors while moving between floors
        Some "Cannot open doors while elevator is moving between floors"
    | Open, Closed when elevator.DoorOpenTimeRemaining.IsSome && 
                       elevator.DoorOpenTimeRemaining.Value > 0 ->
        // Cannot close doors while timer is still active (safety feature)
        Some $"Cannot close doors yet, wait for timer ({elevator.DoorOpenTimeRemaining.Value} ticks remaining)"
    | _ -> None

/// Opens the door of an elevator with the specified door open time
/// Uses F# 8's anonymous record copy-and-update expression
/// Returns the updated elevator state or the original if operation is invalid
let openDoor elevator doorOpenTime =
    match validateDoorTransition elevator Open with
    | Some error -> 
        // In a real system, we'd log the error
        // printfn "Door operation error: %s" error
        elevator  // Return unchanged elevator
    | None ->
        { elevator with 
            DoorStatus = Open
            DoorOpenTimeRemaining = Some doorOpenTime }

/// Closes the door of an elevator
/// Uses F# 8's improved record copy expression
/// Returns the updated elevator state or the original if operation is invalid
let closeDoor elevator =
    match validateDoorTransition elevator Closed with
    | Some error -> 
        // In a real system, we'd log the error
        // printfn "Door operation error: %s" error
        elevator  // Return unchanged elevator
    | None ->
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
/// Uses F# 8's enhanced pattern matching with type test patterns
let handleDoorTimer elevator =
    match elevator with
    | { DoorStatus = Open; DoorOpenTimeRemaining = Some time } when time > 1 -> 
        // Door is open and timer is still running, decrement timer
        { elevator with DoorOpenTimeRemaining = Some (time - 1) }
    | { DoorStatus = Open; DoorOpenTimeRemaining = Some 1 } -> 
        // Door timer has expired, close the door
        closeDoor elevator
    | _ -> elevator

/// Process a simulation tick, updating all elevators
/// Uses F# 8's enhanced pattern matching with as patterns
let processTick system =
    let updatedElevators =
        system.Elevators
        |> List.map (fun elevator ->
            // First handle door timer if door is open
            let elevatorWithDoorHandled = handleDoorTimer elevator
            
            // Using F# 8's enhanced pattern matching
            match elevatorWithDoorHandled with
            | { DoorStatus = Open } as e -> 
                // Don't move when doors are open
                e  
            | { DoorStatus = Closed; Direction = Idle } as e -> 
                // Don't move when idle
                e  
            | { DoorStatus = Closed } as e -> 
                // Move elevator and process arrival
                let movedElevator = moveElevator e
                processArrival movedElevator)
    
    { system with Elevators = updatedElevators }
    |> reconsiderExternalRequests

/// Process an elevator event
/// Uses F# 8's enhanced pattern matching and list comprehensions
let processEvent event system =
    match event with
    | RequestFloor (elevatorId, floor) ->
        processInternalRequest elevatorId floor system
    | CallElevator (floor, direction) ->
        processExternalRequest floor direction system
    | MoveElevator elevatorId ->
        { system with
            Elevators = 
                [ for e in system.Elevators -> 
                    if e.Id = elevatorId then moveElevator e else e ] }
    | OpenDoor elevatorId ->
        { system with
            Elevators = 
                [ for e in system.Elevators -> 
                    if e.Id = elevatorId then openDoor e 3 else e ] }
    | CloseDoor elevatorId ->
        { system with
            Elevators = 
                [ for e in system.Elevators -> 
                    if e.Id = elevatorId then closeDoor e else e ] }
    | Tick ->
        processTick system
    | Exit ->
        system

/// Processes the arrival of an elevator at a floor with configurable door open time
let processArrivalWithConfig elevator doorOpenTicks =
    if shouldStopAtFloor elevator then
        // Remove this floor from requested floors
        let newRequestedFloors = Set.remove elevator.CurrentFloor elevator.RequestedFloors
        
        // Determine next target floor and direction using the extracted logic
        let (nextTarget, newDirection) = findNextStop elevator.CurrentFloor elevator.Direction newRequestedFloors
        
        // Using the provided doorOpenTicks value
        { elevator with 
            DoorStatus = Open
            DoorOpenTimeRemaining = Some doorOpenTicks
            RequestedFloors = newRequestedFloors
            TargetFloor = nextTarget
            Direction = newDirection }
    else
        elevator

/// Process a simulation tick with configurable door open time
let processTickWithConfig system doorOpenTicks =
    let updatedElevators =
        system.Elevators
        |> List.map (fun elevator ->
            // First handle door timer if door is open
            let elevatorWithDoorHandled = handleDoorTimer elevator
            
            // Using F# 8's enhanced pattern matching
            match elevatorWithDoorHandled with
            | { DoorStatus = Open } as e -> 
                // Don't move when doors are open
                e  
            | { DoorStatus = Closed; Direction = Idle } as e -> 
                // Don't move when idle
                e  
            | { DoorStatus = Closed } as e -> 
                // Move elevator and process arrival with config
                let movedElevator = moveElevator e
                processArrivalWithConfig movedElevator doorOpenTicks)
    
    { system with Elevators = updatedElevators }
    |> reconsiderExternalRequests

/// Process an elevator event with configuration
/// Uses F# 8's list comprehension syntax
let processEventWithConfig event doorOpenTicks system =
    match event with
    | OpenDoor elevatorId ->
        { system with
            Elevators = 
                [ for e in system.Elevators -> 
                    if e.Id = elevatorId then openDoor e doorOpenTicks else e ] }
    | Tick ->
        processTickWithConfig system doorOpenTicks
    | _ -> 
        processEvent event system
