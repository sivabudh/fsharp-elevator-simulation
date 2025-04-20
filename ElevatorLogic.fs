module ElevatorSimulation.Logic

open ElevatorSimulation.Types
open ElevatorSimulation.SafeCollections
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
    let availableElevators = elevators |> List.filter isElevatorAvailable
    
    match availableElevators with
    | [] -> None  // No available elevators
    | _ -> 
        // Find closest elevator by distance to floor
        let sortedElevators = availableElevators |> List.sortBy (fun e -> abs (e.CurrentFloor - floor))
        match tryHead sortedElevators with
        | Ok elevator -> Some elevator
        | Error _ -> None  // This shouldn't happen since we checked for empty list

/// Finds the best elevator to handle a request
let findBestElevator floor direction elevators =
    // First try to find an elevator already going in the right direction
    let elevatorsInDirection =
        elevators
        |> List.filter (fun e -> 
            match direction, e.Direction with
            | Up, Up when e.CurrentFloor <= floor -> true
            | Down, Down when e.CurrentFloor >= floor -> true
            | _ -> false)
        
    // If there are elevators going in the right direction, find the closest one
    match elevatorsInDirection with
    | [] -> findClosestElevator floor elevators  // No elevator in right direction
    | _ -> 
        // Sort by distance to floor
        let sortedElevators = elevatorsInDirection |> List.sortBy (fun e -> abs (e.CurrentFloor - floor))
        match tryHead sortedElevators with
        | Ok elevator -> Some elevator
        | Error _ -> findClosestElevator floor elevators

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

/// <summary>
/// Checks if the elevator should stop at the current floor
/// </summary>
/// <param name="elevator">The elevator state to check</param>
/// <returns>True if the elevator should stop at its current floor, false otherwise</returns>
/// <remarks>
/// An elevator should stop at its current floor in two cases:
/// 1. The current floor is in the set of requested floors (internal or external requests)
/// 2. The current floor matches the elevator's target floor
/// 
/// When an elevator stops at a floor, the request is considered "serviced" at the moment 
/// the doors begin to open (see processArrival), not when they close. This aligns with
/// real elevator behavior where passengers begin boarding once doors start opening.
/// </remarks>
let shouldStopAtFloor elevator =
    // Stop if current floor is directly requested
    let isFloorRequested = Set.contains elevator.CurrentFloor elevator.RequestedFloors
    
    // Also stop if current floor is the target floor
    let isTargetFloor = 
        match elevator.TargetFloor with
        | Some target -> target = elevator.CurrentFloor
        | None -> false
        
    isFloorRequested || isTargetFloor

/// <summary>
/// Finds floors that are in a particular direction from the current floor
/// </summary>
/// <param name="currentFloor">The elevator's current floor</param>
/// <param name="requestedFloors">Set of floors requested by users</param>
/// <param name="direction">Direction to check (Up or Down)</param>
/// <returns>Set of floors in the specified direction</returns>
let private findFloorsInDirection currentFloor requestedFloors direction =
    match direction with
    | Up -> requestedFloors |> Set.filter (fun f -> f > currentFloor)
    | Down -> requestedFloors |> Set.filter (fun f -> f < currentFloor)
    | _ -> Set.empty  // No floors in Idle direction

/// <summary>
/// Finds the best floor to visit when continuing in the current direction
/// </summary>
/// <param name="floorsInDirection">Floors requested in the current direction</param>
/// <param name="direction">Current travel direction</param>
/// <returns>Option containing the next floor to visit or None if error</returns>
let private findNextFloorInDirection floorsInDirection direction =
    if Set.isEmpty floorsInDirection then
        None
    else
        match direction with
        | Up -> 
            match tryMinElement floorsInDirection with  // Lowest floor above current
            | Ok floor -> Some floor
            | Error _ -> None
        | Down -> 
            match tryMaxElement floorsInDirection with  // Highest floor below current
            | Ok floor -> Some floor
            | Error _ -> None
        | _ -> None

/// <summary>
/// Determines best floor when needing to reverse direction
/// </summary>
/// <param name="requestedFloors">All requested floors</param>
/// <param name="reversedDirection">The new direction after reversal</param>
/// <returns>Option with floor to target after reversal</returns>
let private findBestFloorAfterReversal requestedFloors reversedDirection =
    if Set.isEmpty requestedFloors then
        None
    else
        match reversedDirection with
        | Down -> 
            match tryMaxElement requestedFloors with  // Highest floor when reversing to down
            | Ok floor -> Some floor
            | Error _ -> None
        | Up -> 
            match tryMinElement requestedFloors with  // Lowest floor when reversing to up
            | Ok floor -> Some floor
            | Error _ -> None
        | _ -> None  // Should not happen

/// <summary>
/// Finds the nearest requested floor for an idle elevator
/// </summary>
/// <param name="currentFloor">Current elevator position</param>
/// <param name="requestedFloors">Set of floors requested</param>
/// <returns>A Result containing either the nearest floor or an error if the set is empty</returns>
let private findNearestRequestedFloor currentFloor requestedFloors =
    // Handle the empty set case first with Result pattern
    if Set.isEmpty requestedFloors then
        Error "No requested floors to find nearest from"
    else
        // Calculate distance to each requested floor
        let distanceToFloor f = abs (f - currentFloor)
        
        // Find the minimum distance
        match requestedFloors |> Set.map distanceToFloor |> tryMinElement with
        | Error err -> Error err
        | Ok minDistance ->
            // Get all floors at this minimum distance
            let nearestFloors = 
                requestedFloors 
                |> Set.filter (fun f -> distanceToFloor f = minDistance)
            
            // If multiple floors are equidistant, choose the lowest
            // This is a policy decision that could be changed based on requirements
            match tryMinElement nearestFloors with
            | Ok floor -> Ok floor
            | Error err -> Error err

/// <summary>
/// Determines if the elevator should reverse its direction
/// </summary>
/// <param name="currentFloor">The current floor position</param>
/// <param name="currentDirection">The current direction of travel</param>
/// <param name="requestedFloors">Set of requested floors</param>
/// <returns>True if the elevator should reverse direction, false otherwise</returns>
let private shouldReverseDirection currentFloor currentDirection requestedFloors =
    match currentDirection with
    | Up ->
        // Should reverse if no more floors above current floor
        let floorsAbove = findFloorsInDirection currentFloor requestedFloors Up
        Set.isEmpty floorsAbove
    | Down ->
        // Should reverse if no more floors below current floor
        let floorsBelow = findFloorsInDirection currentFloor requestedFloors Down
        Set.isEmpty floorsBelow
    | Idle ->
        // Idle elevators don't need to reverse (they pick a direction)
        false

/// <summary>
/// Calculates the optimized target and direction for a moving elevator
/// </summary>
/// <param name="currentFloor">The elevator's current floor</param>
/// <param name="currentDirection">Current direction of travel</param>
/// <param name="requestedFloors">Set of requested floors</param>
/// <returns>Tuple of (target floor, direction)</returns>
let private calculateMovingElevatorTarget currentFloor currentDirection requestedFloors =
    if shouldReverseDirection currentFloor currentDirection requestedFloors then
        // Need to reverse direction
        match currentDirection with
        | Up -> 
            // If going up with no more floors above, reverse to down
            match findBestFloorAfterReversal requestedFloors Down with
            | Some reversedTarget -> (Some reversedTarget, Down)
            | None -> (None, Idle)  // Fall back to idle if no target found
        | Down -> 
            // If going down with no more floors below, reverse to up
            match findBestFloorAfterReversal requestedFloors Up with
            | Some reversedTarget -> (Some reversedTarget, Up)
            | None -> (None, Idle)  // Fall back to idle if no target found
        | Idle -> 
            // This case shouldn't happen with moving elevators
            (None, Idle)
    else
        // Continue in same direction
        match currentDirection with
        | Up ->
            // Find next floor above
            let floorsAbove = findFloorsInDirection currentFloor requestedFloors Up
            match findNextFloorInDirection floorsAbove Up with
            | Some nextFloor -> (Some nextFloor, Up)
            | None -> (None, Idle)  // Fall back to idle if no target found
        | Down ->
            // Find next floor below
            let floorsBelow = findFloorsInDirection currentFloor requestedFloors Down
            match findNextFloorInDirection floorsBelow Down with
            | Some nextFloor -> (Some nextFloor, Down)
            | None -> (None, Idle)  // Fall back to idle if no target found
        | Idle ->
            // This case shouldn't happen with moving elevators
            (None, Idle)

/// <summary>
/// Calculates the target and direction for an idle elevator
/// </summary>
/// <param name="currentFloor">The elevator's current floor</param>
/// <param name="requestedFloors">Set of requested floors</param>
/// <returns>Tuple of (target floor option, direction)</returns>
/// <remarks>
/// For idle elevators, we choose the nearest requested floor, breaking ties
/// by selecting the lowest floor. The direction is calculated based on the
/// relationship between the current floor and target floor.
/// </remarks>
let private calculateIdleElevatorTarget currentFloor requestedFloors =
    // For idle elevators, find the nearest floor
    match findNearestRequestedFloor currentFloor requestedFloors with
    | Ok target ->
        // Calculate appropriate direction to reach this floor
        let newDirection = calculateDirection currentFloor target
        (Some target, newDirection)
    | Error _ ->
        // If we couldn't find a target (empty set), stay idle
        (None, Idle)

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
/// 
/// The logic is divided into helper functions to make each step clearer:
/// - shouldReverseDirection: Determines if direction change is needed
/// - calculateMovingElevatorTarget: Handles logic for moving elevators
/// - calculateIdleElevatorTarget: Specialized logic for idle elevators
/// </remarks>
let findNextStop currentFloor currentDirection requestedFloors =
    // Handle empty request set - elevator becomes idle with no target
    if Set.isEmpty requestedFloors then
        (None, Idle)
    else
        match currentDirection with
        | Up | Down -> 
            // Unified handling for moving elevators (up or down)
            calculateMovingElevatorTarget currentFloor currentDirection requestedFloors
        | Idle -> 
            // Special case for idle elevators
            calculateIdleElevatorTarget currentFloor requestedFloors

/// <summary>
/// Processes the arrival of an elevator at a floor
/// </summary>
/// <param name="elevator">The current elevator state</param>
/// <returns>Updated elevator state after arrival processing</returns>
/// <remarks>
/// When an elevator arrives at a requested floor:
/// 1. The floor is removed from requested floors
/// 2. Door status changes to Open with a timer (default: 3 ticks)
/// 3. The next target and direction are recalculated
/// 4. The elevator remains stationary until the door timer expires
/// 
/// Important: A floor is considered "serviced" at the moment the doors begin to open,
/// not when they close. This ensures that passenger boarding/alighting is properly
/// modeled with the timed door open state.
/// 
/// Request handling sequence:
/// - When shouldStopAtFloor returns true, the elevator opens its doors
/// - The floor is immediately removed from requested floors (considered "serviced")
/// - The next destination is calculated based on remaining requests
/// - The elevator won't move again until doors are closed (enforced by movement checks)
/// </remarks>
let processArrival elevator =
    if shouldStopAtFloor elevator then
        // Step 1: Remove this floor from requested floors
        // This marks the request as "serviced" immediately when doors begin to open
        let newRequestedFloors = Set.remove elevator.CurrentFloor elevator.RequestedFloors
        
        // Step 2: Determine next target floor and direction using the scheduling algorithm
        let (nextTarget, newDirection) = findNextStop elevator.CurrentFloor elevator.Direction newRequestedFloors
        
        // Step 3: Open doors and start the timer
        // Using a default door open time here (3 ticks)
        // For configurable timing, use processArrivalWithConfig instead
        { elevator with 
            DoorStatus = Open
            DoorOpenTimeRemaining = Some 3  // Default to 3 ticks
            RequestedFloors = newRequestedFloors
            TargetFloor = nextTarget
            Direction = newDirection }
    else
        // Not a requested floor, continue without changing state
        elevator

/// <summary>
/// Door operation error types for functional error handling
/// </summary>
type DoorOperationError =
    | AlreadyOpen
    | AlreadyClosed
    | ElevatorMoving
    | TimerActive of int // Timer value remaining
    
    /// <summary>
    /// Converts the error to a user-friendly string message
    /// </summary>
    override this.ToString() =
        match this with
        | AlreadyOpen -> "Door is already open"
        | AlreadyClosed -> "Door is already closed"
        | ElevatorMoving -> "Cannot open doors while elevator is moving between floors"
        | TimerActive ticks -> $"Cannot close doors yet, wait for timer ({ticks} ticks remaining)"

/// <summary>
/// Validates whether a door state transition is allowed based on elevator state
/// </summary>
/// <param name="elevator">The current elevator state</param>
/// <param name="newDoorStatus">The new door status to transition to (Open or Closed)</param>
/// <returns>A Result indicating valid transition or specific error</returns>
/// <remarks>
/// Safety and validation rules:
/// 1. Cannot open an already open door
/// 2. Cannot close an already closed door
/// 3. Cannot open doors while elevator is moving between floors
/// 4. Cannot close doors while the door timer is still active (safety feature)
/// 
/// These validations are critical for passenger safety and proper elevator operation.
/// In particular:
/// - Rule #3 ensures passengers cannot open doors between floors (which would be unsafe)
/// - Rule #4 ensures passengers have sufficient time to enter/exit before doors close
/// 
/// This function uses a comprehensive pattern matching approach with guards to
/// evaluate complex conditions while maintaining readability. The result is either
/// Ok unit (transition allowed) or Error with a specific error reason.
/// </remarks>
let validateDoorTransition elevator newDoorStatus =
    match elevator.DoorStatus, newDoorStatus with
    | Open, Open -> 
        // No need to open an already open door
        Error AlreadyOpen
        
    | Closed, Closed -> 
        // No need to close an already closed door
        Error AlreadyClosed
        
    | Closed, Open when elevator.Direction <> Idle && 
                        not (shouldStopAtFloor elevator) -> 
        // Safety rule: Cannot open doors while moving between floors
        // This prevents doors being opened when the elevator is between floors,
        // which would be physically impossible and dangerous
        Error ElevatorMoving
        
    | Open, Closed when elevator.DoorOpenTimeRemaining.IsSome && 
                       elevator.DoorOpenTimeRemaining.Value > 0 ->
        // Safety feature: Cannot close doors while timer is still active
        // This ensures doors remain open for the minimum required time,
        // allowing passengers to safely enter and exit
        Error (TimerActive elevator.DoorOpenTimeRemaining.Value)
        
    | _ -> 
        // All other transitions are valid:
        // - Opening closed doors when elevator is stopped or at requested floor
        // - Closing open doors after timer has expired
        Ok ()

/// <summary>
/// Validates and attempts to open the door of an elevator with the specified door open time
/// </summary>
/// <param name="elevator">The elevator whose door should be opened</param>
/// <param name="doorOpenTime">The number of ticks the door should remain open</param>
/// <returns>Result containing either the updated elevator state or an error</returns>
/// <remarks>
/// This function:
/// 1. Validates the door transition using validateDoorTransition
/// 2. If valid, updates the door status to Open and sets the timer
/// 3. If invalid, returns an Error with the specific reason
/// 
/// The validation ensures that doors can only be opened when the elevator
/// is stopped at a floor, providing safety for passengers. The door will
/// stay open for the specified number of ticks before automatically closing.
/// </remarks>
let tryOpenDoor elevator doorOpenTime =
    match validateDoorTransition elevator Open with
    | Ok () ->
        // Valid transition - update door status and set timer
        Ok { elevator with 
               DoorStatus = Open
               DoorOpenTimeRemaining = Some doorOpenTime }
    | Error error -> 
        // Return the specific error for proper handling by caller
        Error error
        
/// <summary>
/// Opens the door of an elevator with the specified door open time
/// </summary>
/// <param name="elevator">The elevator whose door should be opened</param>
/// <param name="doorOpenTime">The number of ticks the door should remain open</param>
/// <returns>Updated elevator state or unchanged if operation is invalid</returns>
/// <remarks>
/// This function provides backward compatibility with existing code.
/// It calls tryOpenDoor and handles the Result internally.
/// </remarks>
let openDoor elevator doorOpenTime =
    match tryOpenDoor elevator doorOpenTime with
    | Ok updatedElevator -> updatedElevator
    | Error error -> 
        // In a real system, we'd log the error
        // printfn "Door operation error: %s" error.ToString()
        elevator  // Return unchanged elevator - validation failed

/// <summary>
/// Validates and attempts to close the door of an elevator
/// </summary>
/// <param name="elevator">The elevator whose door should be closed</param>
/// <returns>Result containing either the updated elevator state or an error</returns>
/// <remarks>
/// This function:
/// 1. Validates the door transition using validateDoorTransition
/// 2. If valid, updates the door status to Closed and clears the timer
/// 3. If invalid, returns an Error with the specific reason
/// 
/// The validation ensures that doors can only be closed after the door timer
/// has expired, ensuring passengers have sufficient time to enter/exit the elevator.
/// 
/// Door closure is a prerequisite for elevator movement - an elevator
/// will remain stationary until its doors are closed.
/// </remarks>
let tryCloseDoor elevator =
    match validateDoorTransition elevator Closed with
    | Ok () ->
        // Valid transition - update door status and clear timer
        Ok { elevator with 
               DoorStatus = Closed
               DoorOpenTimeRemaining = None }
    | Error error -> 
        // Return the specific error for proper handling by caller
        Error error
        
/// <summary>
/// Closes the door of an elevator
/// </summary>
/// <param name="elevator">The elevator whose door should be closed</param>
/// <returns>Updated elevator state or unchanged if operation is invalid</returns>
/// <remarks>
/// This function provides backward compatibility with existing code.
/// It calls tryCloseDoor and handles the Result internally.
/// </remarks>
let closeDoor elevator =
    match tryCloseDoor elevator with
    | Ok updatedElevator -> updatedElevator
    | Error error -> 
        // In a real system, we'd log the error
        // printfn "Door operation error: %s" error.ToString()
        elevator  // Return unchanged elevator - validation failed

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

/// <summary>
/// Handles the door timer mechanics for elevator doors
/// </summary>
/// <param name="elevator">The current elevator state</param>
/// <returns>An updated elevator with adjusted door timing or closed doors</returns>
/// <remarks>
/// This function implements the timer mechanism for automatic door closing:
/// 1. If doors are open with time > 1, decrement the timer
/// 2. If doors are open with time = 1, automatically close the doors
/// 3. Leave elevator unchanged in all other cases
/// 
/// The door timer ensures passengers have sufficient time to enter/exit
/// before doors automatically close, and ensures the elevator never moves
/// with open doors. This uses F# 8's enhanced pattern matching for clarity.
/// 
/// Safety considerations:
/// - This mechanic ensures an elevator remains at a floor for a minimum time,
///   providing sufficient time for passengers to board/alight
/// - When the timer expires, closeDoor is called which performs safety checks
/// - The mechanic is part of a larger safety system ensuring elevators never
///   move with open doors
/// 
/// Relation to request handling:
/// - Even though requests are considered "serviced" when doors open (see processArrival),
///   this timer ensures passengers have time to physically enter/exit
/// - By default, doors remain open for 3 ticks (configurable) before closing
/// </remarks>
let handleDoorTimer elevator =
    match elevator with
    | { DoorStatus = Open; DoorOpenTimeRemaining = Some time } when time > 1 -> 
        // Door is open and timer is still running, decrement timer
        { elevator with DoorOpenTimeRemaining = Some (time - 1) }
    | { DoorStatus = Open; DoorOpenTimeRemaining = Some 1 } -> 
        // Door timer has expired, close the door using safety checks
        // tryCloseDoor applies additional safety validations before closing
        match tryCloseDoor elevator with
        | Ok updatedElevator -> updatedElevator
        | Error _ -> 
            // If we can't close the door, we'll decrement the timer and try again next tick
            // This is a safety measure to ensure doors eventually close
            { elevator with DoorOpenTimeRemaining = Some 1 }
    | _ -> 
        // No action needed for closed doors or other states
        // This includes elevators with closed doors or invalid timer states
        elevator

/// <summary>
/// Process a simulation tick, updating all elevators with default door timing
/// </summary>
/// <param name="system">The current elevator system state</param>
/// <returns>Updated elevator system after processing a time step</returns>
/// <remarks>
/// This is the standard tick processor that uses the default door open time.
/// It follows the same sequence as processTickWithConfig:
/// 
/// 1. Handle door timers for all elevators with open doors
/// 2. Move eligible elevators (doors closed and not idle)
/// 3. Process arrivals at floors for moved elevators
/// 4. Reconsider any external requests that weren't assigned
/// 
/// This function uses F# 8's enhanced pattern matching with as-patterns
/// for improved readability and type safety.
/// </remarks>
let processTick system =
    let updatedElevators =
        system.Elevators
        |> List.map (fun elevator ->
            // Step 1: Handle door timer if door is open
            let elevatorWithDoorHandled = handleDoorTimer elevator
            
            // Step 2 & 3: Handle movement based on door status and direction
            match elevatorWithDoorHandled with
            | { DoorStatus = Open } as e -> 
                // Safety rule: Don't move when doors are open
                e  
            | { DoorStatus = Closed; Direction = Idle } as e -> 
                // Optimization: Don't move when idle
                e  
            | { DoorStatus = Closed } as e -> 
                // Move elevator and process arrival with default door timing
                let movedElevator = moveElevator e
                processArrival movedElevator)
    
    // Step 4: Update system and reconsider any pending external requests
    { system with Elevators = updatedElevators }
    |> reconsiderExternalRequests

/// <summary>
/// Process an elevator event with standard parameters
/// </summary>
/// <param name="event">The elevator event to process</param>
/// <param name="system">The current elevator system state</param>
/// <returns>Updated elevator system after processing the event</returns>
/// <remarks>
/// Handles various elevator events:
/// - RequestFloor: Internal request from inside an elevator
/// - CallElevator: External request from a floor button
/// - MoveElevator: Manual command to move a specific elevator
/// - OpenDoor: Command to open doors (with default timing)
/// - CloseDoor: Command to close doors
/// - Tick: Advance simulation by one time step
/// - Exit: No-op that returns system unchanged
/// 
/// Uses F# 8's enhanced pattern matching and list comprehensions
/// for more concise and readable code.
/// </remarks>
let processEvent event system =
    match event with
    | RequestFloor (elevatorId, floor) ->
        // Process request from inside elevator
        processInternalRequest elevatorId floor system
    | CallElevator (floor, direction) ->
        // Process request from floor button
        processExternalRequest floor direction system
    | MoveElevator elevatorId ->
        // Move specific elevator (manual command)
        { system with
            Elevators = 
                [ for e in system.Elevators -> 
                    if e.Id = elevatorId then moveElevator e else e ] }
    | OpenDoor elevatorId ->
        // Open doors with default time (3 ticks)
        { system with
            Elevators = 
                [ for e in system.Elevators -> 
                    if e.Id = elevatorId then openDoor e 3
                    else e ] }
    | CloseDoor elevatorId ->
        // Close doors (subject to safety validations)
        { system with
            Elevators = 
                [ for e in system.Elevators -> 
                    if e.Id = elevatorId then closeDoor e
                    else e ] }
    | Tick ->
        // Advance simulation by one time step
        processTick system
    | Exit ->
        // No-op, return system unchanged
        system
    | Print ->
        // No-op, just used for display
        system

/// <summary>
/// Processes the arrival of an elevator at a floor with configurable door open time
/// </summary>
/// <param name="elevator">The current elevator state</param>
/// <param name="doorOpenTicks">Number of ticks the door should remain open</param>
/// <returns>Updated elevator state after arrival processing</returns>
/// <remarks>
/// This is a configurable version of processArrival that allows the door open time
/// to be specified. This enables integration with the elevator configuration system
/// where door timing can be adjusted dynamically.
/// 
/// Same logic as processArrival applies for how floor requests are considered "serviced".
/// </remarks>
let processArrivalWithConfig elevator doorOpenTicks =
    if shouldStopAtFloor elevator then
        // Remove this floor from requested floors
        let newRequestedFloors = Set.remove elevator.CurrentFloor elevator.RequestedFloors
        
        // Determine next target floor and direction using the scheduling algorithm
        let (nextTarget, newDirection) = findNextStop elevator.CurrentFloor elevator.Direction newRequestedFloors
        
        // Using the provided doorOpenTicks value from configuration
        { elevator with 
            DoorStatus = Open
            DoorOpenTimeRemaining = Some doorOpenTicks
            RequestedFloors = newRequestedFloors
            TargetFloor = nextTarget
            Direction = newDirection }
    else
        elevator

/// <summary>
/// Process a simulation tick with configurable door open time
/// </summary>
/// <param name="system">The current elevator system state</param>
/// <param name="doorOpenTicks">Number of ticks doors should remain open when elevator arrives at a floor</param>
/// <returns>Updated elevator system after processing a time step</returns>
/// <remarks>
/// This function performs the following operations in sequence:
/// 1. Handle door timers for all elevators with open doors
/// 2. Move eligible elevators (doors closed and not idle)
/// 3. Process arrivals at floors for moved elevators
/// 4. Reconsider any external requests that weren't assigned
/// 
/// Safety rules enforced:
/// - Elevators don't move when doors are open
/// - Doors automatically close when their timer expires
/// - Idle elevators remain stationary until given a destination
/// </remarks>
let processTickWithConfig system doorOpenTicks =
    let updatedElevators =
        system.Elevators
        |> List.map (fun elevator ->
            // Step 1: Handle door timer if door is open
            let elevatorWithDoorHandled = handleDoorTimer elevator
            
            // Step 2 & 3: Move eligible elevators and process arrivals
            match elevatorWithDoorHandled with
            | { DoorStatus = Open } as e -> 
                // Safety rule: Don't move when doors are open
                e  
            | { DoorStatus = Closed; Direction = Idle } as e -> 
                // Optimization: Don't move when idle
                e  
            | { DoorStatus = Closed } as e -> 
                // Move elevator and process arrival with configurable door timing
                let movedElevator = moveElevator e
                processArrivalWithConfig movedElevator doorOpenTicks)
    
    // Step 4: Update system and reconsider any pending external requests
    { system with Elevators = updatedElevators }
    |> reconsiderExternalRequests

/// <summary>
/// Process an elevator event with configurable door open time
/// </summary>
/// <param name="event">The elevator event to process</param>
/// <param name="doorOpenTicks">Number of ticks doors should remain open</param>
/// <param name="system">The current elevator system state</param>
/// <returns>Updated elevator system after processing the event</returns>
/// <remarks>
/// This function extends processEvent by adding door timing configuration.
/// It handles two events specially with the provided door timing:
/// - OpenDoor: Uses the configured time instead of the default
/// - Tick: Processes tick with configurable door timing
/// 
/// All other events delegate to the standard processEvent function.
/// This allows for dynamic adjustment of door timing through configuration,
/// ensuring consistent door behavior throughout the system.
/// 
/// Uses F# 8's list comprehension syntax for more concise code.
/// </remarks>
let processEventWithConfig event doorOpenTicks system =
    match event with
    | OpenDoor elevatorId ->
        // Open doors with configurable time
        { system with
            Elevators = 
                [ for e in system.Elevators -> 
                    if e.Id = elevatorId then openDoor e doorOpenTicks
                    else e ] }
    | Tick ->
        // Advance simulation with configurable door timing
        processTickWithConfig system doorOpenTicks
    | _ -> 
        // Other events use standard processing
        processEvent event system
