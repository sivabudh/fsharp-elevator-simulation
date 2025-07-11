module ElevatorSimulation.Types

/// Direction the elevator is moving
type Direction =
    | Up
    | Down
    | Idle
    // Using F# 7 string interpolation improvements
    override this.ToString() = 
        match this with
        | Up -> $"↑"
        | Down -> $"↓"
        | Idle -> $"-"

/// Status of the elevator door
type DoorStatus =
    | Open
    | Closed
    // Using F# 7 string interpolation improvements
    override this.ToString() = 
        match this with
        | Open -> $"Open"
        | Closed -> $"Closed"

/// Internal request parameters
type InternalRequestParams = {
    ElevatorId: int
    DestinationFloor: int
}

/// External request parameters
type ExternalRequestParams = {
    Floor: int
    Direction: Direction
}

/// Request to the elevator system, can be internal (from inside the elevator) or external (from a floor)
type Request =
    | Internal of InternalRequestParams
    | External of ExternalRequestParams

/// State of a single elevator
type ElevatorState = {
    Id: int
    CurrentFloor: int
    TargetFloor: int option
    Direction: Direction
    DoorStatus: DoorStatus
    /// Remaining ticks before door closes (when DoorStatus is Open)
    DoorOpenTimeRemaining: int option
    /// Set of floors this elevator needs to visit
    RequestedFloors: Set<int>
}

/// Represents the entire elevator system
type ElevatorSystem = {
    Elevators: ElevatorState list
    ExternalRequests: (int * Direction) list  // Floors with external requests pending
    FloorCount: int
}

/// Possible events in the elevator system
type ElevatorEvent =
    | RequestFloor of int * int  // ElevatorId, DestinationFloor
    | CallElevator of int * Direction  // Floor, Direction
    | MoveElevator of int  // Elevator Id
    | OpenDoor of int  // Elevator Id
    | CloseDoor of int  // Elevator Id
    | Tick  // Simulation time tick
    | Exit  // Exit the simulation
    | Print // Display current system state

/// Floor call request data
type FloorCallRequest = {
    Floor: int
    Direction: Direction
}

/// Elevator request data 
type ElevatorRequest = {
    ElevatorId: int
    TargetFloor: int
}

/// Command types for the validation system - temporarily commented out
(*
type Command =
    | Call of FloorCallRequest
    | Request of int * int  // ElevatorId, TargetFloor
    | Tick
    | Print
    | Exit
*)
