module ElevatorSimulation.Config

/// Configuration for the elevator simulation
type SimulationConfig = {
    /// Number of elevators in the system
    ElevatorCount: int
    
    /// Number of floors in the building
    FloorCount: int
    
    /// Time between automatic ticks (in milliseconds)
    TickIntervalMs: int
    
    /// Number of ticks to keep elevator doors open
    DoorOpenTicks: int
}

/// Default configuration
let defaultConfig = {
    ElevatorCount = 3
    FloorCount = 10
    TickIntervalMs = 1000
    DoorOpenTicks = 3
}

/// Creates a custom configuration
/// Uses F# 8's enhanced record creation syntax
let createConfig elevatorCount floorCount tickIntervalMs doorOpenTicks = 
    // Using the simplified record creation from F# 8
    { 
        ElevatorCount = elevatorCount
        FloorCount = floorCount
        TickIntervalMs = tickIntervalMs
        DoorOpenTicks = doorOpenTicks
    }