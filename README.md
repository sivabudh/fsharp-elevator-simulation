# F# Elevator Simulation

A sophisticated elevator simulation system built with F# that demonstrates functional programming principles for complex state management and multi-elevator coordination.

## Overview

This project implements a full-featured elevator control system that can:
- Manage multiple elevators
- Process user requests from inside and outside elevators
- Schedule elevators efficiently
- Visualize elevator positions and states in real-time
- Support both interactive and demo modes

## Architecture

The system is built using functional programming principles, with immutable data structures and pure functions. The core components are:

### ElevatorConfig.fs
- Centralized configuration settings
- Customizable parameters for simulation behavior
- Door timer settings and interval configuration

### ElevatorTypes.fs
- Defines the core domain model
- Includes discriminated unions for Direction, DoorStatus, Request types
- Defines record types for elevator and system state
- Implements door timer mechanism

### ElevatorLogic.fs
- Implements core logic for elevator scheduling
- Contains algorithms for elevator selection
- Handles elevator movement, door operations, and request management
- Manages door open/close timing

### ElevatorSystem.fs
- Manages the overall simulation
- Processes user commands
- Maintains simulation state
- Provides the main simulation loop
- Applies configuration to simulation parameters

### ElevatorUI.fs
- Renders the text-based visualization
- Displays elevator positions and status
- Shows door states with visual indicators

### Program.fs
- Serves as the application entry point
- Handles command-line arguments
- Implements the demo scenario
- Sets up simulation with configurable options

### Tests.fs
- Contains unit tests for system components

## Running the Simulation

### Prerequisites
- .NET 6.0 SDK or higher

### Build and Run
```bash
dotnet build
dotnet run
```

### Demo Mode
Run with the `--demo` flag to see an automated demonstration:
```bash
dotnet run -- --demo
```

## Interactive Commands

The simulation accepts the following commands in interactive mode:

- `call <floor> <direction>` - Call an elevator to a floor (direction: up/down)
  - Example: `call 5 up`
- `request <elevator> <floor>` - Request an elevator to go to a floor
  - Example: `request 1 8`
- `tick` - Advance the simulation by one time step
- `auto` - Enable automatic ticking (continuous simulation)
- `stop` - Disable automatic ticking
- `help` - Display available commands
- `exit` - Exit the simulation

## Design Principles

The system demonstrates several functional programming concepts:
- Immutable data structures
- Pure functions for state transitions
- Pattern matching for control flow
- Higher-order functions
- Discriminated unions for rich type modeling

## Configuration and Customization

The simulation is highly configurable through the `ElevatorConfig.fs` module:

```fsharp
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
```

Key configuration features:
- **Door Timer**: Elevators keep doors open for a configurable number of ticks before automatically closing
- **Tick Interval**: Control the speed of the simulation in auto-tick mode
- **Building Size**: Customize the number of elevators and floors
- **Default Configuration**: Predefined settings for quick startup

## Scheduling Algorithm

The elevator scheduling algorithm considers:
1. Direction of travel
2. Current floor
3. Requested destinations
4. External calls
5. Idle elevators

The system prioritizes elevators already moving in the direction of a call and picks the closest available elevator when multiple options exist.

## Visualization

The text-based UI displays:
- Elevator positions across all floors
- Direction of travel for each elevator
- Door status (open/closed)
- Current floor and target floor information
- Pending requests for each elevator

Example:
```
Elevator Simulation
===================
   | 1   2   3 |
---+-------------+
10 |           |
 9 |           |
 8 |    [2]    |
 7 |           |
 6 |           |
 5 |[1]        |
 4 |           |
 3 |           |
 2 |           |
 1 |        |3||
---+-------------+
Elevator Status:
Elevator 1: Floor 5, Direction ↑, Door Open, Target: 10, Requests: 3, 10
Elevator 2: Floor 8, Direction ↓, Door Open, Target: 7, Requests: 7
Elevator 3: Floor 1, Direction -, Door Closed, Target: None, Requests: None
```

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

### Development Setup
1. Clone the repository
2. Install the .NET 6.0 SDK
3. Open in your favorite F# editor (Visual Studio, VS Code with Ionide, JetBrains Rider)
4. Build with `dotnet build`
5. Run tests with `dotnet test`

## License

This project is licensed under the MIT License - see the LICENSE file for details.