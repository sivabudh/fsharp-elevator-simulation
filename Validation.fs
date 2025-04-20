module Validation

open System
open ElevatorSimulation.Types
open ElevatorSimulation.Config

// Validation error types
type ValidationError =
    | InvalidFloor of int * int  // floor, max floor
    | InvalidElevatorId of int * int  // id, max id  
    | InvalidDirection of string
    | SameFloorRequest of int
    | ParseError of string
    | RuntimeError of string
    override this.ToString() =
        match this with
        | InvalidFloor (floor, max) -> sprintf "Floor %d is invalid. Valid range: 1-%d" floor max
        | InvalidElevatorId (id, max) -> sprintf "Elevator %d is invalid. Valid range: 1-%d" id max
        | InvalidDirection dir -> sprintf "Invalid direction '%s'. Use 'up' or 'down'" dir
        | SameFloorRequest floor -> sprintf "Elevator is already at floor %d" floor
        | ParseError msg -> sprintf "Parse error: %s" msg
        | RuntimeError msg -> sprintf "Runtime error: %s" msg

// Validation functions
let validateFloor config floor =
    if floor >= 1 && floor <= config.FloorCount then
        Ok ()
    else
        Error (InvalidFloor(floor, config.FloorCount))

let validateElevatorId config id =
    if id >= 1 && id <= config.ElevatorCount then
        Ok ()
    else
        Error (InvalidElevatorId(id, config.ElevatorCount))

// Command parsing
let tryParseInt (s: string) =
    match Int32.TryParse(s) with
    | true, value -> Ok value
    | _ -> Error (ParseError(sprintf "Could not parse '%s' as a number" s))

let tryParseDirection (s: string) =
    match s.ToLowerInvariant() with
    | "up" -> Ok Up
    | "down" -> Ok Down
    | _ -> Error (InvalidDirection(s))

// Command parsing with validation - temporary placeholder until Command type is reimplemented
(*
let tryParseCommand (input: string) =
    let tokens = input.Trim().Split(' ', StringSplitOptions.RemoveEmptyEntries)
    
    match tokens with
    | [| "exit" |] -> Ok Exit
    | [| "tick" |] -> Ok Tick
    | [| "print" |] -> Ok Print
    | [| "call"; floorStr; dirStr |] ->
        // Parse floor
        tryParseInt floorStr
        |> Result.bind (fun floor ->
            // Parse direction
            tryParseDirection dirStr
            |> Result.map (fun dir ->
                // Create floor call request
                Call { Floor = floor; Direction = dir }
            )
        )
    | [| "request"; elevatorStr; floorStr |] ->
        // Parse elevator ID
        tryParseInt elevatorStr
        |> Result.bind (fun elevatorId ->
            // Parse floor
            tryParseInt floorStr
            |> Result.map (fun floor ->
                // Create elevator request
                Request (elevatorId, floor)
            )
        )
    | _ -> Error (ParseError "Invalid command format")
*)

// Command/Event conversion functions temporarily commented out
// Will be reimplemented when validation features are added back
(*
// Convert Command type to ElevatorEvent type
let commandToEvent cmd : ElevatorEvent =
    match cmd with
    | Call req -> CallElevator (req.Floor, req.Direction)
    | Request (eid, floor) -> RequestFloor (eid, floor)
    | Tick -> Tick
    | Print -> Print
    | Exit -> Exit
    
// Convert ElevatorEvent type to Command type
let eventToCommand evt : Command =
    match evt with
    | CallElevator (floor, dir) -> Call { Floor = floor; Direction = dir }
    | RequestFloor (eid, floor) -> Request (eid, floor)
    | Tick -> Tick
    | Print -> Print
    | Exit -> Exit
    | _ -> Tick // Default case for events that don't map directly to commands
*)