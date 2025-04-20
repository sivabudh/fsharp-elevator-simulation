module ValidationTests

open ElevatorSimulation.Types
open ElevatorSimulation.Config
open ElevatorSimulation.System
open Validation

// These validation tests will be implemented in future updates
// Don't run these tests yet as we're incrementally implementing the validation system

(*
let testParseValidCall () =
    match tryParseCommand "call 3 up" with
    | Ok (Call req) when req.Floor = 3 && req.Direction = Up -> 
        printfn "PASS: call"
    | _ -> failwith "FAIL: call parsing"

let testParseInvalidCall () =
    match tryParseCommand "call -1 down" with
    | Error _ -> printfn "PASS: invalid call"
    | _ -> failwith "FAIL: negative floor not caught"

let testInvalidElevator () =
    let config = { FloorCount = 5; ElevatorCount = 2; TickIntervalMs = 1000; DoorOpenTicks = 3 }
    let state = createSimulationWithConfig config
    let cmd = Request (5, 2) // Invalid elevator ID
    match applyCommand config cmd state.System with
    | Error err -> printfn "PASS: invalid elevator ID - %s" (err.ToString())
    | Ok _ -> failwith "FAIL: invalid elevator ID not caught"

let runAllTests () =
    printfn "Running validation tests..."
    testParseValidCall ()
    testParseInvalidCall ()
    testInvalidElevator ()
    printfn "All validation tests completed."
*)

// Placeholder for validation tests
printfn "Validation testing framework is being set up..."