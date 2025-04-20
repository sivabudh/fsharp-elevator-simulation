module ElevatorSimulation.Tests

open ElevatorSimulation.Types
open ElevatorSimulation.Logic
open Xunit
open FsUnit.Xunit

[<Fact>]
let ``Creating an elevator should set initial values correctly`` () =
    let elevator = createElevator 1
    
    elevator.Id |> should equal 1
    elevator.CurrentFloor |> should equal 1
    elevator.Direction |> should equal Idle
    elevator.DoorStatus |> should equal Closed
    elevator.RequestedFloors |> should be Empty
    elevator.TargetFloor |> should equal None

[<Fact>]
let ``Creating an elevator system should create the correct number of elevators`` () =
    let system = createElevatorSystem 3 10
    
    system.Elevators.Length |> should equal 3
    system.FloorCount |> should equal 10
    system.ExternalRequests |> should be Empty

[<Fact>]
let ``Calculate direction should return correct direction`` () =
    calculateDirection 1 5 |> should equal Up
    calculateDirection 5 1 |> should equal Down
    calculateDirection 3 3 |> should equal Idle

[<Fact>]
let ``Adding a requested floor should update elevator state`` () =
    let elevator = createElevator 1
    let updatedElevator = addRequestedFloor 5 elevator
    
    updatedElevator.RequestedFloors |> should contain 5
    updatedElevator.TargetFloor |> should equal (Some 5)
    
    // Adding another floor should keep first target
    let furtherUpdated = addRequestedFloor 7 updatedElevator
    furtherUpdated.RequestedFloors |> should contain 5
    furtherUpdated.RequestedFloors |> should contain 7
    furtherUpdated.TargetFloor |> should equal (Some 5)

[<Fact>]
let ``Processing internal request should update elevator correctly`` () =
    let system = createElevatorSystem 1 10
    let updatedSystem = processInternalRequest 1 5 system
    
    let elevator = updatedSystem.Elevators.[0]
    elevator.RequestedFloors |> should contain 5
    elevator.Direction |> should equal Up
    elevator.TargetFloor |> should equal (Some 5)

[<Fact>]
let ``Processing external request should update system correctly`` () =
    let system = createElevatorSystem 1 10
    let updatedSystem = processExternalRequest 5 Up system
    
    let elevator = updatedSystem.Elevators.[0]
    elevator.RequestedFloors |> should contain 5
    elevator.Direction |> should equal Up
    elevator.TargetFloor |> should equal (Some 5)

[<Fact>]
let ``Moving an elevator should update floor correctly`` () =
    // Using F# 8's enhanced record syntax
    let elevator = 
        { createElevator 1 with 
            Direction = Up
            TargetFloor = Some 5 }
    
    // Using F# 7/8 piping and assertion
    elevator
    |> moveElevator
    |> fun e -> e.CurrentFloor |> should equal 2
    
    // Using F# 8's improved record creation and construction
    let downElevator = 
        { createElevator 1 with 
            CurrentFloor = 5
            Direction = Down
            TargetFloor = Some 1 }
    
    // Using F# 7/8 piping for improved readability
    downElevator
    |> moveElevator
    |> fun e -> e.CurrentFloor |> should equal 4

[<Fact>]
let ``Elevator should stop at requested floors`` () =
    let elevator = 
        { createElevator 1 with 
            CurrentFloor = 3
            Direction = Up
            TargetFloor = Some 5
            RequestedFloors = Set.ofList [3; 5] }
    
    shouldStopAtFloor elevator |> should be True
    
    let notStoppingElevator = 
        { elevator with 
            CurrentFloor = 4
            RequestedFloors = Set.ofList [3; 5] }
    
    shouldStopAtFloor notStoppingElevator |> should be False

[<Fact>]
let ``Processing arrival should update elevator correctly when at target floor`` () =
    let elevator = 
        { createElevator 1 with 
            CurrentFloor = 5
            Direction = Up
            TargetFloor = Some 5
            RequestedFloors = Set.ofList [5] }
    
    let arrivedElevator = processArrival elevator
    
    arrivedElevator.DoorStatus |> should equal Open
    arrivedElevator.RequestedFloors |> should be Empty
    arrivedElevator.Direction |> should equal Idle
    arrivedElevator.TargetFloor |> should equal None

[<Fact>]
let ``Processing arrival should update direction when there are more floors to visit`` () =
    let elevator = 
        { createElevator 1 with 
            CurrentFloor = 3
            Direction = Up
            TargetFloor = Some 3
            RequestedFloors = Set.ofList [3; 5; 7] }
    
    let arrivedElevator = processArrival elevator
    
    arrivedElevator.DoorStatus |> should equal Open
    arrivedElevator.RequestedFloors |> should equal (Set.ofList [5; 7])
    arrivedElevator.Direction |> should equal Up
    arrivedElevator.TargetFloor |> should equal (Some 5)

[<Fact>]
let ``validateDoorTransition should enforce safety rules`` () =
    // Test case 1: Cannot open a door that's already open
    let openDoorElevator = 
        { createElevator 1 with 
            DoorStatus = Open
            DoorOpenTimeRemaining = Some 2 }
    match validateDoorTransition openDoorElevator Open with
    | Error _ -> true |> should be True  // Expect error
    | Ok _ -> false |> should be True    // Force failure if we get Ok
    
    // Test case 2: Cannot close a door that's already closed
    let closedDoorElevator = createElevator 1  // Default is closed
    match validateDoorTransition closedDoorElevator Closed with
    | Error _ -> true |> should be True  // Expect error
    | Ok _ -> false |> should be True    // Force failure if we get Ok
    
    // Test case 3: Cannot open doors while moving between floors
    let movingElevator = 
        { createElevator 1 with 
            Direction = Up
            TargetFloor = Some 5
            CurrentFloor = 3 }  // Not a requested floor
    match validateDoorTransition movingElevator Open with
    | Error _ -> true |> should be True  // Expect error
    | Ok _ -> false |> should be True    // Force failure if we get Ok
    
    // Test case 4: Cannot close doors while timer is still active
    let activeTimerElevator = 
        { createElevator 1 with 
            DoorStatus = Open
            DoorOpenTimeRemaining = Some 2 }
    match validateDoorTransition activeTimerElevator Closed with
    | Error _ -> true |> should be True  // Expect error
    | Ok _ -> false |> should be True    // Force failure if we get Ok
    
    // Test case 5: Can open doors at a requested floor
    let atDestinationElevator = 
        { createElevator 1 with 
            CurrentFloor = 5
            RequestedFloors = Set.ofList [5]
            Direction = Up }
    validateDoorTransition atDestinationElevator Open 
    |> function 
       | Ok _ -> true |> should be True 
       | Error _ -> false |> should be True
    
    // Test case 6: Can close doors when timer is expired
    let expiredTimerElevator = 
        { createElevator 1 with 
            DoorStatus = Open
            DoorOpenTimeRemaining = Some 0 }
    validateDoorTransition expiredTimerElevator Closed 
    |> function 
       | Ok _ -> true |> should be True
       | Error _ -> false |> should be True

[<Fact>]
let ``findNextStop should calculate correct next floor and direction`` () =
    // Test case 1: Going up with floors above - should continue going up
    let requestedFloors1 = Set.ofList [5; 7; 9]
    let (target1, direction1) = findNextStop 3 Up requestedFloors1
    target1 |> should equal (Some 5)  // Nearest floor above
    direction1 |> should equal Up     // Continue going up
    
    // Test case 2: Going up with no floors above - should reverse direction
    let requestedFloors2 = Set.ofList [1; 3]
    let (target2, direction2) = findNextStop 5 Up requestedFloors2
    target2 |> should equal (Some 3)  // Highest floor below
    direction2 |> should equal Down   // Reverse to down
    
    // Test case 3: Going down with floors below - should continue going down
    let requestedFloors3 = Set.ofList [2; 4; 6]
    let (target3, direction3) = findNextStop 8 Down requestedFloors3
    target3 |> should equal (Some 6)  // Nearest floor below
    direction3 |> should equal Down   // Continue going down
    
    // Test case 4: Going down with no floors below - should reverse direction
    let requestedFloors4 = Set.ofList [5; 7]
    let (target4, direction4) = findNextStop 2 Down requestedFloors4
    target4 |> should equal (Some 5)  // Lowest floor above
    direction4 |> should equal Up     // Reverse to up
    
    // Test case 5: Idle with equidistant floors - should choose lowest
    let requestedFloors5 = Set.ofList [3; 7]  // Both 2 floors away from current floor 5
    let (target5, direction5) = findNextStop 5 Idle requestedFloors5
    target5 |> should equal (Some 3)  // Closest floor (lowest in case of tie)
    direction5 |> should equal Down   // Direction to this floor
    
    // Test case 6: Empty requested floors - should become idle with no target
    let emptyFloors = Set.empty
    let (target6, direction6) = findNextStop 5 Up emptyFloors
    target6 |> should equal None      // No target
    direction6 |> should equal Idle   // Become idle

[<Fact>]
let ``System tick should move all elevators`` () =
    // Using F# 8's improved record syntax and collection literals
    let elevator1 = 
        { createElevator 1 with 
            CurrentFloor = 1
            Direction = Up
            TargetFloor = Some 3
            RequestedFloors = Set.ofList [3]
            DoorStatus = Closed }
    
    // Using F# 8's enhanced record construction
    let elevator2 = 
        { createElevator 2 with 
            CurrentFloor = 5
            Direction = Down
            TargetFloor = Some 1
            RequestedFloors = Set.ofList [1]
            DoorStatus = Closed }
    
    // Using F# 8's list literals and record updates
    let system = { createElevatorSystem 2 10 with Elevators = [elevator1; elevator2] }
    
    // Using F# 7/8 pipeline for better readability
    let tickedSystem = processTick system
    
    // Using F# 7/8's improved lambda expressions and pattern matching
    tickedSystem.Elevators 
    |> List.find (fun e -> e.Id = 1)
    |> fun e -> e.CurrentFloor |> should equal 2
    
    tickedSystem.Elevators
    |> List.find (fun e -> e.Id = 2) 
    |> fun e -> e.CurrentFloor |> should equal 4
