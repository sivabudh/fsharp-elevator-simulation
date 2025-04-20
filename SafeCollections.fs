/// <summary>
/// Safe collection operations that return Results instead of throwing exceptions
/// </summary>
module ElevatorSimulation.SafeCollections

/// <summary>
/// Safely gets the minimum element from a set
/// </summary>
/// <param name="set">The input set</param>
/// <returns>Result with either the minimum element or an error if the set is empty</returns>
let tryMinElement set =
    if Set.isEmpty set then
        Error "Cannot get minimum element from an empty set"
    else
        Ok (Set.minElement set)

/// <summary>
/// Safely gets the maximum element from a set
/// </summary>
/// <param name="set">The input set</param>
/// <returns>Result with either the maximum element or an error if the set is empty</returns>
let tryMaxElement set =
    if Set.isEmpty set then
        Error "Cannot get maximum element from an empty set"
    else
        Ok (Set.maxElement set)

/// <summary>
/// Safely gets the first element from a list
/// </summary>
/// <param name="list">The input list</param>
/// <returns>Result with either the first element or an error if the list is empty</returns>
let tryHead list =
    match list with
    | head :: _ -> Ok head
    | [] -> Error "Cannot get head from an empty list"

/// <summary>
/// Safely gets the element with the minimum value according to a projection function
/// </summary>
/// <param name="projection">Function to project elements to comparable values</param>
/// <param name="list">The input list</param>
/// <returns>Result with either the minimum element or an error if the list is empty</returns>
let tryMinBy projection list =
    match list with
    | [] -> Error "Cannot get minimum element from an empty list"
    | _ -> Ok (List.minBy projection list)