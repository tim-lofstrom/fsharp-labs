open System

exception CommandNotRegisteredException of string

type Command =
    | Add
    | Lookup
    | Exit
    | InvalidCommand
    | NoCommand

type State = Map<string, string array>

let addFunction (state: State) (args: string array option) : State =
    match args with
    | Some [| name; number |] ->
        let exists = state |> Map.containsKey name
        match exists with
        | true ->
            state
            |> Map.find name
            |> (fun numbers -> state |> Map.add name (Array.append [| number |] numbers))
        | false -> state |> Map.add name [| number |]
    | _ -> state

let lookupFunction (state: State) (args: string array option) : State =
    match args with
    | Some [| name |] ->
        let result = Map.tryFind name state
        match result with
        | Some value -> Array.iter (printfn "%s") value
        | None -> printfn "Key not found"
    | _ -> printfn "Missing args"

    state

let invalidCommand (state: State) (args: string array option) : State =
    printfn "Kommandot finns inte!"
    state

let noCommand (state: State) (args: string array option) : State = state

type CommandHandler = State -> string array option -> State

let commandMap: Map<Command, CommandHandler> =
    Map
        [ Add, addFunction
          Lookup, lookupFunction
          InvalidCommand, invalidCommand
          NoCommand, noCommand ]

let parseCommand (part: string option) : Command =
    match part with
    | Some "exit" -> Exit
    | Some "add" -> Add
    | Some "lookup" -> Lookup
    | None -> NoCommand
    | _ -> InvalidCommand

let executeCommand command args state =
    match Map.tryFind command commandMap with
    | Some func -> func state args
    | None -> raise (CommandNotRegisteredException("Command not registered: " + string command))

let tryTail (arr: string array) : (string array option) =
    match arr with
    | [||] -> None
    | [| _ |] -> None
    | _ -> Some(Array.tail arr)

let rec phonebook state =
    printf "phonebook>"
    let input = Console.ReadLine()

    let parts =
        input.Split(" ")
        |> Array.filter (fun str -> not (String.IsNullOrWhiteSpace(str)))

    let command = (Array.tryHead parts) |> parseCommand
    let args = tryTail parts

    match command with
    | Exit -> 0
    | _ -> phonebook (executeCommand command args state)

let initialState: Map<string, string array> = Map.empty

[<EntryPoint>]
let main _argv = phonebook initialState
