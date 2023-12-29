open System

type Field =
    | Fullname
    | FirstName
    | LastName
    | EmailAddress
    | PhoneNumber

    member x.toPredicate =
        match x with
        | Fullname -> "fullname", "\"FirstName\" || \"LastName\" LIKE '%{{fullname}}%'"
        | FirstName -> "firstname", "\"FirstName\" LIKE '%{{firstname}}%'"
        | LastName -> "lastname", "\"LastName\" LIKE '%{{lastname}}%'"
        | EmailAddress -> "email", "\"Email\" LIKE '%{{email}}%'"
        | PhoneNumber -> "phonenumber", " \"MobilePhone\" LIKE '{{phonenumber}}%'"

type FieldFilter = Field * string

type FilterOperation =
    | And of FieldFilter list
    | Or of FieldFilter list

    member x.toString =
        match x with
        | And _ -> "AND"
        | Or _ -> "OR"

type ConditionOperation =
    | And of Condition
    | Or of Condition

    member x.toString =
        match x with
        | And _ -> "AND"
        | Or _ -> "OR"

and Condition = {
    FilterOperation: FilterOperation
    ConditionOperation: ConditionOperation option
}

let rec generateWhereClause (condition: Condition) =
    let filterOperationString =
        match condition.FilterOperation with
        | FilterOperation.And filters ->
            filters
            |> List.map (fun (field, value) ->
                let fieldname, pred = field.toPredicate
                pred.Replace("{{" + fieldname + "}}", value))
            |> String.concat " AND "

        | FilterOperation.Or filters ->
            filters
            |> List.map (fun (field, value) ->
                let fieldname, pred = field.toPredicate
                pred.Replace("{{" + fieldname + "}}", value))
            |> String.concat " OR "

    printfn $"filterOperationString: {filterOperationString}"

    match condition.ConditionOperation with
    | Some innerCondition ->
        match innerCondition with
        | And cc ->
            $"({filterOperationString}) {innerCondition.toString} ({generateWhereClause cc})"

        | Or cc ->
            $"({filterOperationString}) {innerCondition.toString} ({generateWhereClause cc})"
    | None -> filterOperationString


let condition1 = {
    FilterOperation = FilterOperation.And [ Field.FirstName, "John"; Field.LastName, "Doe" ]
    ConditionOperation = Some (
        And {
            FilterOperation = FilterOperation.Or [ Field.EmailAddress, ""; Field.PhoneNumber, "" ]
            ConditionOperation = None
        }
    )
}

generateWhereClause condition1
