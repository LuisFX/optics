type Lens<'a,'b> = ('a -> 'b) * ('b -> 'a -> 'a)
type Prism<'a,'b> =  ('a -> 'b option) * ('b -> 'a -> 'a)

type Lens =
    | Lens with
    static member (>->) (Lens, (g2, s2): Lens<'b,'c>) =
        fun ((g1, s1): Lens<'a,'b>) ->
            (fun a -> g2 (g1 a)),
            (fun c a -> s1 (s2 c (g1 a)) a) : Lens<'a,'c>

type Prism =
    | Prism with
        static member (>?>) (Prism, (g2, s2): Lens<'b,'c>) =
            fun ((g1, s1): Prism<'a,'b>) ->
                (fun a -> Option.map g2 (g1 a)),
                (fun c a -> Option.map (s2 c) (g1 a) |> function | Some b -> s1 b a
                                                                 | _ -> a) : Prism<'a,'c>

        static member (>?>) (Prism, (g2, s2): Prism<'b,'c>) =
            fun ((g1, s1): Prism<'a,'b>) ->
                (fun a -> Option.bind g2 (g1 a)),
                (fun c a -> Option.map (s2 c) (g1 a) |> function | Some b -> s1 b a
                                                                 | _ -> a) : Prism<'a,'c>
let inline L l o = (Lens >-> o) l
let inline P p o = (Prism >?> o) p

//GENERIC GET/SET
let get (g,_) = fun a -> g a

(* Lens<'a,'b> -> 'b -> 'a -> 'a *)
let set (_,s) = fun b a -> s b a

// Focus<'a,'b> -> 'a -> 'b
let inline (>->) l o = L l o

let inline (>?>) p o = P p o

let compose = fun (g1,s1) (g2,s2) -> (fun a -> g2 (g1 a)), (fun c a -> s1 (s2 c (g1 a)) a)


/////////////////////DONT WORRY ABOUT THIS STUFF/////////////////////


open System

[<RequireQualifiedAccess>]
module DateOfBirth =

    type T = private | DateOfBirth of DateOnly

    let create (text: DateOnly) = DateOfBirth text

    let value (DateOfBirth dateOfBirth) = dateOfBirth

    let toString (DateOfBirth dateOfBirth) =
        let day = dateOfBirth.Day.ToString("D2")
        let month = dateOfBirth.Month.ToString("D2")
        $"{dateOfBirth.Year}-{month}-{day}"

module User =

    type UserAccount =
        {
            AdditionalDetails : AccountDetails
        }

        static member additionalDeails_: Lens<UserAccount, AccountDetails> =
            let get = (fun a -> a.AdditionalDetails)
            let set = (fun b a -> { a with AdditionalDetails = b })
            get, set
        static member patientAccountDetails_ = AccountDetails.patientAccountDetails_

        static member maybeAdditionalDetails_: Prism<UserAccount,AccountDetails> =
            let get = (fun (a:UserAccount) -> Some a.AdditionalDetails )
            let set =
                (fun b a ->
                    { a with AdditionalDetails = b }

                )
            get, set

        static member maybePatientAccountDob_ =  UserAccount.maybeAdditionalDetails_ >?> (UserAccount.patientAccountDetails_ >?> PatientAccountDetailsB.dateOfBirth_)


    and AccountDetails =
        | PatientAccountDetails of PatientAccountDetailsB
        | NoDetails

        static member dateOfBirth_ = UserAccount.maybeAdditionalDetails_ >?> (UserAccount.patientAccountDetails_ >?> PatientAccountDetailsB.dateOfBirth_)
        static member patientAccountDetails_ : Prism<AccountDetails, PatientAccountDetailsB> =
            let get =
                (fun m ->
                    match m with
                    | PatientAccountDetails pad ->
                        printfn $"PATIENT: PatientAccountDetails {pad}"
                        Some pad
                    | _ ->
                        printfn $"NON PATIENT: {m}"
                        None
                )
            let set =
                (fun v m ->
                    match m with
                    | PatientAccountDetails pad -> m
                    | _ -> m
                )

            get, set

    and PatientAccountDetailsB =
        {
            DateOfBirth : DateOfBirth.T
        }

        static member dateOfBirth_: Lens<PatientAccountDetailsB, DateOfBirth.T> =
            let a : (PatientAccountDetailsB -> DateOfBirth.T) * (DateOfBirth.T -> PatientAccountDetailsB -> PatientAccountDetailsB) =
                (fun a -> a.DateOfBirth),
                (fun b a -> { a with DateOfBirth = b })
            a

let account1: User.UserAccount = {
    AdditionalDetails =
        User.PatientAccountDetails {
            DateOfBirth = DateOfBirth.create (DateOnly.FromDateTime (DateTime.Parse("1973-02-23")))
        }
}

// printfn $"account1: {account1}"

let account2: User.UserAccount = {
    AdditionalDetails = User.NoDetails
}


let patientDob = get User.UserAccount.maybePatientAccountDob_ account1
printfn $"patientDob: {patientDob}"






// let getAccountDetails = get User.UserAccount.additionalDeails_ account1

// let aa_bb = User.UserAccount.additionalDeails_ >?> User.UserAccount.patientAccountDetails_

// let testPatientAccountDetails = get User.UserAccount.patientAccountDetails_ getAccountDetails

// let getAccountDetailsDbo = get User.AccountDetails.dateOfBirth_ getAccountDetails

// let getPatientAccountDetailsDob = get User.PatientAccountDetailsB.dateOfBirth_ getAccountDetails

// let xx = (User.PatientAccountDetailsB.dateOfBirth_ >?> User.UserAccount.patientAccountDetails_)

