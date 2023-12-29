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






//TEST

// Lens<'a,'b> -> Lens<'b,'c> -> Lens<'a,'c> *)
type MemberAccount =
    { B: RecordB }
    static member B_ =
        (fun a -> a.B), //get
        (fun b a -> { a with B = b}) //set

and RecordB =
    { C: RecordC }
    static member C_ =
        (fun b -> b.C), //get
        (fun c b -> { b with C = c }) //set

and RecordC =
    { D: RecordD }
    static member D_ = (fun b -> b.D), (fun d b -> { b with D = d })

and RecordD =
    { PhoneNumber: string }
    static member PhoneNumber_ =
        (fun b -> b.PhoneNumber), //get
        (fun value b -> { b with PhoneNumber = value }) //set


// let a b = b

// let c d = d

// let f = c >> a

// let memberPhoneNumber_get_set =  (MemberAccount.B_ >-> RecordB.C_ >-> RecordC.D_ >-> RecordD.PhoneNumber_)
// ////////////////////////////////////////////////////////////////////////////////////////////


// let myMemberAccount = { B = { C = { D = { PhoneNumber = "99999999" } } } }

// let memberPhoneNumber = myMemberAccount |> get memberPhoneNumber_get_set

// let memberWtithNewPhoneNumber = myMemberAccount |> set memberPhoneNumber_get_set "555-555-5555"

// let whatsMySessionTokenPermission =  "" ///////?????????????










// let theMagic = get a_b_c a

// let cV = get b_c bV

open System

//TEST PRISMS

type RecordAA =
    { BB: RecordBB option }
    static member BB_ = (fun a -> a.BB), (fun b a -> { a with BB = Some b })

 and RecordBB =
    {
        VValue: string
        ValueC: RecordCC
    }
    static member VValue_ = (fun b -> b.VValue), (fun value b -> { b with VValue = value })
    static member RecordC_ = (fun b -> b.ValueC), (fun value c -> { c with ValueC = value } )
and RecordCC =
    {
        CCValue: string
    }
    static member CCValue_ = (fun c -> c.CCValue ), (fun value c -> { c with CCValue = value } )



// //Prism<RecordA,string>
let aavalue_ = RecordAA.BB_ >?> RecordBB.VValue_

let a_b_c = RecordAA.BB_ >?> (RecordBB.RecordC_ >-> RecordCC.CCValue_)

// let aaValueC_ = RecordAA.BB_ >?> (RecordBB.RecordC_ >-> RecordCC.CCValue_)

let recordAA =
    {
        BB = Some {
            VValue = "TTEST PRISM"
            ValueC = { CCValue = "CCCCCCC" }
        }
    }

let prismValue = get a_b_c recordAA

printfn $" "

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
        // type Lens<'a,'b> = ('a -> 'b) * ('b -> 'a -> 'a)

        static member additionalDeails_: Lens<UserAccount, AccountDetails> =
            let get = (fun a -> a.AdditionalDetails)
            let set = (fun b a -> { a with AdditionalDetails = b })
            get, set
        static member patientAccountDetails_ = AccountDetails.patientAccountDetails_

    and AccountDetails =
        | PatientAccountDetails of PatientAccountDetailsB
        | NoDetails

        static member dateOfBirth_ = (UserAccount.patientAccountDetails_ >?> PatientAccountDetailsB.dateOfBirth_)
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
                    | PatientAccountDetails pad -> m // { m with PatientAccountDetails = pad }
                    | _ -> m
                )

            get, set
                // (fun i m ->
                //     match m with
                //     | PatientAccountDetails pad -> PatientAccountDetails i
                //     | m -> m)

    and PatientAccountDetailsB =
        {
            DateOfBirth : DateOfBirth.T
        } //(PatientAccountDetailsB -> DateOfBirth) * (DateOfBirth -> PatientAccountDetailsB -> PatientAccountDetailsB)
        
        static member dateOfBirth_: Lens<PatientAccountDetailsB, DateOfBirth.T> =
            let a : (PatientAccountDetailsB -> DateOfBirth.T) * (DateOfBirth.T -> PatientAccountDetailsB -> PatientAccountDetailsB) =
                (fun a -> a.DateOfBirth),
                (fun b a -> { a with DateOfBirth = b })
            a

            // let dob = Antidote.Core.V2.Types.DateOfBirth.value
            // ( fun (a: PatientAccountDetails) -> a )
            // (fun (b:DateOnly) (a:PatientAccountDetails) -> { a with DateOfBirth = DateOfBirth.create b })

let patientAccountDetails : User.PatientAccountDetailsB = {
    DateOfBirth = DateOfBirth.create (DateOnly.FromDateTime (DateTime.Parse("1973-02-23")))
}
let accountDetails: User.AccountDetails = User.PatientAccountDetails patientAccountDetails

let account: User.UserAccount = {
    AdditionalDetails = accountDetails
}

let getAccountDetails = get User.UserAccount.additionalDeails_ account

// let testPatientAccountDetails = get User.UserAccount.patientAccountDetails_ account

let getAccountDetailsDbo = get User.AccountDetails.dateOfBirth_ accountDetails
let getPatientAccountDetailsDob = get User.PatientAccountDetailsB.dateOfBirth_ patientAccountDetails

// let xx = (User.PatientAccountDetailsB.dateOfBirth_ >?> User.UserAccount.patientAccountDetails_)