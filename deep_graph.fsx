type Focus<'a,'b> = ('a -> 'b) * ('b -> 'a -> 'a)

type Lens =
    | Lens with
    static member (>->) (Lens, (g2, s2): Focus<'b,'c>) =
        fun ((g1, s1): Focus<'a,'b>) ->
            (fun a -> g2 (g1 a)),
            (fun c a -> s1 (s2 c (g1 a)) a) : Focus<'a,'c>

type Prism<'a,'b> =  ('a -> 'b option) * ('b -> 'a -> 'a)

    type Prism =
        | Prism with

        static member (>?>) (Prism, (g2, s2): Focus<'b,'c>) =
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
