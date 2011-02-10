module FParsec.Applicative

open System
open FParsec.Primitives
open Microsoft.FSharp.Core.Operators.Unchecked 

/// ap :: Monad m => m (a -> b) -> m a -> m b
let inline ap f a = f >>= fun f' -> a >>= fun a' -> preturn (f' a') 

/// (<*>) :: Applicative f => f (a -> b) -> f a -> f b
let inline (<*>) f a = ap f a

/// (<**>) :: Applicative f => f a -> f (a -> b) -> f b
let inline apr f a = a <*> f
let inline (<**>) f a = apr f a

/// liftA :: Applicative f => (a -> b) -> f a -> f b
let inline liftA f a = a |>> f 

/// (<$>) :: Functor f => (a -> b) -> f a -> f b
let inline (<!>) f a = liftA f a 

/// (<$) :: Functor f => a -> f b -> f a
let inline (<!) f a = preturn f .>> a
let inline (!>) f a = f >>. preturn a

/// liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
let inline liftA2 f a b = pipe2 a b f         // preturn f <*> a <*> b 
let inline (<!!>) f a b = liftA2 f a b 

/// liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
let inline liftA3 f a b c = pipe3 a b c f
let inline (<!!!>) f a b c = liftA3 f a b c

/// (*>) :: Applicative f => f a -> f b -> f b
let inline ( *>) x y = x >>. y                 // liftA2 (fun _ z -> z) x y 

/// (<*) :: Applicative f => f a -> f b -> f a
let inline ( <*) x y = x .>> y                 // liftA2 (fun z _ -> z) x y 

/// sequenceA :: Applicative f => [f a] -> f [a]
let sequenceA ps = List.foldBack (liftA2 (fun x y -> x::y)) ps (preturn [])

/// sequenceA_ :: Applicative f => [f a] -> f ()
let sequenceA_ ps = List.fold ( *>) (preturn ()) ps

/// mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
let mapA f xs = sequenceA (List.map f xs)

/// mapA_ :: Applicative f => (a -> f b) -> [a] -> f ()
let mapA_ f xs = sequenceA_ (List.map f xs)

/// foreverA :: Applicative f => f a -> f b
let rec foreverA a = a *> foreverA a

/// asum :: Alternative f => [f a] -> f a
let sumA ps = List.fold (<|>) (preturn defaultof<'a>) ps

//filterA :: Applicative f => (a -> f Bool) -> f [a] -> f [a]
let filterA f ps = 
  let addIf x b xs = if b then x::xs else xs
  let consA x a = liftA2 (addIf x) (f x) a
  List.foldBack consA ([]) ps

/// zipWithA :: Applicative f => (a -> b -> f c) -> [a] -> [b] -> f [c]
let map2A f xs ys = sequenceA (List.map2 f xs ys)

/// zipWithA_ :: Applicative f => (a -> b -> f c) -> [a] -> [b] -> f ()
let map2A_ f xs ys = sequenceA_ (List.map2 f xs ys)

/// mapAndUnzipA :: Applicative f => (a -> f (b, c)) -> [a] -> f ([b], [c])
let mapAndUnzipA f xs = liftA List.unzip (mapA f xs)

/// replicateA :: Applicative f => Int -> f a -> f [a]
let replicateA n a = sequenceA (List.replicate n a)

/// replicateA_ :: Applicative f => Int -> f a -> f ()
let replicateA_ n a = sequenceA_ (List.replicate n a)

/// unlessA :: Applicative f => Bool -> f () -> f ()
let unlessA b a = if b then preturn () else a

/// guardA :: Alternative f => Bool -> f ()
let guardA b = unlessA b (preturn ())

/// whenA :: Applicative f => Bool -> f () -> f ()
let whenA b a = if b then a else preturn ()

//let (<*>) f a = 
//    parse { 
//        let! f' = f 
//        let! a' = a 
//        return f' a' } 
//let (<!>) f a = preturn f <*> a
//let ( *>) a b = preturn (fun resultA resultB -> resultB) <*> a <*> b

//let empty<'a> = preturn defaultof<'a>

//open FParsec.CharParsers 
//
//// コンピューテーション式で
//let foo m1 m2 f = parse {let! a= m1
//                         let! b = m2
//                         return f a b}
//
//// Applicative parsingで
//let foo'' m1 m2 f = f <!> m1 <*> m2

//let readHex a b = 
//    char <| Byte.Parse(sprintf "%c%c" a b, System.Globalization.NumberStyles.HexNumber)
//
//let a_hex4 : Parser<char, unit> = 
//    readHex <!> skipChar '%' *> hex <*> hex 
//
//let a_hex5 : Parser<char, unit> = 
//    readHex |> pipe2 (skipChar '%' >>. hex) hex