open System.IO
open System.Text.RegularExpressions

let data = Regex.Split(File.ReadAllText("data.txt")," ") |> Seq.map int |> Seq.toList

let sum = ref 0;
let skipAndSum n list =
  let localSum = (List.take n list |> List.sum)
  sum := !sum + localSum
  List.skip n list


let rec sumFirstTree input n =
  match input with
  | [] -> []
  | [a] -> []
  | 0::nrMeta::rest -> skipAndSum nrMeta rest |> Seq.toList
  | nrChilds :: nrMeta :: rest -> skipAndSum nrMeta (sumSubs nrChilds (rest |> Seq.toList))
and sumSubs nrChilds input = 
  let childSeq = seq {1..nrChilds}
  Seq.fold sumFirstTree input childSeq
  

sumFirstTree data 0
printfn "%d" !sum
