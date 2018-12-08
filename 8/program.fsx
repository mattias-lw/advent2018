open System.IO
open System.Text.RegularExpressions

let data = Regex.Split(File.ReadAllText("data.txt")," ") |> Seq.map int |> Seq.toList


let rec sumFirstTree (input:int list * int) (n:int) : int list * int =
  match input with
  | ([],_)  -> ([],0)
  | ([a],_) -> ([],0)
  | (0::nrMeta::rest,sum) -> (List.skip nrMeta rest |> Seq.toList,sum + (List.take nrMeta rest |> List.sum))
  | (nrChilds :: nrMeta :: rest,sum) -> match sumSubs nrChilds (rest |> Seq.toList, 0) with 
                                        | (newRest, subSum) -> (List.skip nrMeta newRest, sum + subSum + (List.take nrMeta newRest |> List.sum))
and sumSubs (nrChilds:int) (input:int list * int) : int list * int= 
  let childSeq = seq {1..nrChilds}
  Seq.fold sumFirstTree input childSeq
  
let sumsAccToMeta sumsList meta =
  let sums = sumsList |> List.toArray
  let res = meta |> List.filter (fun i -> (i <= Seq.length sums) && i > 0) |> List.map (fun i -> sums.[i-1])
  res
let rec sumFirstTree2 (input:int list * int list) (n:int) : int list * int list=
  match input with
  | ([],sums)  -> ([],sums)
  | ([a],sums) -> ([],sums)
  | (0::nrMeta::rest,sums) -> (List.skip nrMeta rest |> Seq.toList,List.append sums [(List.take nrMeta rest |> List.sum)])
  | (nrChilds :: nrMeta :: rest,sums) -> match sumSubs2 nrChilds (rest |> Seq.toList, []) with 
                                         | (newRest, subSums) -> (List.skip nrMeta newRest, List.append sums [(sumsAccToMeta subSums (List.take nrMeta newRest) |> List.sum)])
and sumSubs2 (nrChilds:int) (input:int list * int list) : int list * int list= 
  let childSeq = seq {1..nrChilds}
  Seq.fold sumFirstTree2 input childSeq

printfn "%A" (snd (sumFirstTree (data,0) 0))
printfn "%d" (Seq.head (snd (sumFirstTree2 (data,[]) 0)))
