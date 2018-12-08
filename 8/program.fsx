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
  

sumFirstTree (data,0) 0