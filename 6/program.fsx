open System.IO
open System.Text.RegularExpressions

let data = File.ReadAllLines("data.txt") 
          |> Seq.map (fun l -> Regex.Matches(l,"\\d+") |> Seq.map (fun m -> m.Groups.[0].Captures.[0].Value))
          |> Seq.map Seq.toArray
          |> Seq.map (fun a -> (int a.[0],int a.[1])) 


let distance = fun a b ->
  match a,b with
  | (x1,y1),(x2,y2) -> (abs (x1 - x2)) + (abs (y1 - y2))


let dists = fun d dm c ->  
  Map.add c (match (Map.tryFind c dm),d,(distance c d) with
             |None,d , dist -> (dist,d)
             |Some (odist,(ox,oy)),d,dist when dist < odist -> (dist,d)
             |Some (odist,(ox,oy)),_,dist when dist = odist -> (dist,(2147483647,2147483647))
             |Some o,_,_ -> o) dm
let calc = fun dim data->
  let cords = seq {for x in -dim.. (dim-1) do
                    for y in -dim .. (dim-1) do
                      yield (x,y)
  }
  Seq.fold (fun m d -> Seq.fold (dists d) m cords) Map.empty data |> Map.toSeq |> Seq.map snd |> Seq.groupBy snd |> Seq.map (fun e -> (Seq.length (snd e), (fst e))) |>Set.ofSeq

let distsAcc = fun d dm c ->  
  Map.add c (match (Map.tryFind c dm),d,(distance c d) with
             |None,d , dist -> dist
             |Some odist,d,dist when (dist + odist) < 10000 -> dist + odist
             |Some _,_,_ -> 999999) dm

let calc2 = fun dim data-> 
  let cords = seq {for x in -dim.. (dim-1) do
                    for y in -dim .. (dim-1) do
                      yield (x,y)}
  Seq.fold (fun m d -> Seq.fold (distsAcc d) m cords) Map.empty data |> Map.toSeq |> Seq.map snd |> Seq.filter (fun i -> i < 10000) |> Seq.length

let result1 = calc 600 data
let result2 = calc 602 data
let result = Set.intersect result1 result2  |> Seq.maxBy fst
printfn "%d" (fst result)

printfn "%d" (calc2 400 data)