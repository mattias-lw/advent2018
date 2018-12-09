open System.IO
open System.Text.RegularExpressions
open System

let data = File.ReadAllLines("data.txt") 
          |> Seq.map (fun l -> Regex.Matches(l,"[A-Z]") |> Seq.map (fun m -> m.Groups.[0].Captures.[0].Value))
          |> Seq.map Seq.toArray
          |> Seq.map (fun a -> (a.[1],a.[2])) 



let createReqFor = fun m (r,d) ->
  Map.add r (match Map.tryFind r m with
                    |None -> Set.ofList [d]
                    |Some old -> Set.add d old) m

let createDependsOn = fun m (r,d) ->
  Map.add d (match Map.tryFind d m with
                    |None -> Set.ofList [r]
                    |Some old -> Set.add r old) m

let reqFor = Seq.fold createReqFor Map.empty data
let dependsOn = Seq.fold createDependsOn Map.empty data
let start =  (Seq.map fst data |> Set.ofSeq) - (Seq.map snd data |> Set.ofSeq) |> Set.toList

let fulfilled = fun visited cd-> match Map.tryFind cd dependsOn with
                                 |None -> true
                                 |Some cs -> Set.isEmpty (cs - visited)

let requiredFor = fun c -> match Map.tryFind c reqFor with
                                 | None -> []
                                 | Some s -> Set.toList s

let letter2num(c:string) = (int (c.[0])) - (int 'A') + 61

let candidates = fun visited ->
  let firstCands = visited |> Set.toSeq |> Seq.map (fun c -> Map.tryFind c reqFor) |> Seq.choose id|> Seq.fold Set.union Set.empty
  (firstCands |> Set.filter (fulfilled visited)) - visited |> Set.toList
                  
let rec digest1 avail result =
  let availSet = (Set.ofList avail) - (Set.ofList result)
  let sorted = availSet|> Set.toList |> List.filter (fulfilled (Set.ofList result)) |> List.sort
  match sorted with
  | [] -> result |> List.rev |> List.fold (+) ""
  | h::hs -> digest1 (List.append sorted  (requiredFor h)) (h::result)

let rec allocate avail wip  =
  match wip,avail with
  |[],_ -> []
  |(Some x,t)::wr,hs -> (Some x,t) :: allocate hs wr
  |xt,[] -> xt
  |(None,t)::wr,h::hs -> (Some h,(letter2num h)) :: allocate hs wr
 

let rec digest2 avail wip seconds result =
  
  
  let decreasedWip = wip |> Seq.map (fun w -> match w with
                                              |(Some x, left) -> (Some x, left - 1)
                                              | x -> x
  ) 

  let finished = decreasedWip |> Seq.filter (fun w -> match w with
                                                      |(Some x, 0) -> true
                                                      |_ -> false) |> Seq.map fst |> Seq.map Option.get |>Seq.toList

  let cleanedWip = decreasedWip |> Seq.map (fun w -> match w with
                                                        | (Some x, 0) -> (None,0)
                                                        | wo -> wo)

  let newResult = List.append finished result
  let nrIdleBefore = (Seq.length cleanedWip) - (cleanedWip |> Seq.map fst |> Seq.choose id |> Seq.length)
  let availSet = (Set.ofList (List.append avail (List.collect requiredFor finished))) - (Set.ofList result)
  let sorted = availSet|> Set.toList |> List.filter (fulfilled (Set.ofList newResult)) |> List.sort
  let nrToStart = min (Seq.length sorted) nrIdleBefore
  let newWip = allocate sorted (Seq.toList cleanedWip)   
  let nrIdleAfter = (Seq.length newWip) - (newWip |> Seq.map fst |> Seq.choose id |> Seq.length)                              
  let newAvail = List.skip nrToStart sorted
 
  match newAvail with
  | [] when nrIdleAfter = Seq.length wip -> (newResult |> List.rev |> List.fold (+) "",seconds)
  | _ -> digest2 newAvail newWip (seconds + 1) newResult



let wip = [(None,0); (None,0);(None,0); (None,0);(None,0)]

let result1 = digest1 start []
printfn "%s" result1

let result2= digest2 start wip 0 []
