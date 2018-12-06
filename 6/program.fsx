open System.IO
open System
open System.Text.RegularExpressions

let data = File.ReadAllLines("data.txt") 
          |> Seq.map (fun l -> Regex.Matches(l,"\\d+") |> Seq.map (fun m -> m.Groups.[0].Captures.[0].Value))
          |> Seq.map Seq.toArray
          |> Seq.map (fun a -> (int a.[0],int a.[1])) 


let distance = fun a b ->
  match a,b with
  | (x1,y1),(x2,y2) -> (abs (x1 - x2)) + (abs (y1 - y2))

//let printfield = fun dim x_cords y_cords field ->  
//  for x in x_cords do
//    for y in y_cords do
//      match field.[x+dim + 2 * dim * (y+dim)] with
//      | (dist,(ox,oy)) ->  printf "(%2d, (%2d,%2d) " dist ox oy
//    printfn ""
 
let calc = fun dim ->

  let x_cords = seq {-dim .. dim-1}
  let y_cords = seq {-dim .. dim-1} 

  let field = Array.create (2*dim*2*dim) (2147483647,(0,0))

  let mark = fun c ->
    for x in x_cords do
      for y in y_cords do
        let old = field.[x+dim + 2 * dim * (y+dim)]
        Array.set field (x+dim + 2 * dim * (y+dim)) (match old,c,(distance c (x,y)) with
                                                     |(odist,(ox,oy)),c,dist when dist < odist -> (dist,c)
                                                     |(odist,(ox,oy)),c,dist when dist = odist -> (dist,(2147483647,2147483647))
                                                     |o,_,_ -> o
        )

  data |> Seq.iter mark

  field |> Seq.groupBy snd |> Seq.map (fun e -> (Seq.length (snd e), (fst e)))

let result1 = calc 601
let result2 = calc 500
let result = Seq.filter (fun e -> Seq.contains e result1) result2 |> Seq.maxBy fst

printfn "%d %d %d" (fst result) (fst (snd result)) (snd (snd result))