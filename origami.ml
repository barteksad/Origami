(* autor : Bartek Sadlej *)

type point = float * float;;

type kartka = point -> int;;

let cross_product (vec1 : float*float) (vec2 : float*float) =
    match vec1, vec2 with
    | (a1,a2), (b1,b2) ->   a1*.b1 +. a2*.b2
    | _ -> assert false

let normalize (vec : float*float) =
    let x_square = (fst vec) *. (fst vec) in
    let y_square = (snd vec) *. (snd vec) in

    let w = sqrt(x_square+. y_square) in

    (fst vec)/. w, (snd vec)/. w;;


(* 
 0 - na lini
 1 - na prawo
-1 - na lewo
 *)
let point_position  (line : float*float) (x : float*float)= 
    let v = cross_product line x in

    if abs_float v  < 5.0 *.epsilon_float
        then
            0
        else
            if v > 0.0
                then 1 
            else -1


let count_points kartka zlozenia = 
    let f kartka (p : point) =
    1
    in
    f kartka
    

let prostokat (p1 : point) (p2: point) = 
    count_points (p1,p2) [];;

let kolko p1 r =
    count_points (p1,r) [];; 

let pom f a b c=
    (f c) b


let complex_multiply (a1,b1 : float*float) (a2,b2:float*float) = 
    let a3 = a1 *. a2 -. b1 *. b2 in
    let b3 = a1 *. b2 +. a2 *. b1 in
    (a3,b3);;

let complex_mod ( a1,b1 : float*float) = 
    (a1,-.b1);;

let complex_divide (a1,b1 : float*float) (a2,b2:float*float) = 
    let (a3,b3) = complex_multiply (a1,b1) (complex_mod (a2,b2)) in
    let w = a2*. a2 +. b2 *. b2 in
    (a3/.w,b3/.w);;


let odbij (p1 : point)  (p2 : point) (p3 : point) = 
    let line = normalize (fst p2 -. fst p1, snd p2 -. snd p1) in
    let x = normalize (fst p3 -. fst p1,snd p3 -. snd p3) in

    let p3_info = point_position line x in

    if p3_info > 0 
        then p3, p3, false

    else if p3_info = 0
        then p3,p3,true

    else
        complex_divide (complex_multiply (complex_mod x) line) (complex_mod line)  ,p3 ,true;;
        


let zloz (p1 : point)  (p2 : point) (k : kartka) = 
    function x -> 
        let nowy1,nowy2,zla_strona = odbij p1 p2 x in
        if zla_strona = false
            then 0
        else if nowy1 = nowy2
            then
                k nowy1
        else k nowy1 + k nowy2;;

let skladaj (l : (point*point) list) (k : kartka) = 
    let pom ((p1 : point),(p2 :point)) (k1: kartka) =
        zloz p1 p2 k1
    in
    List.fold_right pom l k;; 

    