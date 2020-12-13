(* autor : Bartek Sadlej *)

type point = float * float;;

type kartka = point -> int;;

(* tak jakby iloczyn skalarny *)
(* 3 współżędna z iloczynu wektorowego *)
(*  znak wyrażenia określa położenie drugieo wektora względem pierwszego *)
(* 0 - współliniowe *)
(* <0 - drugi wektor na prawo od pierwszego *)
(* >0 - drugi wektor na lewo od pierwszego *)
let cross_product (vec1 : float*float) (vec2 : float*float) =
    match vec1, vec2 with
    | (a1,a2), (b1,b2) ->   a1*.b2 -. a2*.b1
    | _ -> assert false

(* funkcja normalizująca długośc wektora tak żeby jego długość = 1 *)
let normalize (vec : float*float) =
    let x_square = (fst vec) *. (fst vec) in
    let y_square = (snd vec) *. (snd vec) in

    let w = sqrt(x_square+. y_square) in

    (fst vec)/. w, (snd vec)/. w ;;


(* 
Pozycja punktu względem prostej przechodzącej przez dwa punkty
niedokładność obliczeń uwzględnia 0 = 0 +- 15* 2.22044604925031308e-16
 0 - na lini
-1 - na prawo
 1 - na lewo
 *)
let point_position  (line : float*float) (x : float*float)= 
    let v = cross_product line x in

    if (abs_float v)  < 15.0 *.epsilon_float
        then
            0
        else
            if v < 0.0
                then -1 
            else 1;;

(* funkcja tworzy prostokąt który jest funkcją oczekującą na punkt i zwracającą info 0 - poza prostokątem 1 - w nim lub na granicy *)
let prostokat (p1 : point) (p2: point) = 
    let prostokat_sprawdz (k : (float*float)*(float*float)) (p : point) =
        let (x,y) = p in
        let (a1,b1),(a2,b2) = k in
        if x>= a1 && x<= a2 && y>= b1 && y <= b2;
            then
                1
            else
                0
    in
    prostokat_sprawdz (p1,p2);;

(* funkcja tworzy okrąg który jest funkcją oczekującą na punkt i zwracającą info 0 - poza okręgiem 1 - w nim lub na granicy *)
let kolko (p1 : point) (r : float) =
    let kolko_sprawdz ( k :(float*float)*float) (p: point)=
        let (x,y) = p in
        let (a1,b1),r = k in  
        if (a1 -. x) *. (a1 -. x) +. (b1 -. y) *. (b1 -. y) <= r*.r
            then
                1
            else
                0

    in
    kolko_sprawdz (p1,r)

(* --- aytmetyka liczb zespolonych --- *)

(* mnożenie *)
let complex_multiply (a1,b1 : float*float) (a2,b2:float*float) = 
    let a3 = a1 *. a2 -. b1 *. b2 in
    let b3 = a1 *. b2 +. a2 *. b1 in
    (a3,b3);;

(* sprzężenie *)
let complex_conj ( a1,b1 : float*float) = 
    (a1,b1 *. (-1.0));;

(* moduł *)
let complex_mod ( a1,b1 : float*float) = 
    sqrt (a1 *. a1 +. b1 *. b1 );;

(* dzielenie *)
let complex_divide (a1,b1 : float*float) (a2,b2:float*float) = 
    let (a3,b3) = complex_multiply (a1,b1) (complex_conj (a2,b2)) in
    let w = a2*. a2 +. b2 *. b2 in
    (a3/.w,b3/.w);;

(* dodawanie *)
let add (a1,b1 : float*float) (a2,b2:float*float) = 
    a1 +. a2,b1 +. b2;;
(* --- --- --- *)

(* funkcja pomocnicza do złóż *)
(* odbija punkt względem prostej *)
(* jeśli punkt był po lewej zwraca odbity punk, oryginalny i false *)
(* jeśli punkt był na lini zwraca ten sam, ten sam i false*)
(* jeśli punkt był po prawo od prostej to (niema-znaczenia-co) i  true *)
let odbij (p1 : point)  (p2 : point) (p3 : point) = 
    let line = fst p2 -. fst p1, snd p2 -. snd p1 in
    let x=fst p3 -. fst p1,snd p3 -. snd p1 in

    let p3_info = point_position (normalize line)  (normalize x) in

    if p3_info < 0 
        then p3, p3, true

    else if p3_info = 0
        then p3,p3,false

    else
        add (complex_divide (complex_multiply (complex_conj x) line )(complex_conj line)) p1  ,p3 ,false;;
        

(* funkcja złóż *)
(* zwraca funkcje która oczekuje punktu i w zależnośc gdzie względem prostej ten punk się znajduje to *)
(* - jeśli po prawo to zwraca 0 *)
(* - jeśli na lini to wywołuje kartke od tego puktu *)
(* - jeśli na lewo to wywołuje karte od tego punktu + kartkę od jego odbicia *)
let zloz (p1 : point)  (p2 : point) (k : kartka) = 
    function x -> 
        let nowy1,nowy2,zla_strona = odbij p1 p2 x in
        if zla_strona = true
            then 0
        else if nowy1 = nowy2
            then
                k nowy1
        else k nowy1 + k nowy2;;

(* składaj apllikuje zloz po elementach listy l fold leftem do kartki  *)
let skladaj (l : (point*point) list) (k : kartka) = 
    let wrap_zloz (k1: kartka) ((p1 : point),(p2 :point)) =
        zloz p1 p2 k1
    in
    List.fold_left wrap_zloz k l;; 

    