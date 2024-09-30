type couleur= |Coeur 
             |Carreau
             |Trefle
             |Pique
type figure= |Roi
            |Reine
            |As
            |Valet
            |Cavalier

(****Question 1****)
type carte = couleur*figure
let zip l1 l2 =
  let rec remplis elem1 laux acc =(*cette fonction recursive prends un elements et une  (l2) ainsi qu'un accumulateur en argument, elle renvoie une liste composé de tous les couples (elem1,element_de_la_liste)::accumulateur *)
    match laux with
    | [] -> acc
    | t :: q -> remplis elem1 q (((elem1, t):carte) :: acc) in
  let rec chaquelem l1aux acc =(*cette fonction applique la fonction remplis à tous les elements de l1 (remplis t l2 acc) *)
    match l1aux with
    | [] -> acc
    | t :: q -> chaquelem q (remplis t l2 acc) in(*l'accumulateur accumule tous les couple formes d'un element de l1 et des elements de l2*)
  (chaquelem l1 [])(**)

let liste_couleurs=[Coeur;Carreau;Trefle;Pique];;
let liste_figures=[Reine;Roi;As;Valet;Cavalier];;
let cartes= zip liste_couleurs liste_figures;;

(****Question 2****)
print_int (List.length cartes) (*ca renoie 20*)



type jeu={pairs : carte list array;plateau:carte option array array};;

(****Question 3****)
let make_plateau placeholder =(*le parametre transforme ce qui aurait pu etre une variable en fonction*)
  let empty_plateau= Array.make 4 (Array.make 5 (Some ((Coeur,Reine):carte))) in (*4 rangees de 5 cartes*)
  let rec make_base_plateau i =
    if i<4 then
    let base_rangee=Array.make 5 (Some ((Coeur,Reine):carte)) in
    empty_plateau.(i) <- base_rangee;
    make_base_plateau (i+1)
    else () in
  let ()=make_base_plateau 0 in (*generation d'un tableau sans copies superficielles*)
  let rec fill_plateau i j liste_cartes =
    if i<4 then 
      if j<5 then begin
        let t::q =liste_cartes in
        empty_plateau.(i).(j)<-(Some t) ;
        fill_plateau i (j+1) q;
      end else fill_plateau (i+1) 0 liste_cartes else () in
  let ()=fill_plateau 0 0 cartes in
  empty_plateau;; 


let init_jeu ()= {pairs=[|[];[]|];plateau=(make_plateau ())};; (*initialise un jeu ou rien n'a ete joue*)
(****Question 4****)

let swap t i1 j1 i2 j2=
  let i1_j1_value=t.(i1).(j1) in
  let i2_j2_value=t.(i2).(j2) in
  t.(i1).(j1)<-i2_j2_value;
  t.(i2).(j2)<-i1_j1_value;
  ();;

(*les tests sont realises sur le top-level directement*)

(****Question 5****)
Random.self_init () ;;(*initialisation du module pour generer des nombres semi aleatoires*)
let melange tablo nbr =
  let rangees=(Array.length tablo)in (*ce n'est pas l'indice *)
  let colonnes=(Array.length tablo.(0))in
  let rec make_swap i=
  if i<nbr then begin
    swap tablo (Random.int rangees) (Random.int colonnes) (Random.int rangees) (Random.int colonnes);
    make_swap (i+1) end
  else () in make_swap 0;;
(****Question 6****)
let contient_some tablo=
let rangees=(Array.length tablo)in
let colonnes=(Array.length tablo.(0)) in
let rec check_rangee i j=
if j<colonnes then
  match tablo.(i).(j) with
  |None -> check_rangee i (j+1)
  |Some c ->true
else false in
let rec check_tablo i=
  if i<rangees then
    match (check_rangee i 0) with 
    |false ->check_tablo (i+1)
    |true ->true 
  else false in
  check_tablo 0;;

(****Question 7****)
let affiche tablo =
let rangees=(Array.length tablo)in
let colonnes=(Array.length tablo.(0)) in
let rec check_rangee i j =
  if j<colonnes then
    match tablo.(i).(j) with
    |None ->print_string "0";check_rangee i (j+1)
    |Some c ->print_string "X";check_rangee i (j+1)
  in
  let rec check_tablo i=
    if i<rangees then begin
      check_rangee i (0);
      print_newline();
      check_tablo (i+1)
    end in check_tablo 0;;

(****Question 8****)
let demande placeholder =
  let ()=print_string "[?]which line ? /starting at 0/" in
  let line=(let line_str=read_line () in int_of_string line_str ) in
  let ()=print_newline() in
  let ()=print_string "[?]which column ? /starting at 0/" in
  let column=(let column_str=read_line () in int_of_string column_str ) in
  let ()=print_newline() in
  (line,column);;

(****Question 9****)
let annonce (lacarte:carte)=
  let (lacouleur,lafigure)=lacarte in
  let () = match lacouleur with 
  |Coeur ->print_string "Coeur"
  |Carreau ->print_string "Carrau"
  |Pique -> print_string "Pique"
  |Trefle ->print_string "Trefle" in
  let () = match lafigure with
  |Roi -> print_string " Roi"
  |Reine -> print_string " Reine"
  |Valet -> print_string " Valet"
  |As -> print_string " As"
  |Cavalier -> print_string " Cavalier" in
  print_newline ();;

(****Question 10****)
let boucle_jeu placeholder = 
  let lejeu=init_jeu () in
  let ()=melange lejeu.plateau 100 in
  let rec process_round player =
    
    if (contient_some lejeu.plateau) then
    begin
    print_string "[*]player's turn :  ";
    print_int player;
    print_newline ();
    affiche lejeu.plateau;
    let coords1=demande () in
    match coords1 with
    |line,colonne when ((line>4 || line<0)||(colonne<0 || colonne>3))->begin print_string "[!] wrong coords \n"; melange lejeu.plateau 20; process_round ((player mod 2)+1)end
    |i,j when lejeu.plateau.(i).(j)=None ->begin print_string "[!] no card here \n"; melange lejeu.plateau 20; process_round ((player mod 2)+1)end
    |i,j -> let premiere_carte=Option.get (lejeu.plateau.(i).(j)) in
    let ()=annonce premiere_carte in
    let coords2=demande () in
    match coords2 with
    |line,colonne when ((line>4 || line<0)||(colonne<0 || colonne>3))->begin print_string "[!] wrong coords !\n"; melange lejeu.plateau 20; process_round ((player mod 2)+1)end
    |i,j when lejeu.plateau.(i).(j)=None ->begin print_string "[!] no card here !\n"; melange lejeu.plateau 20; process_round ((player mod 2)+1)end
    |i,j -> let deuxieme_carte=Option.get (lejeu.plateau.(i).(j)) in
    let () = annonce deuxieme_carte in
    if premiere_carte=deuxieme_carte then begin print_string "[*]cards matching!\n"; let i1,j1= coords1 in let i2,j2=coords2 in lejeu.plateau.(i1).(j1)<-None ;lejeu.plateau.(i2).(j2)<-None; lejeu.pairs.(player - 1) <- [premiere_carte; deuxieme_carte] @ lejeu.pairs.(player - 1); process_round ((player mod 2)+1)end
    else process_round ((player mod 2)+1)
    end else begin
      if (List.length lejeu.pairs.(0))>(List.length lejeu.pairs.(1)) then
        print_string "[!] player 1 win"
      else if (List.length lejeu.pairs.(0))=(List.length lejeu.pairs.(1)) then
        print_string "[!]it's a tie"
      else print_string "[!]player 2 win" end in

    process_round 1;;

let ()=boucle_jeu ();;