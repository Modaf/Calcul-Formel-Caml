type fonction = Cos | Sin | Exp | Ln | Inverse;;

let veriter fonction = match fonction with
	|Exp -> exp
	|Cos -> cos
	|Sin -> sin
	|Ln -> log
	|Inverse -> fun x -> 1. /. x;;

let string_of_f fonction = match fonction with
	|Exp -> "exp"
	|Cos -> "cos"
	|Sin -> "sin"
	|Ln -> "ln"
	|Inverse -> "1 /";;

type expression = 
	Cst of float |
	Variable of string |
	Addition of expression*expression |
	Multiplication of expression*expression |
	Fonction of fonction |
	Operation of expression*expression;;

let string_of_expr expression = match expression with
	|(Cst n) -> "Cst"
	|(Variable x) -> "Variable"
	|(Addition (a, b)) -> "Addition"
	|(Multiplication (a, b)) -> "Multiplication"
	|(Fonction f) -> "Fonction"
	|(Operation (a, b)) -> "Operation";;

let same a b = if (string_of_expr a = string_of_expr b) then true else false;;

let se expression = string_of_expr expression;;

let convFonction expression = match expression with
	|(Fonction f) -> veriter f
	|x -> fun y -> y;;

let abs x = match x with
	|x when x < 0. -> -.x
	|x -> x;;

let rec calcul expression i = match expression with
	|(Cst n) -> n
	|(Variable x) -> i
	|(Addition (a, b)) -> (calcul a i) +. (calcul b i)
	|(Multiplication (a, b)) -> (calcul a i) *. (calcul b i)
	|(Operation (a, b)) -> (convFonction a) (calcul b i);;

let verif e1 e2 =
	let epsilon = 0.01 in (*Erreur d'arrondis*)
	let s = [|0.2; 0.6; 0.3; 1.|] in
	let n = vect_length s in
	let res = ref true in
	for i=0 to n-1 do
		if (abs ((calcul e1 s.(i)) -. (calcul e2 s.(i))) > epsilon) then res := false
	done;
	!res;;

let rec ecrit expression = match expression with
	|(Cst n) -> string_of_float n
	|(Variable x) -> x
	|(Addition (a, b)) when ecrit a="" -> (ecrit b)
	|(Addition (a, b)) when ecrit b="" -> (ecrit a)
	|(Addition (a, b)) -> "(" ^ (ecrit a) ^ "+" ^ (ecrit b) ^ ")"
	|(Multiplication (a, b)) when same a b && string_of_expr a = "Cst" -> string_of_float ((float_of_string (ecrit a)) *. (float_of_string (ecrit b)))
	|(Multiplication (a, b)) when ecrit a = "1.0" -> ecrit b
	|(Multiplication (a, b)) when ecrit a = "0.0" -> ""
	|(Multiplication (a, b)) when ecrit b = "1.0" -> ecrit a
	|(Multiplication (a, b)) when ecrit b = "0.0" -> ""
	|(Multiplication (a, b)) when ecrit a = "-1.0" -> "-"^ecrit b
	|(Multiplication (a, b)) when ecrit b = "-1.0" -> "-"^ecrit a
	|(Multiplication (a, b)) -> "(" ^ (ecrit a) ^ "*" ^ (ecrit b) ^ ")"
	|(Fonction f) -> (string_of_f f)
	|(Operation (a, b)) -> "(" ^ (ecrit a) ^ " " ^ (ecrit b) ^ ")";;

let rec der expression (Variable x) = match expression with
	|(Cst n) -> (Cst 0.)
	|(Variable y) -> if y=x then (Cst 1.) else (Cst 0.)
	|(Addition (a, b)) -> Addition (der a (Variable x), der b (Variable x))
	|(Multiplication (a, b))-> Addition (Multiplication(der a (Variable x), b), Multiplication(a, der b (Variable x)))
	|(Operation (f, a)) -> match f with 
		|(Fonction f) -> match f with
			|Exp -> Multiplication ((der a) (Variable x), Operation ((Fonction Exp), a))
			|Cos -> Multiplication (der a (Variable x), Multiplication ((Cst (- 1.)), Operation ((Fonction Sin), a)))
			|Sin -> Multiplication (der a (Variable x), Operation ((Fonction Cos), a))
			|Inverse -> Multiplication (Multiplication ((Cst (- 1.)), der a (Variable x)), Operation ((Fonction Inverse), Multiplication (a, a)))
			|Ln -> Multiplication (der a (Variable x), Operation (Fonction Inverse, a));;

let rec aux expression (Variable x) liste = match liste with
	|[] -> ecrit (Cst(0.))
	|h :: q ->
		let a expression (Variable x) elmt =
			let everybody = ref [] in
			everybody := elmt :: !everybody;
			everybody := Multiplication (elmt, Variable "x") :: !everybody;
			everybody := Addition (elmt, Multiplication((Cst (-1.)), Variable "x")) :: !everybody;
			everybody := Operation (Fonction Ln, elmt):: !everybody;
			everybody := Operation (Fonction Exp, elmt) :: !everybody;
			everybody := Operation (Fonction Cos, elmt) :: !everybody;
			everybody := Operation (Fonction Sin, elmt) :: !everybody;
			everybody := Operation (Fonction Inverse, elmt) :: !everybody;
			let rec b expression l acc= match l with
				|[] -> aux expression (Variable x) (q @ acc)
				|h :: q when verif (der h (Variable "x")) expression -> "La primitive : " ^ (ecrit h) ^ " et la primitive que l'on file a der (ie la fonction de base) : " ^ (ecrit (der h (Variable "x")));
				|h :: q -> b expression q ((der h (Variable "x"))::(h)::acc)
			in b expression !everybody [];
		in a expression (Variable x) h;;

let a = Operation (Fonction Ln, Multiplication (Variable "x", Variable "x"));;
ecrit a;;
aux a (Variable "x") [Multiplication (Variable "x", Variable "x")];;

















let a = Operation (Fonction Inverse, Variable "x");;
ecrit a;;
ecrit (der a (Variable "x"));;

let b = Operation (Fonction Inverse, Multiplication (Variable "x", Variable "x"));;
ecrit b;;
ecrit (der b (Variable "x"));;

let c = Operation (Fonction Exp, Multiplication (Variable "x", Variable "x"));;
ecrit c;;
ecrit (der c (Variable "x"));;
ecrit (der (der c (Variable "x")) (Variable "x"));;
ecrit (der (der (der c (Variable "x")) (Variable "x")) (Variable "x"));;

let d = Multiplication (Multiplication (Cst 54., Variable "x"), Variable "y");;
ecrit d;;
ecrit (der d (Variable "x"));;
ecrit (der d (Variable "y"));;
ecrit (der (der d (Variable "y")) (Variable "x"));;
(*Egal par le th de Schrawz*)
ecrit (der (der d (Variable "x")) (Variable "y"));;

let e = Operation (Fonction Exp, Variable "x");;
ecrit e;;
ecrit (der e (Variable "x"));;
ecrit (der (der e (Variable "x")) (Variable "x"));;
ecrit (der (der (der e (Variable "x")) (Variable "x")) (Variable "x"));;
ecrit (der (der (der (der e (Variable "x")) (Variable "x")) (Variable "x")) (Variable "x"));;
ecrit (der (der (der (der (der e (Variable "x")) (Variable "x")) (Variable "x")) (Variable "x")) (Variable "x"));;
ecrit (der (der (der (der (der (der e (Variable "x")) (Variable "x")) (Variable "x")) (Variable "x")) (Variable "x")) (Variable "x"));;
ecrit (der (der (der (der (der (der (der e (Variable "x")) (Variable "x")) (Variable "x")) (Variable "x")) (Variable "x")) (Variable "x")) (Variable "x"));;
ecrit (der (der (der (der (der (der (der (der e (Variable "x")) (Variable "x")) (Variable "x")) (Variable "x")) (Variable "x")) (Variable "x")) (Variable "x")) (Variable "x"));;

let f = Multiplication ((Cst 2.), Operation ((Fonction Exp), (Operation ((Fonction Cos), Multiplication (Variable "x", Variable "x")))));;
ecrit f;;
ecrit (der f (Variable "x"));;
der f (Variable "x");;

let g = Operation ((Fonction Ln), Multiplication (Variable "x", Variable "x"));;
ecrit g;;
ecrit (der g (Variable "x"));;













