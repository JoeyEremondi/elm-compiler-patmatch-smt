;(set-logic UF )

(set-option :smt.mbqi true )

(push )

(declare-fun literal_0 () Bool )

(declare-fun literal_1 () Bool )

(declare-fun literal_2 () Bool )

(declare-fun literal_3 () Bool )

(declare-fun literal_4 () Bool )

(declare-fun literal_5 () Bool )

(declare-fun literal_6 () Bool )

(declare-fun literal_7 () Bool )

(declare-fun literal_8 () Bool )

(declare-fun literal_9 () Bool )

(declare-fun literal_10 () Bool )

(declare-fun literal_11 () Bool )

(declare-fun literal_12 () Bool )

(assert (and literal_1 (=> literal_5 (and literal_2 literal_9 ) ) (=> (not literal_5 ) literal_0 ) (and literal_8 literal_6 ) (= (and literal_3 literal_10 ) (and literal_7 literal_12 ) ) (= (and literal_4 literal_11 ) (and literal_7 literal_12 ) ) ) )

;Done asserting formula, 13 literals total
;Partitioned into 1 subproblems: [[Literal {unLiteral = (Bottom,Intersect (Var "SetVar_18") (FunApp "--Cons" [Top,Top]))},Literal {unLiteral = (FunApp "--Cons" [Var "projVar_21_1",Var "projVar_21_2"],Intersect (Var "SetVar_18") (FunApp "--Cons" [Top,Top]))},Literal {unLiteral = (Bottom,Var "projVar_21_2")},Literal {unLiteral = (Intersect (Var "SetVar_18") (FunApp "--Cons" [Top,Top]),FunApp "--Cons" [Var "projVar_21_1",Var "projVar_21_2"])},Literal {unLiteral = (Bottom,Var "projVar_21_1")},Literal {unLiteral = (Var "projVar_21_1",Bottom)},Literal {unLiteral = (Var "projVar_21_2",Bottom)},Literal {unLiteral = (Intersect (Var "SetVar_18") (FunApp "--Cons" [Top,Top]),Bottom)},Literal {unLiteral = (Intersect (Var "SetVar_18") (FunApp "--Cons" [Top,FunApp "--Null" []]),Bottom)},Literal {unLiteral = (Var "SetVar_18",FunApp "--Cons" [Top,FunApp "--Null" []])},Literal {unLiteral = (Var "SetVar_18",Var "SetVar_20")},Literal {unLiteral = (Bottom,Var "SetVar_20")},Literal {unLiteral = (Var "SetVar_20",Bottom)}]]
;Done asserting subset properties
;Lit Vars: [(Literal {unLiteral = (Var "SetVar_18",Var "SetVar_20")},Atom "literal_0"),(Literal {unLiteral = (Var "SetVar_18",FunApp "--Cons" [Top,FunApp "--Null" []])},Atom "literal_1"),(Literal {unLiteral = (Var "SetVar_20",Bottom)},Atom "literal_2"),(Literal {unLiteral = (Var "projVar_21_1",Bottom)},Atom "literal_3"),(Literal {unLiteral = (Var "projVar_21_2",Bottom)},Atom "literal_4"),(Literal {unLiteral = (Intersect (Var "SetVar_18") (FunApp "--Cons" [Top,FunApp "--Null" []]),Bottom)},Atom "literal_5"),(Literal {unLiteral = (Intersect (Var "SetVar_18") (FunApp "--Cons" [Top,Top]),FunApp "--Cons" [Var "projVar_21_1",Var "projVar_21_2"])},Atom "literal_6"),(Literal {unLiteral = (Intersect (Var "SetVar_18") (FunApp "--Cons" [Top,Top]),Bottom)},Atom "literal_7"),(Literal {unLiteral = (FunApp "--Cons" [Var "projVar_21_1",Var "projVar_21_2"],Intersect (Var "SetVar_18") (FunApp "--Cons" [Top,Top]))},Atom "literal_8"),(Literal {unLiteral = (Bottom,Var "SetVar_20")},Atom "literal_9"),(Literal {unLiteral = (Bottom,Var "projVar_21_1")},Atom "literal_10"),(Literal {unLiteral = (Bottom,Var "projVar_21_2")},Atom "literal_11"),(Literal {unLiteral = (Bottom,Intersect (Var "SetVar_18") (FunApp "--Cons" [Top,Top]))},Atom "literal_12")]
;Pred numbers: fromList [(PVar "SetVar_18",0),(PVar "SetVar_20",1),(PVar "projVar_21_1",4),(PVar "projVar_21_2",5),(PFunApp "--Cons" [Var "projVar_21_1",Var "projVar_21_2"],7),(PFunApp "--Cons" [Top,FunApp "--Null" []],3),(PFunApp "--Cons" [Top,Top],6),(PFunApp "--Null" [],2)]
;In theory solver, numBits: 8
;Declaring domain
;(declare-fun domainToBeSolved (Bool Bool Bool Bool Bool Bool Bool Bool ) Bool )

(define-fun domainToBeSolved 
    ((z_boolDomain-0 Bool ) (z_boolDomain-1 Bool ) (z_boolDomain-2 Bool ) (z_boolDomain-3 Bool ) (z_boolDomain-4 Bool ) (z_boolDomain-5 Bool ) (z_boolDomain-6 Bool ) (z_boolDomain-7 Bool ) ) Bool 
    (or
        ;;Cons Nil Nil
        (and (= z_boolDomain-0 true) (= z_boolDomain-1 true) (= z_boolDomain-2 false) (= z_boolDomain-3 true) (= z_boolDomain-4 false) (= z_boolDomain-5 false) (= z_boolDomain-6 true) (= z_boolDomain-7 true))
        (and (= z_boolDomain-0 false) (= z_boolDomain-1 false) (= z_boolDomain-2 false) (= z_boolDomain-3 true) (= z_boolDomain-4 false) (= z_boolDomain-5 false) (= z_boolDomain-6 true) (= z_boolDomain-7 false))
        (and (= z_boolDomain-0 false) (= z_boolDomain-1 false) (= z_boolDomain-2 false) (= z_boolDomain-3 false) (= z_boolDomain-4 false) (= z_boolDomain-5 false) (= z_boolDomain-6 true) (= z_boolDomain-7 false))
        ;;False
        (and (= z_boolDomain-0 false) (= z_boolDomain-1 false) (= z_boolDomain-2 true) (= z_boolDomain-3 false) (= z_boolDomain-4 true) (= z_boolDomain-5 true) (= z_boolDomain-6 false) (= z_boolDomain-7 false))
    ) )


(define-fun domain 
    ((z_boolDomain-0 Bool ) (z_boolDomain-1 Bool ) (z_boolDomain-2 Bool ) (z_boolDomain-3 Bool ) (z_boolDomain-4 Bool ) (z_boolDomain-5 Bool ) (z_boolDomain-6 Bool ) (z_boolDomain-7 Bool ) ) Bool 
    (and (and 
        (=> literal_0 (=> z_boolDomain-0 z_boolDomain-1 ) ) 
        (=> literal_1 (=> z_boolDomain-0 z_boolDomain-3 ) ) 
        (=> literal_2 (=> z_boolDomain-1 false ) ) 
        (=> literal_3 (=> z_boolDomain-4 false ) ) 
        (=> literal_4 (=> z_boolDomain-5 false ) ) 
        (=> literal_5 (=> (and z_boolDomain-0 z_boolDomain-3 ) false ) ) 
        (=> literal_6 (=> (and z_boolDomain-0 z_boolDomain-6 ) z_boolDomain-7 ) ) 
        (=> literal_7 (=> (and z_boolDomain-0 z_boolDomain-6 ) false ) ) 
        (=> literal_8 (=> z_boolDomain-7 (and z_boolDomain-0 z_boolDomain-6 ) ) ) 
        (=> literal_9 (=> false z_boolDomain-1 ) ) 
        (=> literal_10 (=> false z_boolDomain-4 ) ) 
        (=> literal_11 (=> false z_boolDomain-5 ) ) 
        (=> literal_12 (=> false (and z_boolDomain-0 z_boolDomain-6 ) ) ) ) 
        (domainToBeSolved z_boolDomain-0 z_boolDomain-1 z_boolDomain-2 z_boolDomain-3 z_boolDomain-4 z_boolDomain-5 z_boolDomain-6 z_boolDomain-7 ) ) )

(declare-fun test0 () Bool)
(declare-fun test1 () Bool)
(declare-fun test2 () Bool)
(declare-fun test3 () Bool)
(declare-fun test4 () Bool)
(declare-fun test5 () Bool)
(declare-fun test6 () Bool)
(declare-fun test7 () Bool)


;Declaring constructors
(declare-fun --Cons-0 (Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool ) Bool )

(declare-fun --Cons-1 (Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool ) Bool )

(declare-fun --Cons-4 (Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool ) Bool )

(declare-fun --Cons-5 (Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool ) Bool )

(define-fun --Cons-2 ((f-arg-0-0 Bool ) (f-arg-0-1 Bool ) (f-arg-0-2 Bool ) (f-arg-0-3 Bool ) (f-arg-0-4 Bool ) (f-arg-0-5 Bool ) (f-arg-0-6 Bool ) (f-arg-0-7 Bool ) (f-arg-1-0 Bool ) (f-arg-1-1 Bool ) (f-arg-1-2 Bool ) (f-arg-1-3 Bool ) (f-arg-1-4 Bool ) (f-arg-1-5 Bool ) (f-arg-1-6 Bool ) (f-arg-1-7 Bool ) ) Bool false )

(define-fun --Cons-3 ((f-arg-0-0 Bool ) (f-arg-0-1 Bool ) (f-arg-0-2 Bool ) (f-arg-0-3 Bool ) (f-arg-0-4 Bool ) (f-arg-0-5 Bool ) (f-arg-0-6 Bool ) (f-arg-0-7 Bool ) (f-arg-1-0 Bool ) (f-arg-1-1 Bool ) (f-arg-1-2 Bool ) (f-arg-1-3 Bool ) (f-arg-1-4 Bool ) (f-arg-1-5 Bool ) (f-arg-1-6 Bool ) (f-arg-1-7 Bool ) ) Bool (and true f-arg-1-2 ) )

(define-fun --Cons-6 ((f-arg-0-0 Bool ) (f-arg-0-1 Bool ) (f-arg-0-2 Bool ) (f-arg-0-3 Bool ) (f-arg-0-4 Bool ) (f-arg-0-5 Bool ) (f-arg-0-6 Bool ) (f-arg-0-7 Bool ) (f-arg-1-0 Bool ) (f-arg-1-1 Bool ) (f-arg-1-2 Bool ) (f-arg-1-3 Bool ) (f-arg-1-4 Bool ) (f-arg-1-5 Bool ) (f-arg-1-6 Bool ) (f-arg-1-7 Bool ) ) Bool (and true true ) )

(define-fun --Cons-7 ((f-arg-0-0 Bool ) (f-arg-0-1 Bool ) (f-arg-0-2 Bool ) (f-arg-0-3 Bool ) (f-arg-0-4 Bool ) (f-arg-0-5 Bool ) (f-arg-0-6 Bool ) (f-arg-0-7 Bool ) (f-arg-1-0 Bool ) (f-arg-1-1 Bool ) (f-arg-1-2 Bool ) (f-arg-1-3 Bool ) (f-arg-1-4 Bool ) (f-arg-1-5 Bool ) (f-arg-1-6 Bool ) (f-arg-1-7 Bool ) ) Bool (and f-arg-0-4 f-arg-1-5 ) )

(declare-fun --Null-0 () Bool )

(declare-fun --Null-1 () Bool )

(declare-fun --Null-4 () Bool )

(declare-fun --Null-5 () Bool )

(define-fun --Null-2 () Bool true )

(define-fun --Null-3 () Bool false )

(define-fun --Null-6 () Bool false )

(define-fun --Null-7 () Bool false )

;Declaring existentials
(declare-fun x_exists_12-0 () Bool )

(declare-fun x_exists_12-1 () Bool )

(declare-fun x_exists_12-2 () Bool )

(declare-fun x_exists_12-3 () Bool )

(declare-fun x_exists_12-4 () Bool )

(declare-fun x_exists_12-5 () Bool )

(declare-fun x_exists_12-6 () Bool )

(declare-fun x_exists_12-7 () Bool )

(assert (domain x_exists_12-0 x_exists_12-1 x_exists_12-2 x_exists_12-3 x_exists_12-4 x_exists_12-5 x_exists_12-6 x_exists_12-7 ) )

(declare-fun x_exists_11-0 () Bool )

(declare-fun x_exists_11-1 () Bool )

(declare-fun x_exists_11-2 () Bool )

(declare-fun x_exists_11-3 () Bool )

(declare-fun x_exists_11-4 () Bool )

(declare-fun x_exists_11-5 () Bool )

(declare-fun x_exists_11-6 () Bool )

(declare-fun x_exists_11-7 () Bool )

(assert (domain x_exists_11-0 x_exists_11-1 x_exists_11-2 x_exists_11-3 x_exists_11-4 x_exists_11-5 x_exists_11-6 x_exists_11-7 ) )

(declare-fun x_exists_10-0 () Bool )

(declare-fun x_exists_10-1 () Bool )

(declare-fun x_exists_10-2 () Bool )

(declare-fun x_exists_10-3 () Bool )

(declare-fun x_exists_10-4 () Bool )

(declare-fun x_exists_10-5 () Bool )

(declare-fun x_exists_10-6 () Bool )

(declare-fun x_exists_10-7 () Bool )

(assert (domain x_exists_10-0 x_exists_10-1 x_exists_10-2 x_exists_10-3 x_exists_10-4 x_exists_10-5 x_exists_10-6 x_exists_10-7 ) )

(declare-fun x_exists_9-0 () Bool )

(declare-fun x_exists_9-1 () Bool )

(declare-fun x_exists_9-2 () Bool )

(declare-fun x_exists_9-3 () Bool )

(declare-fun x_exists_9-4 () Bool )

(declare-fun x_exists_9-5 () Bool )

(declare-fun x_exists_9-6 () Bool )

(declare-fun x_exists_9-7 () Bool )

(assert (domain x_exists_9-0 x_exists_9-1 x_exists_9-2 x_exists_9-3 x_exists_9-4 x_exists_9-5 x_exists_9-6 x_exists_9-7 ) )

(declare-fun x_exists_8-0 () Bool )

(declare-fun x_exists_8-1 () Bool )

(declare-fun x_exists_8-2 () Bool )

(declare-fun x_exists_8-3 () Bool )

(declare-fun x_exists_8-4 () Bool )

(declare-fun x_exists_8-5 () Bool )

(declare-fun x_exists_8-6 () Bool )

(declare-fun x_exists_8-7 () Bool )

(assert (domain x_exists_8-0 x_exists_8-1 x_exists_8-2 x_exists_8-3 x_exists_8-4 x_exists_8-5 x_exists_8-6 x_exists_8-7 ) )

(declare-fun x_exists_7-0 () Bool )

(declare-fun x_exists_7-1 () Bool )

(declare-fun x_exists_7-2 () Bool )

(declare-fun x_exists_7-3 () Bool )

(declare-fun x_exists_7-4 () Bool )

(declare-fun x_exists_7-5 () Bool )

(declare-fun x_exists_7-6 () Bool )

(declare-fun x_exists_7-7 () Bool )

(assert (domain x_exists_7-0 x_exists_7-1 x_exists_7-2 x_exists_7-3 x_exists_7-4 x_exists_7-5 x_exists_7-6 x_exists_7-7 ) )

(declare-fun x_exists_6-0 () Bool )

(declare-fun x_exists_6-1 () Bool )

(declare-fun x_exists_6-2 () Bool )

(declare-fun x_exists_6-3 () Bool )

(declare-fun x_exists_6-4 () Bool )

(declare-fun x_exists_6-5 () Bool )

(declare-fun x_exists_6-6 () Bool )

(declare-fun x_exists_6-7 () Bool )

(assert (domain x_exists_6-0 x_exists_6-1 x_exists_6-2 x_exists_6-3 x_exists_6-4 x_exists_6-5 x_exists_6-6 x_exists_6-7 ) )

(declare-fun x_exists_5-0 () Bool )

(declare-fun x_exists_5-1 () Bool )

(declare-fun x_exists_5-2 () Bool )

(declare-fun x_exists_5-3 () Bool )

(declare-fun x_exists_5-4 () Bool )

(declare-fun x_exists_5-5 () Bool )

(declare-fun x_exists_5-6 () Bool )

(declare-fun x_exists_5-7 () Bool )

(assert (domain x_exists_5-0 x_exists_5-1 x_exists_5-2 x_exists_5-3 x_exists_5-4 x_exists_5-5 x_exists_5-6 x_exists_5-7 ) )

(declare-fun x_exists_4-0 () Bool )

(declare-fun x_exists_4-1 () Bool )

(declare-fun x_exists_4-2 () Bool )

(declare-fun x_exists_4-3 () Bool )

(declare-fun x_exists_4-4 () Bool )

(declare-fun x_exists_4-5 () Bool )

(declare-fun x_exists_4-6 () Bool )

(declare-fun x_exists_4-7 () Bool )

(assert (domain x_exists_4-0 x_exists_4-1 x_exists_4-2 x_exists_4-3 x_exists_4-4 x_exists_4-5 x_exists_4-6 x_exists_4-7 ) )

(declare-fun x_exists_3-0 () Bool )

(declare-fun x_exists_3-1 () Bool )

(declare-fun x_exists_3-2 () Bool )

(declare-fun x_exists_3-3 () Bool )

(declare-fun x_exists_3-4 () Bool )

(declare-fun x_exists_3-5 () Bool )

(declare-fun x_exists_3-6 () Bool )

(declare-fun x_exists_3-7 () Bool )

(assert (domain x_exists_3-0 x_exists_3-1 x_exists_3-2 x_exists_3-3 x_exists_3-4 x_exists_3-5 x_exists_3-6 x_exists_3-7 ) )

(declare-fun x_exists_2-0 () Bool )

(declare-fun x_exists_2-1 () Bool )

(declare-fun x_exists_2-2 () Bool )

(declare-fun x_exists_2-3 () Bool )

(declare-fun x_exists_2-4 () Bool )

(declare-fun x_exists_2-5 () Bool )

(declare-fun x_exists_2-6 () Bool )

(declare-fun x_exists_2-7 () Bool )

(assert (domain x_exists_2-0 x_exists_2-1 x_exists_2-2 x_exists_2-3 x_exists_2-4 x_exists_2-5 x_exists_2-6 x_exists_2-7 ) )

(declare-fun x_exists_1-0 () Bool )

(declare-fun x_exists_1-1 () Bool )

(declare-fun x_exists_1-2 () Bool )

(declare-fun x_exists_1-3 () Bool )

(declare-fun x_exists_1-4 () Bool )

(declare-fun x_exists_1-5 () Bool )

(declare-fun x_exists_1-6 () Bool )

(declare-fun x_exists_1-7 () Bool )

(assert (domain x_exists_1-0 x_exists_1-1 x_exists_1-2 x_exists_1-3 x_exists_1-4 x_exists_1-5 x_exists_1-6 x_exists_1-7 ) )

(declare-fun x_exists_0-0 () Bool )

(declare-fun x_exists_0-1 () Bool )

(declare-fun x_exists_0-2 () Bool )

(declare-fun x_exists_0-3 () Bool )

(declare-fun x_exists_0-4 () Bool )

(declare-fun x_exists_0-5 () Bool )

(declare-fun x_exists_0-6 () Bool )

(declare-fun x_exists_0-7 () Bool )

(assert (domain x_exists_0-0 x_exists_0-1 x_exists_0-2 x_exists_0-3 x_exists_0-4 x_exists_0-5 x_exists_0-6 x_exists_0-7 ) )

;Assert existential properties
(assert (=> (not literal_0 ) (and (and (domain x_exists_0-0 x_exists_0-1 x_exists_0-2 x_exists_0-3 x_exists_0-4 x_exists_0-5 x_exists_0-6 x_exists_0-7 ) x_exists_0-0 ) (not x_exists_0-1 ) ) ) )

(assert (=> (not literal_1 ) (and (and (domain x_exists_1-0 x_exists_1-1 x_exists_1-2 x_exists_1-3 x_exists_1-4 x_exists_1-5 x_exists_1-6 x_exists_1-7 ) x_exists_1-0 ) (not x_exists_1-3 ) ) ) )

(assert (=> (not literal_2 ) (and (and (domain x_exists_2-0 x_exists_2-1 x_exists_2-2 x_exists_2-3 x_exists_2-4 x_exists_2-5 x_exists_2-6 x_exists_2-7 ) x_exists_2-1 ) (not false ) ) ) )

(assert (=> (not literal_3 ) (and (and (domain x_exists_3-0 x_exists_3-1 x_exists_3-2 x_exists_3-3 x_exists_3-4 x_exists_3-5 x_exists_3-6 x_exists_3-7 ) x_exists_3-4 ) (not false ) ) ) )

(assert (=> (not literal_4 ) (and (and (domain x_exists_4-0 x_exists_4-1 x_exists_4-2 x_exists_4-3 x_exists_4-4 x_exists_4-5 x_exists_4-6 x_exists_4-7 ) x_exists_4-5 ) (not false ) ) ) )

(assert (=> (not literal_5 ) (and (and (domain x_exists_5-0 x_exists_5-1 x_exists_5-2 x_exists_5-3 x_exists_5-4 x_exists_5-5 x_exists_5-6 x_exists_5-7 ) (and x_exists_5-0 x_exists_5-3 ) ) (not false ) ) ) )

(assert (=> (not literal_6 ) (and (and (domain x_exists_6-0 x_exists_6-1 x_exists_6-2 x_exists_6-3 x_exists_6-4 x_exists_6-5 x_exists_6-6 x_exists_6-7 ) (and x_exists_6-0 x_exists_6-6 ) ) (not x_exists_6-7 ) ) ) )

(assert (=> (not literal_7 ) (and (and (domain x_exists_7-0 x_exists_7-1 x_exists_7-2 x_exists_7-3 x_exists_7-4 x_exists_7-5 x_exists_7-6 x_exists_7-7 ) (and x_exists_7-0 x_exists_7-6 ) ) (not false ) ) ) )

(assert (=> (not literal_8 ) (and (and (domain x_exists_8-0 x_exists_8-1 x_exists_8-2 x_exists_8-3 x_exists_8-4 x_exists_8-5 x_exists_8-6 x_exists_8-7 ) x_exists_8-7 ) (not (and x_exists_8-0 x_exists_8-6 ) ) ) ) )

(assert (=> (not literal_9 ) (and (and (domain x_exists_9-0 x_exists_9-1 x_exists_9-2 x_exists_9-3 x_exists_9-4 x_exists_9-5 x_exists_9-6 x_exists_9-7 ) false ) (not x_exists_9-1 ) ) ) )

(assert (=> (not literal_10 ) (and (and (domain x_exists_10-0 x_exists_10-1 x_exists_10-2 x_exists_10-3 x_exists_10-4 x_exists_10-5 x_exists_10-6 x_exists_10-7 ) false ) (not x_exists_10-4 ) ) ) )

(assert (=> (not literal_11 ) (and (and (domain x_exists_11-0 x_exists_11-1 x_exists_11-2 x_exists_11-3 x_exists_11-4 x_exists_11-5 x_exists_11-6 x_exists_11-7 ) false ) (not x_exists_11-5 ) ) ) )

(assert (=> (not literal_12 ) (and (and (domain x_exists_12-0 x_exists_12-1 x_exists_12-2 x_exists_12-3 x_exists_12-4 x_exists_12-5 x_exists_12-6 x_exists_12-7 ) false ) (not (and x_exists_12-0 x_exists_12-6 ) ) ) ) )

(check-sat)

;;Functions closed on domain
(assert 
    (forall ((y_univ_1-0 Bool ) (y_univ_1-1 Bool ) (y_univ_1-2 Bool ) (y_univ_1-3 Bool ) (y_univ_1-4 Bool ) (y_univ_1-5 Bool ) (y_univ_1-6 Bool ) (y_univ_1-7 Bool ) (y_univ_2-0 Bool ) (y_univ_2-1 Bool ) (y_univ_2-2 Bool ) (y_univ_2-3 Bool ) (y_univ_2-4 Bool ) (y_univ_2-5 Bool ) (y_univ_2-6 Bool ) (y_univ_2-7 Bool ) (y_univ_3-0 Bool ) (y_univ_3-1 Bool ) (y_univ_3-2 Bool ) (y_univ_3-3 Bool ) (y_univ_3-4 Bool ) (y_univ_3-5 Bool ) (y_univ_3-6 Bool ) (y_univ_3-7 Bool ) (y_univ_4-0 Bool ) (y_univ_4-1 Bool ) (y_univ_4-2 Bool ) (y_univ_4-3 Bool ) (y_univ_4-4 Bool ) (y_univ_4-5 Bool ) (y_univ_4-6 Bool ) (y_univ_4-7 Bool ) )
    
    (and 
        (=> (and (domain y_univ_1-0 y_univ_1-1 y_univ_1-2 y_univ_1-3 y_univ_1-4 y_univ_1-5 y_univ_1-6 y_univ_1-7 ) (domain y_univ_2-0 y_univ_2-1 y_univ_2-2 y_univ_2-3 y_univ_2-4 y_univ_2-5 y_univ_2-6 y_univ_2-7 ) (domain y_univ_3-0 y_univ_3-1 y_univ_3-2 y_univ_3-3 y_univ_3-4 y_univ_3-5 y_univ_3-6 y_univ_3-7 ) (domain y_univ_4-0 y_univ_4-1 y_univ_4-2 y_univ_4-3 y_univ_4-4 y_univ_4-5 y_univ_4-6 y_univ_4-7 ) ) (and true true true (and (= true --Null-2 ) (= (--Cons-2 y_univ_1-0 y_univ_1-1 y_univ_1-2 y_univ_1-3 y_univ_1-4 y_univ_1-5 y_univ_1-6 y_univ_1-7 y_univ_2-0 y_univ_2-1 y_univ_2-2 y_univ_2-3 y_univ_2-4 y_univ_2-5 y_univ_2-6 y_univ_2-7 ) false ) ) (and (= (and true y_univ_2-2 ) (--Cons-3 y_univ_1-0 y_univ_1-1 y_univ_1-2 y_univ_1-3 y_univ_1-4 y_univ_1-5 y_univ_1-6 y_univ_1-7 y_univ_2-0 y_univ_2-1 y_univ_2-2 y_univ_2-3 y_univ_2-4 y_univ_2-5 y_univ_2-6 y_univ_2-7 ) ) (= --Null-3 false ) ) true true true true (and (= (and true true ) (--Cons-6 y_univ_1-0 y_univ_1-1 y_univ_1-2 y_univ_1-3 y_univ_1-4 y_univ_1-5 y_univ_1-6 y_univ_1-7 y_univ_2-0 y_univ_2-1 y_univ_2-2 y_univ_2-3 y_univ_2-4 y_univ_2-5 y_univ_2-6 y_univ_2-7 ) ) (= --Null-6 false ) ) true (and (= (and y_univ_1-4 y_univ_2-5 ) (--Cons-7 y_univ_1-0 y_univ_1-1 y_univ_1-2 y_univ_1-3 y_univ_1-4 y_univ_1-5 y_univ_1-6 y_univ_1-7 y_univ_2-0 y_univ_2-1 y_univ_2-2 y_univ_2-3 y_univ_2-4 y_univ_2-5 y_univ_2-6 y_univ_2-7 ) ) (= --Null-7 false ) ) ) ) 
        (and (domain (--Cons-0 y_univ_1-0 y_univ_1-1 y_univ_1-2 y_univ_1-3 y_univ_1-4 y_univ_1-5 y_univ_1-6 y_univ_1-7 y_univ_2-0 y_univ_2-1 y_univ_2-2 y_univ_2-3 y_univ_2-4 y_univ_2-5 y_univ_2-6 y_univ_2-7 ) (--Cons-1 y_univ_1-0 y_univ_1-1 y_univ_1-2 y_univ_1-3 y_univ_1-4 y_univ_1-5 y_univ_1-6 y_univ_1-7 y_univ_2-0 y_univ_2-1 y_univ_2-2 y_univ_2-3 y_univ_2-4 y_univ_2-5 y_univ_2-6 y_univ_2-7 ) (--Cons-2 y_univ_1-0 y_univ_1-1 y_univ_1-2 y_univ_1-3 y_univ_1-4 y_univ_1-5 y_univ_1-6 y_univ_1-7 y_univ_2-0 y_univ_2-1 y_univ_2-2 y_univ_2-3 y_univ_2-4 y_univ_2-5 y_univ_2-6 y_univ_2-7 ) (--Cons-3 y_univ_1-0 y_univ_1-1 y_univ_1-2 y_univ_1-3 y_univ_1-4 y_univ_1-5 y_univ_1-6 y_univ_1-7 y_univ_2-0 y_univ_2-1 y_univ_2-2 y_univ_2-3 y_univ_2-4 y_univ_2-5 y_univ_2-6 y_univ_2-7 ) (--Cons-4 y_univ_1-0 y_univ_1-1 y_univ_1-2 y_univ_1-3 y_univ_1-4 y_univ_1-5 y_univ_1-6 y_univ_1-7 y_univ_2-0 y_univ_2-1 y_univ_2-2 y_univ_2-3 y_univ_2-4 y_univ_2-5 y_univ_2-6 y_univ_2-7 ) (--Cons-5 y_univ_1-0 y_univ_1-1 y_univ_1-2 y_univ_1-3 y_univ_1-4 y_univ_1-5 y_univ_1-6 y_univ_1-7 y_univ_2-0 y_univ_2-1 y_univ_2-2 y_univ_2-3 y_univ_2-4 y_univ_2-5 y_univ_2-6 y_univ_2-7 ) (--Cons-6 y_univ_1-0 y_univ_1-1 y_univ_1-2 y_univ_1-3 y_univ_1-4 y_univ_1-5 y_univ_1-6 y_univ_1-7 y_univ_2-0 y_univ_2-1 y_univ_2-2 y_univ_2-3 y_univ_2-4 y_univ_2-5 y_univ_2-6 y_univ_2-7 ) (--Cons-7 y_univ_1-0 y_univ_1-1 y_univ_1-2 y_univ_1-3 y_univ_1-4 y_univ_1-5 y_univ_1-6 y_univ_1-7 y_univ_2-0 y_univ_2-1 y_univ_2-2 y_univ_2-3 y_univ_2-4 y_univ_2-5 y_univ_2-6 y_univ_2-7 ) ) (domain --Null-0 --Null-1 --Null-2 --Null-3 --Null-4 --Null-5 --Null-6 --Null-7 ) ) ) ) )

;About do check SAT
(check-sat )