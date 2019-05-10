(set-logic UF )

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

(declare-fun literal_13 () Bool )

(declare-fun literal_14 () Bool )

(declare-fun literal_15 () Bool )

(declare-fun literal_16 () Bool )

(declare-fun literal_17 () Bool )

(declare-fun literal_18 () Bool )

(declare-fun literal_19 () Bool )

(declare-fun literal_20 () Bool )

(declare-fun literal_21 () Bool )

(declare-fun literal_22 () Bool )

(declare-fun literal_23 () Bool )

(declare-fun literal_24 () Bool )

(declare-fun literal_25 () Bool )

(declare-fun literal_26 () Bool )

(declare-fun literal_27 () Bool )

(declare-fun literal_28 () Bool )

(declare-fun literal_29 () Bool )

(declare-fun literal_30 () Bool )

(declare-fun literal_31 () Bool )

(declare-fun literal_32 () Bool )

(declare-fun literal_33 () Bool )

(declare-fun literal_34 () Bool )

(declare-fun literal_35 () Bool )


(assert (and (and (and (and literal_2 (=> literal_19 (and literal_5 literal_25 ) ) (=> (not literal_19 ) literal_0 ) (=> literal_14 (and literal_7 literal_26 ) ) (=> (not literal_14 ) literal_1 ) (and literal_20 literal_15 ) (= (and literal_9 literal_27 ) (and literal_17 literal_31 ) ) (= (and literal_11 literal_28 ) (and literal_17 literal_31 ) ) ) (not literal_23 ) ) (and literal_3 literal_4 literal_6 literal_8 literal_10 literal_12 literal_13 literal_16 literal_18 literal_21 literal_22 (not literal_23 ) literal_24 literal_25 literal_26 literal_27 literal_28 literal_29 literal_30 literal_31 literal_32 literal_33 literal_34 literal_35 ) ) (and (=> (and literal_0 literal_4 ) literal_3 ) (=> (and literal_1 literal_6 ) literal_3 ) (=> (and literal_2 literal_12 ) literal_3 ) (=> (and literal_3 literal_22 ) literal_3 ) (=> (and literal_4 literal_22 ) literal_4 ) (=> (and literal_4 literal_23 ) literal_5 ) (=> (and literal_5 literal_34 ) literal_4 ) (=> (and literal_5 literal_35 ) literal_5 ) (=> (and literal_6 literal_22 ) literal_6 ) (=> (and literal_6 literal_23 ) literal_7 ) (=> (and literal_7 literal_34 ) literal_6 ) (=> (and literal_7 literal_35 ) literal_7 ) (=> (and literal_8 literal_22 ) literal_8 ) (=> (and literal_8 literal_23 ) literal_9 ) (=> (and literal_9 literal_34 ) literal_8 ) (=> (and literal_9 literal_35 ) literal_9 ) (=> (and literal_10 literal_22 ) literal_10 ) (=> (and literal_10 literal_23 ) literal_11 ) (=> (and literal_11 literal_34 ) literal_10 ) (=> (and literal_11 literal_35 ) literal_11 ) (=> (and literal_12 literal_22 ) literal_12 ) (=> (and literal_13 literal_22 ) literal_13 ) (=> (and literal_13 literal_23 ) literal_14 ) (=> (and literal_14 literal_34 ) literal_13 ) (=> (and literal_14 literal_35 ) literal_14 ) (=> (and literal_15 literal_21 ) literal_16 ) (=> (and literal_16 literal_22 ) literal_16 ) (=> (and literal_16 literal_23 ) literal_17 ) (=> (and literal_17 literal_33 ) literal_15 ) (=> (and literal_17 literal_34 ) literal_16 ) (=> (and literal_17 literal_35 ) literal_17 ) (=> (and literal_18 literal_22 ) literal_18 ) (=> (and literal_18 literal_23 ) literal_19 ) (=> (and literal_19 literal_34 ) literal_18 ) (=> (and literal_19 literal_35 ) literal_19 ) (=> (and literal_20 literal_16 ) literal_21 ) (=> (and literal_21 literal_22 ) literal_21 ) (=> (and literal_22 literal_22 ) literal_22 ) (=> (and literal_22 literal_23 ) literal_23 ) (=> (and literal_23 literal_34 ) literal_22 ) (=> (and literal_23 literal_35 ) literal_23 ) (=> (and literal_24 literal_0 ) literal_25 ) (=> (and literal_24 literal_1 ) literal_26 ) (=> (and literal_24 literal_2 ) literal_29 ) (=> (and literal_24 literal_3 ) literal_34 ) (=> (and literal_25 literal_4 ) literal_34 ) (=> (and literal_25 literal_5 ) literal_35 ) (=> (and literal_26 literal_6 ) literal_34 ) (=> (and literal_26 literal_7 ) literal_35 ) (=> (and literal_27 literal_8 ) literal_34 ) (=> (and literal_27 literal_9 ) literal_35 ) (=> (and literal_28 literal_10 ) literal_34 ) (=> (and literal_28 literal_11 ) literal_35 ) (=> (and literal_29 literal_12 ) literal_34 ) (=> (and literal_30 literal_13 ) literal_34 ) (=> (and literal_30 literal_14 ) literal_35 ) (=> (and literal_31 literal_15 ) literal_33 ) (=> (and literal_31 literal_16 ) literal_34 ) (=> (and literal_31 literal_17 ) literal_35 ) (=> (and literal_32 literal_18 ) literal_34 ) (=> (and literal_32 literal_19 ) literal_35 ) (=> (and literal_33 literal_20 ) literal_31 ) (=> (and literal_33 literal_21 ) literal_34 ) (=> (and literal_34 literal_22 ) literal_34 ) (=> (and literal_34 literal_23 ) literal_35 ) (=> (and literal_35 literal_24 ) literal_24 ) (=> (and literal_35 literal_25 ) literal_25 ) (=> (and literal_35 literal_26 ) literal_26 ) (=> (and literal_35 literal_27 ) literal_27 ) (=> (and literal_35 literal_28 ) literal_28 ) (=> (and literal_35 literal_29 ) literal_29 ) (=> (and literal_35 literal_30 ) literal_30 ) (=> (and literal_35 literal_31 ) literal_31 ) (=> (and literal_35 literal_32 ) literal_32 ) (=> (and literal_35 literal_33 ) literal_33 ) (=> (and literal_35 literal_34 ) literal_34 ) (=> (and literal_35 literal_35 ) literal_35 ) ) ) )

(declare-fun domainToBeSolved (Bool Bool Bool Bool Bool Bool Bool Bool Bool ) Bool )

(define-fun domain ((z_boolDomain-0 Bool ) (z_boolDomain-1 Bool ) (z_boolDomain-2 Bool ) (z_boolDomain-3 Bool ) (z_boolDomain-4 Bool ) (z_boolDomain-5 Bool ) (z_boolDomain-6 Bool ) (z_boolDomain-7 Bool ) (z_boolDomain-8 Bool ) ) Bool (and (and (=> literal_0 (=> z_boolDomain-0 z_boolDomain-1 ) ) (=> literal_1 (=> z_boolDomain-0 z_boolDomain-2 ) ) (=> literal_2 (=> z_boolDomain-0 (or z_boolDomain-4 z_boolDomain-3 ) ) ) (=> literal_3 (=> z_boolDomain-0 true ) ) (=> literal_4 (=> z_boolDomain-1 true ) ) (=> literal_5 (=> z_boolDomain-1 false ) ) (=> literal_6 (=> z_boolDomain-2 true ) ) (=> literal_7 (=> z_boolDomain-2 false ) ) (=> literal_8 (=> z_boolDomain-5 true ) ) (=> literal_9 (=> z_boolDomain-5 false ) ) (=> literal_10 (=> z_boolDomain-6 true ) ) (=> literal_11 (=> z_boolDomain-6 false ) ) (=> literal_12 (=> (or z_boolDomain-4 z_boolDomain-3 ) true ) ) (=> literal_13 (=> (and z_boolDomain-0 z_boolDomain-4 ) true ) ) (=> literal_14 (=> (and z_boolDomain-0 z_boolDomain-4 ) false ) ) (=> literal_15 (=> (and z_boolDomain-0 z_boolDomain-7 ) z_boolDomain-8 ) ) (=> literal_16 (=> (and z_boolDomain-0 z_boolDomain-7 ) true ) ) (=> literal_17 (=> (and z_boolDomain-0 z_boolDomain-7 ) false ) ) (=> literal_18 (=> (and z_boolDomain-0 z_boolDomain-3 ) true ) ) (=> literal_19 (=> (and z_boolDomain-0 z_boolDomain-3 ) false ) ) (=> literal_20 (=> z_boolDomain-8 (and z_boolDomain-0 z_boolDomain-7 ) ) ) (=> literal_21 (=> z_boolDomain-8 true ) ) (=> literal_22 (=> true true ) ) (=> literal_23 (=> true false ) ) (=> literal_24 (=> false z_boolDomain-0 ) ) (=> literal_25 (=> false z_boolDomain-1 ) ) (=> literal_26 (=> false z_boolDomain-2 ) ) (=> literal_27 (=> false z_boolDomain-5 ) ) (=> literal_28 (=> false z_boolDomain-6 ) ) (=> literal_29 (=> false (or z_boolDomain-4 z_boolDomain-3 ) ) ) (=> literal_30 (=> false (and z_boolDomain-0 z_boolDomain-4 ) ) ) (=> literal_31 (=> false (and z_boolDomain-0 z_boolDomain-7 ) ) ) (=> literal_32 (=> false (and z_boolDomain-0 z_boolDomain-3 ) ) ) (=> literal_33 (=> false z_boolDomain-8 ) ) (=> literal_34 (=> false true ) ) (=> literal_35 (=> false false ) ) ) (domainToBeSolved z_boolDomain-0 z_boolDomain-1 z_boolDomain-2 z_boolDomain-3 z_boolDomain-4 z_boolDomain-5 z_boolDomain-6 z_boolDomain-7 z_boolDomain-8 ) ) )

(declare-fun --Cons-0 (Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool ) Bool )

(declare-fun --Cons-1 (Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool ) Bool )

(declare-fun --Cons-2 (Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool ) Bool )

(declare-fun --Cons-5 (Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool ) Bool )

(declare-fun --Cons-6 (Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool ) Bool )

(define-fun --Cons-3 ((f-arg-0-0 Bool ) (f-arg-0-1 Bool ) (f-arg-0-2 Bool ) (f-arg-0-3 Bool ) (f-arg-0-4 Bool ) (f-arg-0-5 Bool ) (f-arg-0-6 Bool ) (f-arg-0-7 Bool ) (f-arg-0-8 Bool ) (f-arg-1-0 Bool ) (f-arg-1-1 Bool ) (f-arg-1-2 Bool ) (f-arg-1-3 Bool ) (f-arg-1-4 Bool ) (f-arg-1-5 Bool ) (f-arg-1-6 Bool ) (f-arg-1-7 Bool ) (f-arg-1-8 Bool ) ) Bool false )

(define-fun --Cons-4 ((f-arg-0-0 Bool ) (f-arg-0-1 Bool ) (f-arg-0-2 Bool ) (f-arg-0-3 Bool ) (f-arg-0-4 Bool ) (f-arg-0-5 Bool ) (f-arg-0-6 Bool ) (f-arg-0-7 Bool ) (f-arg-0-8 Bool ) (f-arg-1-0 Bool ) (f-arg-1-1 Bool ) (f-arg-1-2 Bool ) (f-arg-1-3 Bool ) (f-arg-1-4 Bool ) (f-arg-1-5 Bool ) (f-arg-1-6 Bool ) (f-arg-1-7 Bool ) (f-arg-1-8 Bool ) ) Bool (and true f-arg-1-3 ) )

(define-fun --Cons-7 ((f-arg-0-0 Bool ) (f-arg-0-1 Bool ) (f-arg-0-2 Bool ) (f-arg-0-3 Bool ) (f-arg-0-4 Bool ) (f-arg-0-5 Bool ) (f-arg-0-6 Bool ) (f-arg-0-7 Bool ) (f-arg-0-8 Bool ) (f-arg-1-0 Bool ) (f-arg-1-1 Bool ) (f-arg-1-2 Bool ) (f-arg-1-3 Bool ) (f-arg-1-4 Bool ) (f-arg-1-5 Bool ) (f-arg-1-6 Bool ) (f-arg-1-7 Bool ) (f-arg-1-8 Bool ) ) Bool (and true true ) )

(define-fun --Cons-8 ((f-arg-0-0 Bool ) (f-arg-0-1 Bool ) (f-arg-0-2 Bool ) (f-arg-0-3 Bool ) (f-arg-0-4 Bool ) (f-arg-0-5 Bool ) (f-arg-0-6 Bool ) (f-arg-0-7 Bool ) (f-arg-0-8 Bool ) (f-arg-1-0 Bool ) (f-arg-1-1 Bool ) (f-arg-1-2 Bool ) (f-arg-1-3 Bool ) (f-arg-1-4 Bool ) (f-arg-1-5 Bool ) (f-arg-1-6 Bool ) (f-arg-1-7 Bool ) (f-arg-1-8 Bool ) ) Bool (and f-arg-0-5 f-arg-1-6 ) )

(declare-fun --Null-0 () Bool )

(declare-fun --Null-1 () Bool )

(declare-fun --Null-2 () Bool )

(declare-fun --Null-5 () Bool )

(declare-fun --Null-6 () Bool )

(define-fun --Null-3 () Bool true )

(define-fun --Null-4 () Bool false )

(define-fun --Null-7 () Bool false )

(define-fun --Null-8 () Bool false )

(declare-fun x_exists_35-0 () Bool )

(declare-fun x_exists_35-1 () Bool )

(declare-fun x_exists_35-2 () Bool )

(declare-fun x_exists_35-3 () Bool )

(declare-fun x_exists_35-4 () Bool )

(declare-fun x_exists_35-5 () Bool )

(declare-fun x_exists_35-6 () Bool )

(declare-fun x_exists_35-7 () Bool )

(declare-fun x_exists_35-8 () Bool )

(assert (domain x_exists_35-0 x_exists_35-1 x_exists_35-2 x_exists_35-3 x_exists_35-4 x_exists_35-5 x_exists_35-6 x_exists_35-7 x_exists_35-8 ) )

(declare-fun x_exists_34-0 () Bool )

(declare-fun x_exists_34-1 () Bool )

(declare-fun x_exists_34-2 () Bool )

(declare-fun x_exists_34-3 () Bool )

(declare-fun x_exists_34-4 () Bool )

(declare-fun x_exists_34-5 () Bool )

(declare-fun x_exists_34-6 () Bool )

(declare-fun x_exists_34-7 () Bool )

(declare-fun x_exists_34-8 () Bool )

(assert (domain x_exists_34-0 x_exists_34-1 x_exists_34-2 x_exists_34-3 x_exists_34-4 x_exists_34-5 x_exists_34-6 x_exists_34-7 x_exists_34-8 ) )

(declare-fun x_exists_33-0 () Bool )

(declare-fun x_exists_33-1 () Bool )

(declare-fun x_exists_33-2 () Bool )

(declare-fun x_exists_33-3 () Bool )

(declare-fun x_exists_33-4 () Bool )

(declare-fun x_exists_33-5 () Bool )

(declare-fun x_exists_33-6 () Bool )

(declare-fun x_exists_33-7 () Bool )

(declare-fun x_exists_33-8 () Bool )

(assert (domain x_exists_33-0 x_exists_33-1 x_exists_33-2 x_exists_33-3 x_exists_33-4 x_exists_33-5 x_exists_33-6 x_exists_33-7 x_exists_33-8 ) )

(declare-fun x_exists_32-0 () Bool )

(declare-fun x_exists_32-1 () Bool )

(declare-fun x_exists_32-2 () Bool )

(declare-fun x_exists_32-3 () Bool )

(declare-fun x_exists_32-4 () Bool )

(declare-fun x_exists_32-5 () Bool )

(declare-fun x_exists_32-6 () Bool )

(declare-fun x_exists_32-7 () Bool )

(declare-fun x_exists_32-8 () Bool )

(assert (domain x_exists_32-0 x_exists_32-1 x_exists_32-2 x_exists_32-3 x_exists_32-4 x_exists_32-5 x_exists_32-6 x_exists_32-7 x_exists_32-8 ) )

(declare-fun x_exists_31-0 () Bool )

(declare-fun x_exists_31-1 () Bool )

(declare-fun x_exists_31-2 () Bool )

(declare-fun x_exists_31-3 () Bool )

(declare-fun x_exists_31-4 () Bool )

(declare-fun x_exists_31-5 () Bool )

(declare-fun x_exists_31-6 () Bool )

(declare-fun x_exists_31-7 () Bool )

(declare-fun x_exists_31-8 () Bool )

(assert (domain x_exists_31-0 x_exists_31-1 x_exists_31-2 x_exists_31-3 x_exists_31-4 x_exists_31-5 x_exists_31-6 x_exists_31-7 x_exists_31-8 ) )

(declare-fun x_exists_30-0 () Bool )

(declare-fun x_exists_30-1 () Bool )

(declare-fun x_exists_30-2 () Bool )

(declare-fun x_exists_30-3 () Bool )

(declare-fun x_exists_30-4 () Bool )

(declare-fun x_exists_30-5 () Bool )

(declare-fun x_exists_30-6 () Bool )

(declare-fun x_exists_30-7 () Bool )

(declare-fun x_exists_30-8 () Bool )

(assert (domain x_exists_30-0 x_exists_30-1 x_exists_30-2 x_exists_30-3 x_exists_30-4 x_exists_30-5 x_exists_30-6 x_exists_30-7 x_exists_30-8 ) )

(declare-fun x_exists_29-0 () Bool )

(declare-fun x_exists_29-1 () Bool )

(declare-fun x_exists_29-2 () Bool )

(declare-fun x_exists_29-3 () Bool )

(declare-fun x_exists_29-4 () Bool )

(declare-fun x_exists_29-5 () Bool )

(declare-fun x_exists_29-6 () Bool )

(declare-fun x_exists_29-7 () Bool )

(declare-fun x_exists_29-8 () Bool )

(assert (domain x_exists_29-0 x_exists_29-1 x_exists_29-2 x_exists_29-3 x_exists_29-4 x_exists_29-5 x_exists_29-6 x_exists_29-7 x_exists_29-8 ) )

(declare-fun x_exists_28-0 () Bool )

(declare-fun x_exists_28-1 () Bool )

(declare-fun x_exists_28-2 () Bool )

(declare-fun x_exists_28-3 () Bool )

(declare-fun x_exists_28-4 () Bool )

(declare-fun x_exists_28-5 () Bool )

(declare-fun x_exists_28-6 () Bool )

(declare-fun x_exists_28-7 () Bool )

(declare-fun x_exists_28-8 () Bool )

(assert (domain x_exists_28-0 x_exists_28-1 x_exists_28-2 x_exists_28-3 x_exists_28-4 x_exists_28-5 x_exists_28-6 x_exists_28-7 x_exists_28-8 ) )

(declare-fun x_exists_27-0 () Bool )

(declare-fun x_exists_27-1 () Bool )

(declare-fun x_exists_27-2 () Bool )

(declare-fun x_exists_27-3 () Bool )

(declare-fun x_exists_27-4 () Bool )

(declare-fun x_exists_27-5 () Bool )

(declare-fun x_exists_27-6 () Bool )

(declare-fun x_exists_27-7 () Bool )

(declare-fun x_exists_27-8 () Bool )

(assert (domain x_exists_27-0 x_exists_27-1 x_exists_27-2 x_exists_27-3 x_exists_27-4 x_exists_27-5 x_exists_27-6 x_exists_27-7 x_exists_27-8 ) )

(declare-fun x_exists_26-0 () Bool )

(declare-fun x_exists_26-1 () Bool )

(declare-fun x_exists_26-2 () Bool )

(declare-fun x_exists_26-3 () Bool )

(declare-fun x_exists_26-4 () Bool )

(declare-fun x_exists_26-5 () Bool )

(declare-fun x_exists_26-6 () Bool )

(declare-fun x_exists_26-7 () Bool )

(declare-fun x_exists_26-8 () Bool )

(assert (domain x_exists_26-0 x_exists_26-1 x_exists_26-2 x_exists_26-3 x_exists_26-4 x_exists_26-5 x_exists_26-6 x_exists_26-7 x_exists_26-8 ) )

(declare-fun x_exists_25-0 () Bool )

(declare-fun x_exists_25-1 () Bool )

(declare-fun x_exists_25-2 () Bool )

(declare-fun x_exists_25-3 () Bool )

(declare-fun x_exists_25-4 () Bool )

(declare-fun x_exists_25-5 () Bool )

(declare-fun x_exists_25-6 () Bool )

(declare-fun x_exists_25-7 () Bool )

(declare-fun x_exists_25-8 () Bool )

(assert (domain x_exists_25-0 x_exists_25-1 x_exists_25-2 x_exists_25-3 x_exists_25-4 x_exists_25-5 x_exists_25-6 x_exists_25-7 x_exists_25-8 ) )

(declare-fun x_exists_24-0 () Bool )

(declare-fun x_exists_24-1 () Bool )

(declare-fun x_exists_24-2 () Bool )

(declare-fun x_exists_24-3 () Bool )

(declare-fun x_exists_24-4 () Bool )

(declare-fun x_exists_24-5 () Bool )

(declare-fun x_exists_24-6 () Bool )

(declare-fun x_exists_24-7 () Bool )

(declare-fun x_exists_24-8 () Bool )

(assert (domain x_exists_24-0 x_exists_24-1 x_exists_24-2 x_exists_24-3 x_exists_24-4 x_exists_24-5 x_exists_24-6 x_exists_24-7 x_exists_24-8 ) )

(declare-fun x_exists_23-0 () Bool )

(declare-fun x_exists_23-1 () Bool )

(declare-fun x_exists_23-2 () Bool )

(declare-fun x_exists_23-3 () Bool )

(declare-fun x_exists_23-4 () Bool )

(declare-fun x_exists_23-5 () Bool )

(declare-fun x_exists_23-6 () Bool )

(declare-fun x_exists_23-7 () Bool )

(declare-fun x_exists_23-8 () Bool )

(assert (domain x_exists_23-0 x_exists_23-1 x_exists_23-2 x_exists_23-3 x_exists_23-4 x_exists_23-5 x_exists_23-6 x_exists_23-7 x_exists_23-8 ) )

(declare-fun x_exists_22-0 () Bool )

(declare-fun x_exists_22-1 () Bool )

(declare-fun x_exists_22-2 () Bool )

(declare-fun x_exists_22-3 () Bool )

(declare-fun x_exists_22-4 () Bool )

(declare-fun x_exists_22-5 () Bool )

(declare-fun x_exists_22-6 () Bool )

(declare-fun x_exists_22-7 () Bool )

(declare-fun x_exists_22-8 () Bool )

(assert (domain x_exists_22-0 x_exists_22-1 x_exists_22-2 x_exists_22-3 x_exists_22-4 x_exists_22-5 x_exists_22-6 x_exists_22-7 x_exists_22-8 ) )

(declare-fun x_exists_21-0 () Bool )

(declare-fun x_exists_21-1 () Bool )

(declare-fun x_exists_21-2 () Bool )

(declare-fun x_exists_21-3 () Bool )

(declare-fun x_exists_21-4 () Bool )

(declare-fun x_exists_21-5 () Bool )

(declare-fun x_exists_21-6 () Bool )

(declare-fun x_exists_21-7 () Bool )

(declare-fun x_exists_21-8 () Bool )

(assert (domain x_exists_21-0 x_exists_21-1 x_exists_21-2 x_exists_21-3 x_exists_21-4 x_exists_21-5 x_exists_21-6 x_exists_21-7 x_exists_21-8 ) )

(declare-fun x_exists_20-0 () Bool )

(declare-fun x_exists_20-1 () Bool )

(declare-fun x_exists_20-2 () Bool )

(declare-fun x_exists_20-3 () Bool )

(declare-fun x_exists_20-4 () Bool )

(declare-fun x_exists_20-5 () Bool )

(declare-fun x_exists_20-6 () Bool )

(declare-fun x_exists_20-7 () Bool )

(declare-fun x_exists_20-8 () Bool )

(assert (domain x_exists_20-0 x_exists_20-1 x_exists_20-2 x_exists_20-3 x_exists_20-4 x_exists_20-5 x_exists_20-6 x_exists_20-7 x_exists_20-8 ) )

(declare-fun x_exists_19-0 () Bool )

(declare-fun x_exists_19-1 () Bool )

(declare-fun x_exists_19-2 () Bool )

(declare-fun x_exists_19-3 () Bool )

(declare-fun x_exists_19-4 () Bool )

(declare-fun x_exists_19-5 () Bool )

(declare-fun x_exists_19-6 () Bool )

(declare-fun x_exists_19-7 () Bool )

(declare-fun x_exists_19-8 () Bool )

(assert (domain x_exists_19-0 x_exists_19-1 x_exists_19-2 x_exists_19-3 x_exists_19-4 x_exists_19-5 x_exists_19-6 x_exists_19-7 x_exists_19-8 ) )

(declare-fun x_exists_18-0 () Bool )

(declare-fun x_exists_18-1 () Bool )

(declare-fun x_exists_18-2 () Bool )

(declare-fun x_exists_18-3 () Bool )

(declare-fun x_exists_18-4 () Bool )

(declare-fun x_exists_18-5 () Bool )

(declare-fun x_exists_18-6 () Bool )

(declare-fun x_exists_18-7 () Bool )

(declare-fun x_exists_18-8 () Bool )

(assert (domain x_exists_18-0 x_exists_18-1 x_exists_18-2 x_exists_18-3 x_exists_18-4 x_exists_18-5 x_exists_18-6 x_exists_18-7 x_exists_18-8 ) )

(declare-fun x_exists_17-0 () Bool )

(declare-fun x_exists_17-1 () Bool )

(declare-fun x_exists_17-2 () Bool )

(declare-fun x_exists_17-3 () Bool )

(declare-fun x_exists_17-4 () Bool )

(declare-fun x_exists_17-5 () Bool )

(declare-fun x_exists_17-6 () Bool )

(declare-fun x_exists_17-7 () Bool )

(declare-fun x_exists_17-8 () Bool )

(assert (domain x_exists_17-0 x_exists_17-1 x_exists_17-2 x_exists_17-3 x_exists_17-4 x_exists_17-5 x_exists_17-6 x_exists_17-7 x_exists_17-8 ) )

(declare-fun x_exists_16-0 () Bool )

(declare-fun x_exists_16-1 () Bool )

(declare-fun x_exists_16-2 () Bool )

(declare-fun x_exists_16-3 () Bool )

(declare-fun x_exists_16-4 () Bool )

(declare-fun x_exists_16-5 () Bool )

(declare-fun x_exists_16-6 () Bool )

(declare-fun x_exists_16-7 () Bool )

(declare-fun x_exists_16-8 () Bool )

(assert (domain x_exists_16-0 x_exists_16-1 x_exists_16-2 x_exists_16-3 x_exists_16-4 x_exists_16-5 x_exists_16-6 x_exists_16-7 x_exists_16-8 ) )

(declare-fun x_exists_15-0 () Bool )

(declare-fun x_exists_15-1 () Bool )

(declare-fun x_exists_15-2 () Bool )

(declare-fun x_exists_15-3 () Bool )

(declare-fun x_exists_15-4 () Bool )

(declare-fun x_exists_15-5 () Bool )

(declare-fun x_exists_15-6 () Bool )

(declare-fun x_exists_15-7 () Bool )

(declare-fun x_exists_15-8 () Bool )

(assert (domain x_exists_15-0 x_exists_15-1 x_exists_15-2 x_exists_15-3 x_exists_15-4 x_exists_15-5 x_exists_15-6 x_exists_15-7 x_exists_15-8 ) )

(declare-fun x_exists_14-0 () Bool )

(declare-fun x_exists_14-1 () Bool )

(declare-fun x_exists_14-2 () Bool )

(declare-fun x_exists_14-3 () Bool )

(declare-fun x_exists_14-4 () Bool )

(declare-fun x_exists_14-5 () Bool )

(declare-fun x_exists_14-6 () Bool )

(declare-fun x_exists_14-7 () Bool )

(declare-fun x_exists_14-8 () Bool )

(assert (domain x_exists_14-0 x_exists_14-1 x_exists_14-2 x_exists_14-3 x_exists_14-4 x_exists_14-5 x_exists_14-6 x_exists_14-7 x_exists_14-8 ) )

(declare-fun x_exists_13-0 () Bool )

(declare-fun x_exists_13-1 () Bool )

(declare-fun x_exists_13-2 () Bool )

(declare-fun x_exists_13-3 () Bool )

(declare-fun x_exists_13-4 () Bool )

(declare-fun x_exists_13-5 () Bool )

(declare-fun x_exists_13-6 () Bool )

(declare-fun x_exists_13-7 () Bool )

(declare-fun x_exists_13-8 () Bool )

(assert (domain x_exists_13-0 x_exists_13-1 x_exists_13-2 x_exists_13-3 x_exists_13-4 x_exists_13-5 x_exists_13-6 x_exists_13-7 x_exists_13-8 ) )

(declare-fun x_exists_12-0 () Bool )

(declare-fun x_exists_12-1 () Bool )

(declare-fun x_exists_12-2 () Bool )

(declare-fun x_exists_12-3 () Bool )

(declare-fun x_exists_12-4 () Bool )

(declare-fun x_exists_12-5 () Bool )

(declare-fun x_exists_12-6 () Bool )

(declare-fun x_exists_12-7 () Bool )

(declare-fun x_exists_12-8 () Bool )

(assert (domain x_exists_12-0 x_exists_12-1 x_exists_12-2 x_exists_12-3 x_exists_12-4 x_exists_12-5 x_exists_12-6 x_exists_12-7 x_exists_12-8 ) )

(declare-fun x_exists_11-0 () Bool )

(declare-fun x_exists_11-1 () Bool )

(declare-fun x_exists_11-2 () Bool )

(declare-fun x_exists_11-3 () Bool )

(declare-fun x_exists_11-4 () Bool )

(declare-fun x_exists_11-5 () Bool )

(declare-fun x_exists_11-6 () Bool )

(declare-fun x_exists_11-7 () Bool )

(declare-fun x_exists_11-8 () Bool )

(assert (domain x_exists_11-0 x_exists_11-1 x_exists_11-2 x_exists_11-3 x_exists_11-4 x_exists_11-5 x_exists_11-6 x_exists_11-7 x_exists_11-8 ) )

(declare-fun x_exists_10-0 () Bool )

(declare-fun x_exists_10-1 () Bool )

(declare-fun x_exists_10-2 () Bool )

(declare-fun x_exists_10-3 () Bool )

(declare-fun x_exists_10-4 () Bool )

(declare-fun x_exists_10-5 () Bool )

(declare-fun x_exists_10-6 () Bool )

(declare-fun x_exists_10-7 () Bool )

(declare-fun x_exists_10-8 () Bool )

(assert (domain x_exists_10-0 x_exists_10-1 x_exists_10-2 x_exists_10-3 x_exists_10-4 x_exists_10-5 x_exists_10-6 x_exists_10-7 x_exists_10-8 ) )

(declare-fun x_exists_9-0 () Bool )

(declare-fun x_exists_9-1 () Bool )

(declare-fun x_exists_9-2 () Bool )

(declare-fun x_exists_9-3 () Bool )

(declare-fun x_exists_9-4 () Bool )

(declare-fun x_exists_9-5 () Bool )

(declare-fun x_exists_9-6 () Bool )

(declare-fun x_exists_9-7 () Bool )

(declare-fun x_exists_9-8 () Bool )

(assert (domain x_exists_9-0 x_exists_9-1 x_exists_9-2 x_exists_9-3 x_exists_9-4 x_exists_9-5 x_exists_9-6 x_exists_9-7 x_exists_9-8 ) )

(declare-fun x_exists_8-0 () Bool )

(declare-fun x_exists_8-1 () Bool )

(declare-fun x_exists_8-2 () Bool )

(declare-fun x_exists_8-3 () Bool )

(declare-fun x_exists_8-4 () Bool )

(declare-fun x_exists_8-5 () Bool )

(declare-fun x_exists_8-6 () Bool )

(declare-fun x_exists_8-7 () Bool )

(declare-fun x_exists_8-8 () Bool )

(assert (domain x_exists_8-0 x_exists_8-1 x_exists_8-2 x_exists_8-3 x_exists_8-4 x_exists_8-5 x_exists_8-6 x_exists_8-7 x_exists_8-8 ) )

(declare-fun x_exists_7-0 () Bool )

(declare-fun x_exists_7-1 () Bool )

(declare-fun x_exists_7-2 () Bool )

(declare-fun x_exists_7-3 () Bool )

(declare-fun x_exists_7-4 () Bool )

(declare-fun x_exists_7-5 () Bool )

(declare-fun x_exists_7-6 () Bool )

(declare-fun x_exists_7-7 () Bool )

(declare-fun x_exists_7-8 () Bool )

(assert (domain x_exists_7-0 x_exists_7-1 x_exists_7-2 x_exists_7-3 x_exists_7-4 x_exists_7-5 x_exists_7-6 x_exists_7-7 x_exists_7-8 ) )

(declare-fun x_exists_6-0 () Bool )

(declare-fun x_exists_6-1 () Bool )

(declare-fun x_exists_6-2 () Bool )

(declare-fun x_exists_6-3 () Bool )

(declare-fun x_exists_6-4 () Bool )

(declare-fun x_exists_6-5 () Bool )

(declare-fun x_exists_6-6 () Bool )

(declare-fun x_exists_6-7 () Bool )

(declare-fun x_exists_6-8 () Bool )

(assert (domain x_exists_6-0 x_exists_6-1 x_exists_6-2 x_exists_6-3 x_exists_6-4 x_exists_6-5 x_exists_6-6 x_exists_6-7 x_exists_6-8 ) )

(declare-fun x_exists_5-0 () Bool )

(declare-fun x_exists_5-1 () Bool )

(declare-fun x_exists_5-2 () Bool )

(declare-fun x_exists_5-3 () Bool )

(declare-fun x_exists_5-4 () Bool )

(declare-fun x_exists_5-5 () Bool )

(declare-fun x_exists_5-6 () Bool )

(declare-fun x_exists_5-7 () Bool )

(declare-fun x_exists_5-8 () Bool )

(assert (domain x_exists_5-0 x_exists_5-1 x_exists_5-2 x_exists_5-3 x_exists_5-4 x_exists_5-5 x_exists_5-6 x_exists_5-7 x_exists_5-8 ) )

(declare-fun x_exists_4-0 () Bool )

(declare-fun x_exists_4-1 () Bool )

(declare-fun x_exists_4-2 () Bool )

(declare-fun x_exists_4-3 () Bool )

(declare-fun x_exists_4-4 () Bool )

(declare-fun x_exists_4-5 () Bool )

(declare-fun x_exists_4-6 () Bool )

(declare-fun x_exists_4-7 () Bool )

(declare-fun x_exists_4-8 () Bool )

(assert (domain x_exists_4-0 x_exists_4-1 x_exists_4-2 x_exists_4-3 x_exists_4-4 x_exists_4-5 x_exists_4-6 x_exists_4-7 x_exists_4-8 ) )

(declare-fun x_exists_3-0 () Bool )

(declare-fun x_exists_3-1 () Bool )

(declare-fun x_exists_3-2 () Bool )

(declare-fun x_exists_3-3 () Bool )

(declare-fun x_exists_3-4 () Bool )

(declare-fun x_exists_3-5 () Bool )

(declare-fun x_exists_3-6 () Bool )

(declare-fun x_exists_3-7 () Bool )

(declare-fun x_exists_3-8 () Bool )

(assert (domain x_exists_3-0 x_exists_3-1 x_exists_3-2 x_exists_3-3 x_exists_3-4 x_exists_3-5 x_exists_3-6 x_exists_3-7 x_exists_3-8 ) )

(declare-fun x_exists_2-0 () Bool )

(declare-fun x_exists_2-1 () Bool )

(declare-fun x_exists_2-2 () Bool )

(declare-fun x_exists_2-3 () Bool )

(declare-fun x_exists_2-4 () Bool )

(declare-fun x_exists_2-5 () Bool )

(declare-fun x_exists_2-6 () Bool )

(declare-fun x_exists_2-7 () Bool )

(declare-fun x_exists_2-8 () Bool )

(assert (domain x_exists_2-0 x_exists_2-1 x_exists_2-2 x_exists_2-3 x_exists_2-4 x_exists_2-5 x_exists_2-6 x_exists_2-7 x_exists_2-8 ) )

(declare-fun x_exists_1-0 () Bool )

(declare-fun x_exists_1-1 () Bool )

(declare-fun x_exists_1-2 () Bool )

(declare-fun x_exists_1-3 () Bool )

(declare-fun x_exists_1-4 () Bool )

(declare-fun x_exists_1-5 () Bool )

(declare-fun x_exists_1-6 () Bool )

(declare-fun x_exists_1-7 () Bool )

(declare-fun x_exists_1-8 () Bool )

(assert (domain x_exists_1-0 x_exists_1-1 x_exists_1-2 x_exists_1-3 x_exists_1-4 x_exists_1-5 x_exists_1-6 x_exists_1-7 x_exists_1-8 ) )

(declare-fun x_exists_0-0 () Bool )

(declare-fun x_exists_0-1 () Bool )

(declare-fun x_exists_0-2 () Bool )

(declare-fun x_exists_0-3 () Bool )

(declare-fun x_exists_0-4 () Bool )

(declare-fun x_exists_0-5 () Bool )

(declare-fun x_exists_0-6 () Bool )

(declare-fun x_exists_0-7 () Bool )

(declare-fun x_exists_0-8 () Bool )

(assert (domain x_exists_0-0 x_exists_0-1 x_exists_0-2 x_exists_0-3 x_exists_0-4 x_exists_0-5 x_exists_0-6 x_exists_0-7 x_exists_0-8 ) )

(assert (=> (not literal_0 ) (and x_exists_0-0 (not x_exists_0-1 ) ) ) )

(assert (=> (not literal_1 ) (and x_exists_1-0 (not x_exists_1-2 ) ) ) )

(assert (=> (not literal_2 ) (and x_exists_2-0 (not (or x_exists_2-4 x_exists_2-3 ) ) ) ) )

(assert (=> (not literal_3 ) (and x_exists_3-0 (not true ) ) ) )

(assert (=> (not literal_4 ) (and x_exists_4-1 (not true ) ) ) )

(assert (=> (not literal_5 ) (and x_exists_5-1 (not false ) ) ) )

(assert (=> (not literal_6 ) (and x_exists_6-2 (not true ) ) ) )

(assert (=> (not literal_7 ) (and x_exists_7-2 (not false ) ) ) )

(assert (=> (not literal_8 ) (and x_exists_8-5 (not true ) ) ) )

(assert (=> (not literal_9 ) (and x_exists_9-5 (not false ) ) ) )

(assert (=> (not literal_10 ) (and x_exists_10-6 (not true ) ) ) )

(assert (=> (not literal_11 ) (and x_exists_11-6 (not false ) ) ) )

(assert (=> (not literal_12 ) (and (or x_exists_12-4 x_exists_12-3 ) (not true ) ) ) )

(assert (=> (not literal_13 ) (and (and x_exists_13-0 x_exists_13-4 ) (not true ) ) ) )

(assert (=> (not literal_14 ) (and (and x_exists_14-0 x_exists_14-4 ) (not false ) ) ) )

(assert (=> (not literal_15 ) (and (and x_exists_15-0 x_exists_15-7 ) (not x_exists_15-8 ) ) ) )

(assert (=> (not literal_16 ) (and (and x_exists_16-0 x_exists_16-7 ) (not true ) ) ) )

(assert (=> (not literal_17 ) (and (and x_exists_17-0 x_exists_17-7 ) (not false ) ) ) )

(assert (=> (not literal_18 ) (and (and x_exists_18-0 x_exists_18-3 ) (not true ) ) ) )

(assert (=> (not literal_19 ) (and (and x_exists_19-0 x_exists_19-3 ) (not false ) ) ) )

(assert (=> (not literal_20 ) (and x_exists_20-8 (not (and x_exists_20-0 x_exists_20-7 ) ) ) ) )

(assert (=> (not literal_21 ) (and x_exists_21-8 (not true ) ) ) )

(assert (=> (not literal_22 ) (and true (not true ) ) ) )

(assert (=> (not literal_23 ) (and true (not false ) ) ) )

(assert (=> (not literal_24 ) (and false (not x_exists_24-0 ) ) ) )

(assert (=> (not literal_25 ) (and false (not x_exists_25-1 ) ) ) )

(assert (=> (not literal_26 ) (and false (not x_exists_26-2 ) ) ) )

(assert (=> (not literal_27 ) (and false (not x_exists_27-5 ) ) ) )

(assert (=> (not literal_28 ) (and false (not x_exists_28-6 ) ) ) )

(assert (=> (not literal_29 ) (and false (not (or x_exists_29-4 x_exists_29-3 ) ) ) ) )

(assert (=> (not literal_30 ) (and false (not (and x_exists_30-0 x_exists_30-4 ) ) ) ) )

(assert (=> (not literal_31 ) (and false (not (and x_exists_31-0 x_exists_31-7 ) ) ) ) )

(assert (=> (not literal_32 ) (and false (not (and x_exists_32-0 x_exists_32-3 ) ) ) ) )

(assert (=> (not literal_33 ) (and false (not x_exists_33-8 ) ) ) )

(assert (=> (not literal_34 ) (and false (not true ) ) ) )

(assert (=> (not literal_35 ) (and false (not false ) ) ) )

(assert (forall ((y_univ_1-0 Bool ) (y_univ_1-1 Bool ) (y_univ_1-2 Bool ) (y_univ_1-3 Bool ) (y_univ_1-4 Bool ) (y_univ_1-5 Bool ) (y_univ_1-6 Bool ) (y_univ_1-7 Bool ) (y_univ_1-8 Bool ) (y_univ_2-0 Bool ) (y_univ_2-1 Bool ) (y_univ_2-2 Bool ) (y_univ_2-3 Bool ) (y_univ_2-4 Bool ) (y_univ_2-5 Bool ) (y_univ_2-6 Bool ) (y_univ_2-7 Bool ) (y_univ_2-8 Bool ) (y_univ_3-0 Bool ) (y_univ_3-1 Bool ) (y_univ_3-2 Bool ) (y_univ_3-3 Bool ) (y_univ_3-4 Bool ) (y_univ_3-5 Bool ) (y_univ_3-6 Bool ) (y_univ_3-7 Bool ) (y_univ_3-8 Bool ) (y_univ_4-0 Bool ) (y_univ_4-1 Bool ) (y_univ_4-2 Bool ) (y_univ_4-3 Bool ) (y_univ_4-4 Bool ) (y_univ_4-5 Bool ) (y_univ_4-6 Bool ) (y_univ_4-7 Bool ) (y_univ_4-8 Bool ) ) (and (=> (and (domain y_univ_1-0 y_univ_1-1 y_univ_1-2 y_univ_1-3 y_univ_1-4 y_univ_1-5 y_univ_1-6 y_univ_1-7 y_univ_1-8 ) (domain y_univ_2-0 y_univ_2-1 y_univ_2-2 y_univ_2-3 y_univ_2-4 y_univ_2-5 y_univ_2-6 y_univ_2-7 y_univ_2-8 ) (domain y_univ_3-0 y_univ_3-1 y_univ_3-2 y_univ_3-3 y_univ_3-4 y_univ_3-5 y_univ_3-6 y_univ_3-7 y_univ_3-8 ) (domain y_univ_4-0 y_univ_4-1 y_univ_4-2 y_univ_4-3 y_univ_4-4 y_univ_4-5 y_univ_4-6 y_univ_4-7 y_univ_4-8 ) ) (and true true true true (and (= true --Null-3 ) (= (--Cons-3 y_univ_1-0 y_univ_1-1 y_univ_1-2 y_univ_1-3 y_univ_1-4 y_univ_1-5 y_univ_1-6 y_univ_1-7 y_univ_1-8 y_univ_2-0 y_univ_2-1 y_univ_2-2 y_univ_2-3 y_univ_2-4 y_univ_2-5 y_univ_2-6 y_univ_2-7 y_univ_2-8 ) false ) ) (and (= (and true y_univ_2-3 ) (--Cons-4 y_univ_1-0 y_univ_1-1 y_univ_1-2 y_univ_1-3 y_univ_1-4 y_univ_1-5 y_univ_1-6 y_univ_1-7 y_univ_1-8 y_univ_2-0 y_univ_2-1 y_univ_2-2 y_univ_2-3 y_univ_2-4 y_univ_2-5 y_univ_2-6 y_univ_2-7 y_univ_2-8 ) ) (= --Null-4 false ) ) true true true true true (and (= (and true true ) (--Cons-7 y_univ_1-0 y_univ_1-1 y_univ_1-2 y_univ_1-3 y_univ_1-4 y_univ_1-5 y_univ_1-6 y_univ_1-7 y_univ_1-8 y_univ_2-0 y_univ_2-1 y_univ_2-2 y_univ_2-3 y_univ_2-4 y_univ_2-5 y_univ_2-6 y_univ_2-7 y_univ_2-8 ) ) (= --Null-7 false ) ) true (and (= (and y_univ_1-5 y_univ_2-6 ) (--Cons-8 y_univ_1-0 y_univ_1-1 y_univ_1-2 y_univ_1-3 y_univ_1-4 y_univ_1-5 y_univ_1-6 y_univ_1-7 y_univ_1-8 y_univ_2-0 y_univ_2-1 y_univ_2-2 y_univ_2-3 y_univ_2-4 y_univ_2-5 y_univ_2-6 y_univ_2-7 y_univ_2-8 ) ) (= --Null-8 false ) ) true ) ) (and (domain (--Cons-0 y_univ_1-0 y_univ_1-1 y_univ_1-2 y_univ_1-3 y_univ_1-4 y_univ_1-5 y_univ_1-6 y_univ_1-7 y_univ_1-8 y_univ_2-0 y_univ_2-1 y_univ_2-2 y_univ_2-3 y_univ_2-4 y_univ_2-5 y_univ_2-6 y_univ_2-7 y_univ_2-8 ) (--Cons-1 y_univ_1-0 y_univ_1-1 y_univ_1-2 y_univ_1-3 y_univ_1-4 y_univ_1-5 y_univ_1-6 y_univ_1-7 y_univ_1-8 y_univ_2-0 y_univ_2-1 y_univ_2-2 y_univ_2-3 y_univ_2-4 y_univ_2-5 y_univ_2-6 y_univ_2-7 y_univ_2-8 ) (--Cons-2 y_univ_1-0 y_univ_1-1 y_univ_1-2 y_univ_1-3 y_univ_1-4 y_univ_1-5 y_univ_1-6 y_univ_1-7 y_univ_1-8 y_univ_2-0 y_univ_2-1 y_univ_2-2 y_univ_2-3 y_univ_2-4 y_univ_2-5 y_univ_2-6 y_univ_2-7 y_univ_2-8 ) (--Cons-3 y_univ_1-0 y_univ_1-1 y_univ_1-2 y_univ_1-3 y_univ_1-4 y_univ_1-5 y_univ_1-6 y_univ_1-7 y_univ_1-8 y_univ_2-0 y_univ_2-1 y_univ_2-2 y_univ_2-3 y_univ_2-4 y_univ_2-5 y_univ_2-6 y_univ_2-7 y_univ_2-8 ) (--Cons-4 y_univ_1-0 y_univ_1-1 y_univ_1-2 y_univ_1-3 y_univ_1-4 y_univ_1-5 y_univ_1-6 y_univ_1-7 y_univ_1-8 y_univ_2-0 y_univ_2-1 y_univ_2-2 y_univ_2-3 y_univ_2-4 y_univ_2-5 y_univ_2-6 y_univ_2-7 y_univ_2-8 ) (--Cons-5 y_univ_1-0 y_univ_1-1 y_univ_1-2 y_univ_1-3 y_univ_1-4 y_univ_1-5 y_univ_1-6 y_univ_1-7 y_univ_1-8 y_univ_2-0 y_univ_2-1 y_univ_2-2 y_univ_2-3 y_univ_2-4 y_univ_2-5 y_univ_2-6 y_univ_2-7 y_univ_2-8 ) (--Cons-6 y_univ_1-0 y_univ_1-1 y_univ_1-2 y_univ_1-3 y_univ_1-4 y_univ_1-5 y_univ_1-6 y_univ_1-7 y_univ_1-8 y_univ_2-0 y_univ_2-1 y_univ_2-2 y_univ_2-3 y_univ_2-4 y_univ_2-5 y_univ_2-6 y_univ_2-7 y_univ_2-8 ) (--Cons-7 y_univ_1-0 y_univ_1-1 y_univ_1-2 y_univ_1-3 y_univ_1-4 y_univ_1-5 y_univ_1-6 y_univ_1-7 y_univ_1-8 y_univ_2-0 y_univ_2-1 y_univ_2-2 y_univ_2-3 y_univ_2-4 y_univ_2-5 y_univ_2-6 y_univ_2-7 y_univ_2-8 ) (--Cons-8 y_univ_1-0 y_univ_1-1 y_univ_1-2 y_univ_1-3 y_univ_1-4 y_univ_1-5 y_univ_1-6 y_univ_1-7 y_univ_1-8 y_univ_2-0 y_univ_2-1 y_univ_2-2 y_univ_2-3 y_univ_2-4 y_univ_2-5 y_univ_2-6 y_univ_2-7 y_univ_2-8 ) ) (domain --Null-0 --Null-1 --Null-2 --Null-3 --Null-4 --Null-5 --Null-6 --Null-7 --Null-8 ) ) ) ) )

(check-sat)