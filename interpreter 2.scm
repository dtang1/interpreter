#lang racket
;(load "simpleParser.scm")
; m_value_int

; Calculates the mathematical value of an expression
;(define m_value_int
;  (lambda (input state)
;    (cond
;      ((number? input) input)
;      ((and (not(list? input)) (equal? (M_lookup-cps input state) 'undefined)) (error 'undefined "Using before assigning"))
;      ((not(list? input)) (M_lookup-cps input state))
;      ((equal? (operator input) '+) (+ (m_value_int (operand1 input) state) (m_value_int (operand2 input)state)))
;      ((and (equal? (operator input) '-) (null? (urnarycheck input))) (* -1 (m_value_int (operand1 input) state)))
;      ((equal? (operator input) '-) (- (m_value_int (operand1 input)state) (m_value_int (operand2 input)state)))
;      ((equal? (operator input) '*) (* (m_value_int (operand1 input)state) (m_value_int (operand2 input)state)))
;      ((equal? (operator input) '/) (quotient (m_value_int (operand1 input)state) (m_value_int (operand2 input)state)))
;      ((equal? (operator input) '%) (remainder (m_value_int (operand1 input)state) (m_value_int (operand2 input)state)))
;      (else (error 'badop "Undefined operator")))))

(define m_value_int-cps
  (lambda (input state return)
    (cond
      ((number? input) (return input))
      ((M_lookup-cps input state (lambda (v) (return (and (eq? v 'undefined) (not(list? input)))))) (error 'undefined "Using before assigning"))
      ((not(list? input)) (M_lookup-cps input state (lambda (v) v)))
      ((equal? (operator input) '+)
       (m_value_int-cps (operand1 input) state
                        (lambda (v1) (m_value_int-cps (operand2 input) state
                                                      (lambda (v2) (return (+ v1 v2)))))))
      ((and (equal? (operator input) '-) (null? (urnarycheck input)))
       (m_value_int-cps (operand1 input) state
                        (lambda (v) (return (* -1 v)))))
      ((equal? (operator input) '-)
       (m_value_int-cps (operand1 input) state
                        (lambda (v1) (m_value_int-cps (operand2 input) state
                                                      (lambda (v2) (return (- v1 v2)))))))
      ((equal? (operator input) '*)
       (m_value_int-cps (operand1 input) state
                        (lambda (v1) (m_value_int-cps (operand2 input) state
                                                      (lambda (v2) (return (* v1 v2)))))))
      ((equal? (operator input) '/)
       (m_value_int-cps (operand1 input) state
                        (lambda (v1) (m_value_int-cps (operand2 input) state
                                                      (lambda (v2) (return (quotient v1 v2)))))))
      ((equal? (operator input) '%)
       (m_value_int-cps (operand1 input) state
                        (lambda (v1) (m_value_int-cps (operand2 input) state
                                                      (lambda (v2) (return (remainder v1 v2)))))))
      (else (error 'badop "Undefined operator")))))

; Gets the operator of an expression
(define operator
  (lambda (e)
    (car e)))

; Gets the first operand of the expression
(define operand1 cadr)

; Gets the second operand of the expression
(define operand2 caddr)

; Checks if there is a second operand, meant for - urnary operator
(define urnarycheck cddr)

; Calculates the boolean value of an expression
(define m_value_boolean_cps
  (lambda (condition state return)
    (cond
      ((boolean? condition) (return condition))
      ((or (equal? condition 'true) (equal? condition 'false)) (return (equal? 'true condition)))
      ((not(list? condition)) (return (M_lookup-cps condition state)))
      ((equal? (operator condition) '<)
       (m_value_boolean_cps condition (m_value_int-cps (operand1 condition) state (lambda (v) v))
                            (lambda (v1) (m_value_boolean_cps condition (m_value_int-cps (operand2 condition) state (lambda (v) v))
                                                              (lambda (v2) (return (< v1 v2)))))))
      ((equal? (operator condition) '<=)
       (m_value_boolean_cps condition (m_value_int-cps (operand1 condition) state (lambda (v) v))
                            (lambda (v1) (m_value_boolean_cps condition (m_value_int-cps (operand2 condition) state (lambda (v) v))
                                                              (lambda (v2) (return (<= v1 v2)))))))
      ((equal? (operator condition) '>)
       (m_value_boolean_cps condition (m_value_int-cps (operand1 condition) state (lambda (v) v))
                            (lambda (v1) (m_value_boolean_cps condition (m_value_int-cps (operand2 condition) state (lambda (v) v))
                                                              (lambda (v2) (return (> v1 v2)))))))
      ((equal? (operator condition) '>=)
       (m_value_boolean_cps condition (m_value_int-cps (operand1 condition) state (lambda (v) v))
                            (lambda (v1) (m_value_boolean_cps condition (m_value_int-cps (operand2 condition) state (lambda (v) v))
                                                              (lambda (v2) (return (>= v1 v2)))))))
      ((equal? (operator condition) '==)
       (m_value_boolean_cps condition (m_value_int-cps (operand1 condition) state (lambda (v) v))
                            (lambda (v1) (m_value_boolean_cps condition (m_value_int-cps (operand2 condition) state (lambda (v) v))
                                                              (lambda (v2) (return (equal? v1 v2)))))))
      ((equal? (operator condition) '!=)
       (m_value_boolean_cps condition (m_value_int-cps (operand1 condition) state (lambda (v) v))
                            (lambda (v1) (m_value_boolean_cps condition (m_value_int-cps (operand2 condition) state (lambda (v) v))
                                                              (lambda (v2) (return (not(equal? v1 v2))))))))
      ((equal? (operator condition) '!)
       (m_value_boolean_cps (operand1 condition) state (lambda (v) (return (not v)))))
      ((equal? (operator condition) '||)
       (m_value_boolean_cps (operand1 condition) state
                            (lambda (v1) (m_value_boolean_cps (operand2 condition) state
                                                              (lambda (v2) (return (or v1 v2)))))))
      ((equal? (operator condition) '&&)
       (m_value_boolean_cps (operand1 condition) state
                            (lambda (v1) (m_value_boolean_cps (operand2 condition) state
                                                              (lambda (v2) (return (and v1 v2)))))))
      (else (error 'badop "Undefined operator")))))

; Finds the given variabl within the state
(define M_lookup-cps
  (lambda (var state return)
    (cond
      ((null? state) (return 'undefinedVar))
      ((equal? var (stateVar state)) (return (stateVal state)))
      (else (M_lookup-cps var (restOf state) return)))))

; Returns the variable (var) of the first element in the given state
(define stateVar
  (lambda (state)
    (caar state)))

; Returns the vaule of a component within the state
(define stateVal
  (lambda (state)
    (cadar state)))

; Gives the rest of the state following the first (variable value)
(define restOf
  (lambda (state)
    (cdr state)))

; Removes a (variable value) from the state 
(define M_remove
  (lambda (var state)
    (cond
      ((null? state) (error 'undefined))
      ((equal? var (stateVar state)) (restOf state))
      (else (cons (firstElementOf state) (M_remove var (restOf state)))))))

; Gives the first (variable value) in the state
(define firstElementOf
  (lambda (state)
    (car state)))

; Adds the (variable value) to the state
(define M_add
  (lambda (var value state)
    (cons (list var value) state)))

; Assigns a value to a variable
(define M_state_assign
  (lambda (var value state)
    (cond
      ((equal? (m_value_int-cps (getValue value) state) 'undefinedVar) (error 'undefined "Using before declaring"))
      ((not (equal? (M_lookup-cps var state) 'undefinedVar)) (M_add var (m_value_int-cps (getValue value) state) (M_remove var state)))
      ((and (equal? (M_lookup-cps var state) 'undefined) (or (isBooleanExpression? (getValue value)))) (M_add var (m_value_boolean_cps (getValue value) state) (M_remove var state)))
      ((equal? (M_lookup-cps var state) 'undefined)(M_add var (m_value_int-cps (getValue value) state) (M_remove var state)))
      (else(error 'undefined "Using before declaring")))))

; Gets the value that will be assigned to the variable
(define getValue
  (lambda (value)
    (car value)))

; Declares a variable and assigns it the given value or to 'undefined' if no value is given
(define M_state_declare
  (lambda (input state)
    (cond
      ((null? (valueOf input)) (M_add (variableOf input) 'undefined state))
      ((and (not (equal? 'undefinedVar (M_lookup-cps (variableOf input) state)))(not (equal? 'undefined (M_lookup-cps (variableOf input) state)))) (error 'badoperation "Attempting to redefine already defined variable"))
      ((and (list? (expressionOf input))(not(null? (valueOfBoolean input)))(isBooleanExpression? (booleanExpressionOf input))) (M_add (variableOf input) (m_value_int-cps(expressionOf input)) state))
      (else (M_add (variableOf input) (m_value_int-cps (expressionOf input) state) state)))))

; Returns the boolean expression of the statement, if there is one
(define booleanExpressionOf
  (lambda (statement)
    (caadr statement)))

; Returns assignment value of input, if there is one
(define valueOfBoolean
  (lambda (statement)
    (caadr statement)))

; Returns the value of the declarative statement, if there is one
(define valueOf
  (lambda (input)
    (cdr input)))

; Returns the variable of the declaration statement
(define variableOf
  (lambda (input)
    (car input)))

; Returns the expression of the input
(define expressionOf
  (lambda (input)
    (cadr input)))

; Determines the state of an if statement
(define M_state_if
  (lambda (cond then else state)
    (if (m_value_int-cps cond state)
        (M_state_stmt then state)
        (M_state_stmt else state))))

; Determines the state of a while loop
(define M_state_while
  (lambda (cond body state)
    (if (m_value_int-cps cond state)
        (M_state_while cond body (M_state_stmt body state))
        state)))

; Determines the value of the return statement
(define M_state_return
  (lambda (statement state)
    (cond
      ((number? statement) statement)
      ((boolean? statement) (booleanToText statement))
      ((or(equal? statement 'true) (equal? statement 'false)) (booleanToText (m_value_int-cps (equal? statement 'true) state)))
      ((and (not (equal? (M_lookup-cps statement state) 'undefinedVar)) (boolean? (M_lookup-cps statement state))) (booleanToText (M_lookup-cps statement state)))
      ((not (equal? (M_lookup-cps statement state) 'undefinedVar)) (M_lookup-cps statement state))
      ((isExpression? (expressionOperand statement)) (m_value_int-cps statement state))
      ((isBooleanExpression? (booleanOperand statement)) (booleanToText (m_value_int-cps statement state)))
      (else 'error "invalid return statement")
      )))

; Retunr the boolean operand of the statement
(define booleanOperand
  (lambda (statement)
    (car statement)))

; Returns the expression operand of the statement
(define expressionOperand
  (lambda (statement)
    (car statement)))

; Determines what state should be called next
(define M_state_stmt
  (lambda (statement state)
    (cond
      ((null? statement) state)
      ((equal? 'var (stateOperator statement)) (M_state_declare (variableOfDeclare statement) state))
      ((equal? '= (stateOperator statement)) (M_state_assign (variableOfAssign statement) (valueOfAssign statement) state))
      ((and (equal? 'if (stateOperator statement))(pair?(ifThen statement))) (M_state_if (ifCondition statement) (statement1 statement) (statement2 statement) state)) ; If Then statement
      ((equal? 'if (stateOperator statement)) (M_state_if (ifCondition statement) (statement1 statement) (emptyList) state))                                      ; If statement
      ((equal? 'while (stateOperator statement)) (M_state_while (whileCondition statement) (bodyOfWhile statement) state))
      ((equal? 'return (stateOperator statement)) (M_state_return (valueOfReturn statement) state)))))

; Gives the value or expression of the return statement
(define valueOfReturn
  (lambda (statement)
    (cadr statement)))

; Returns the body of the while loop
(define bodyOfWhile
  (lambda (statement)
    (caddr statement)))

; Returns the condition of the while loop
(define whileCondition
  (lambda (statement)
    (cadr statement)))

; Returns the second statement of the If statement, if there is one
(define statement2
  (lambda (statement)
    (cadddr statement)))

; Returns the first statement of the If statement
(define statement1
  (lambda (statement)
    (caddr statement)))

; Returns the condition of the If statement
(define ifCondition
  (lambda (statement)
    (cadr statement)))

; Returns the "Then" part of the If statement if there is one
(define ifThen
  (lambda (statement)
    (cdddr statement)))

; Returns the variable of the assignment statement
(define variableOfAssign
  (lambda (statement)
    (cadr statement)))

; Returns the value of the assignement statement
(define valueOfAssign
  (lambda (statement)
    (cddr statement)))

; Returns the variable to be declared for the statement
(define variableOfDeclare
  (lambda (statement)
    (cdr statement)))

; Returns an empty lis
(define emptyList
  (lambda ()
      '()))

; Determines the operator at the beginning of a state
(define stateOperator
  (lambda (statement)
    (car statement)))

; Determines if the given statement is a mathermatical expression
(define isExpression?
  (lambda (operator)
    (or (equal? operator '+) (equal? operator '-) (equal? operator '*)(equal? operator '/)(equal? operator '%))))

; Determines if the given statement is boolean expression
(define isBooleanExpression?
  (lambda (operator)
    (or (boolean? operator) (equal? operator '>) (equal? operator '<)(equal? operator '>=) (equal? operator '<=) (equal? operator '==) (equal? operator '!=) (equal? operator '||) (equal? operator '&&))))    

; Converts the #t and #f to 'true and 'false
(define booleanToText
  (lambda (input)
    (if input
        'true
        'false)))

; Breaks up the given parse list into smaller statements to be executed
(define M_state_stmt_list
  (lambda (slist s)
    (if (null? slist)
        s
        (M_state_stmt_list (cdr slist) (M_state_stmt (car slist) s)))))