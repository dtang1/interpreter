#lang racket
;(load "simpleParser.scm")
; m_value_int

; Calculates the mathematical value of an expression
(define m_value_int-cps
  (lambda (input state return)
    (cond
      ((number? input) (return input))
      ((M_lookup-cps input state (lambda (v) (and (eq? v 'undefined) (not(list? input))))) (error 'undefined "Using before assigning"))
      ((not(list? input)) (M_lookup-cps input state return))
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
(define m_value_boolean-cps
  (lambda (condition state return)
    (cond
      ((boolean? condition) (return condition))
      ((or (equal? condition 'true) (equal? condition 'false)) (return (equal? 'true condition)))
      ((not(list? condition)) (M_lookup-cps condition state return))
      ((equal? (operator condition) '<)
       (m_value_int-cps (operand1 condition) state
                        (lambda (v1) (m_value_int-cps (operand2 condition) state
                                                      (lambda (v2) (return (< v1 v2)))))))
      ((equal? (operator condition) '<=)
       (m_value_int-cps (operand1 condition) state
                        (lambda (v1) (m_value_int-cps (operand2 condition) state
                                                      (lambda (v2) (return (<= v1 v2)))))))
      ((equal? (operator condition) '>)
       (m_value_int-cps (operand1 condition) state
                        (lambda (v1) (m_value_int-cps (operand2 condition) state
                                                      (lambda (v2) (return (> v1 v2)))))))
      ((equal? (operator condition) '>=)
       (m_value_int-cps (operand1 condition) state
                        (lambda (v1) (m_value_int-cps (operand2 condition) state
                                                      (lambda (v2) (return (>= v1 v2)))))))
      ((equal? (operator condition) '==)
       (m_value_int-cps (operand1 condition) state
                        (lambda (v1) (m_value_int-cps (operand2 condition) state
                                                      (lambda (v2) (return (equal? v1 v2)))))))
      ((equal? (operator condition) '!=)
       (m_value_int-cps (operand1 condition) state
                        (lambda (v1) (m_value_int-cps (operand2 condition) state
                                                      (lambda (v2) (return (not (equal? v1 v2))))))))
      ((equal? (operator condition) '!)
       (m_value_boolean-cps (operand1 condition) state (lambda (v) (return (not v)))))
      ((equal? (operator condition) '||)
       (m_value_boolean-cps (operand1 condition) state
                            (lambda (v1) (m_value_boolean-cps (operand2 condition) state
                                                              (lambda (v2) (return (or v1 v2)))))))
      ((equal? (operator condition) '&&)
       (m_value_boolean-cps (operand1 condition) state
                            (lambda (v1) (m_value_boolean-cps (operand2 condition) state
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
(define M_remove-cps
  (lambda (var state return)
    (cond
      ((null? state) (error 'undefined))
      ((equal? var (stateVar state)) (return (restOf state)))
      (else (M_remove-cps var (restOf state) (lambda (v) (return (cons (firstElementOf state) v))))))))

; Gives the first (variable value) in the state
(define firstElementOf
  (lambda (state)
    (car state)))

; Adds the (variable value) to the state
(define M_add
  (lambda (var value state)
    (cons (list var value) state)))

; Assigns a value to a variable
(define M_state_assign-cps
  (lambda (var value state return)
    (cond
      ((equal? (m_value_int-cps (getValue value) state return) 'undefinedVar) (error 'undefined "Using before declaring"))
      ((M_lookup-cps var state (lambda (v1) (not (equal? v1 'undefinedVar))))(m_value_int-cps (getValue value) state (lambda (v2) (M_remove-cps var state (lambda (v3) (return (M_add var v2 v3))))))) 
      ((M_lookup-cps var state (lambda (v1) (and (equal? v1 'undefined) (isBooleanExpression? (getValue value))))) (m_value_boolean-cps (getValue value) state (lambda (v2) (M_remove-cps var state (lambda (v3) (return M_add var v2 v3))))))
      ((M_lookup-cps var state (lambda (v1) (equal? v1 'undefined))) (m_value_int-cps (getValue value) state (lambda (v2) (M_remove-cps var state (lambda (v3) (return (M_add var v2 v3)))))))
      (else (error 'undefined "Using before declaring")))))

; Gets the value that will be assigned to the variable
(define getValue
  (lambda (value)
    (car value)))

; Declares a variable and assigns it the given value or to 'undefined' if no value is given
(define M_state_declare-cps
  (lambda (input state return)
    (cond
      ((null? (valueOf input)) (return (M_add (variableOf input) 'undefined state)))
      ((M_lookup-cps (variableOf input) state (lambda (v1) (and (not (equal? v1 'undefinedVar))(not(equal? v1 'undefined)))))(error 'badoperation "Attempting to redefine already defined variable"))
<<<<<<< HEAD
      ((and (list? (expressionOf input))(not(null? (valueOfBoolean input)))(isBooleanExpression? (booleanExpressionOf input))) (m_value_boolean-cps (expressionOf input) state (lambda (v) (return (M_add v state)))))
      (else (m_value_int-cps (expressionOf input) state (lambda (v) (return (M_add (variableOf input) v state))))))))
=======
      ((and (list? (expressionOf input))(not(null? (valueOfBoolean input)))(isBooleanExpression? (booleanExpressionOf input))) (m_value_boolean-cps (expressionOf input) state (lambda (v) (return (M_add (variableOf input) v state)))))
      (else (m_value_int-cps (expressionOf input) state (lambda (v) (return (M_add (variableOf input) v state))))))));
>>>>>>> 2f22b31ac34b678dc50ddbae297f95a8935838d6

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
(define M_state_if-cps
  (lambda (cond then else state return)
<<<<<<< HEAD
    (if (m_value_boolean-cps cond state return)
=======
    (if (m_value_boolean-cps cond state (lambda (v) v))
>>>>>>> 2f22b31ac34b678dc50ddbae297f95a8935838d6
        (M_state_stmt-cps then state return)
        (M_state_stmt-cps else state return))))

; Determines the state of a while loop
(define M_state_while
  (lambda (cond body state)
    (if (m_value_boolean-cps cond state (lambda (v) v))
        (M_state_while cond body (M_state_stmt-cps body state (lambda (v) v)))
        state)))

; Determines the value of the return statement
(define M_state_return-cps
  (lambda (statement state return)
    (cond
      ((number? statement) (return statement))
      ((boolean? statement) (return (booleanToText statement)))
      ((or(equal? statement 'true) (equal? statement 'false)) (m_value_boolean-cps (equal? statement 'true) state (lambda (v) (return (booleanToText v)))))
      ((M_lookup-cps statement state (lambda (v1) (and (not (equal? v1 'undefinedVar)) (boolean? v1)))) (M_lookup-cps statement state (lambda (v2) (return (booleanToText v2)))))
      ((M_lookup-cps statement state (lambda (v1) (not (equal? v1 'undefinedVar)))) (M_lookup-cps statement state return))
      ((isExpression? (expressionOperand statement)) (m_value_int-cps statement state return))
      ((isBooleanExpression? (booleanOperand statement)) (m_value_boolean-cps statement state (lambda (v) (return (booleanToText v)))))
      (else (error "invalid return statement"))
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
(define M_state_stmt-cps
  (lambda (statement state return)
    (cond
      ((null? statement) (return state))
      ((equal? 'var (stateOperator statement)) (M_state_declare-cps (variableOfDeclare statement) state return))
      ((equal? '= (stateOperator statement)) (M_state_assign-cps (variableOfAssign statement) (valueOfAssign statement) state return))
      ((and (equal? 'if (stateOperator statement))(pair?(ifThen statement))) (M_state_if-cps (ifCondition statement) (statement1 statement) (statement2 statement) state return)) ; If Then statement
      ((equal? 'if (stateOperator statement)) (M_state_if-cps (ifCondition statement) (statement1 statement) (emptyList) state return))                                      ; If statement
      ((equal? 'while (stateOperator statement)) (M_state_while (whileCondition statement) (bodyOfWhile statement) state))
      ((equal? 'return (stateOperator statement)) (M_state_return-cps (valueOfReturn statement) state return)))))

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
        (M_state_stmt_list (cdr slist) (M_state_stmt-cps (car slist) s (lambda (v) v))))))
