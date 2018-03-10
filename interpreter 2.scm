
(load "simpleParser.scm")
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
      ;((null? (cdr state)) (return 'undefinedVar))
      ((or (null? (lowScopeVariablesOf state))(null? (lowScopeVariable state))) (M_lookup-cps var (expandScope state) return))
      ((equal? var (stateVar state)) (return (stateVal state)))
      (else (M_lookup-cps var (restOf state) return)))))

; Determines if the lowest scope sections of the state contains variables
(define lowScopeVariablesOf
  (lambda (state)
    (car state)))

; Determines if the lowest scope states first variable exists
(define lowScopeVariable
  (lambda (state)
    (caar state)))

; Returns the variable (var) of the first element in the given state
(define stateVar
  (lambda (state)
    (caaar state)))

; Returns the vaule of a component within the state
(define stateVal
  (lambda (state)
    (cadaar state)))

; Gives the rest of the state following the first (variable value)
(define restOf
  (lambda (state)
    (cons (cdar state) (cdr state))))

; Returns the larger scope
(define expandScope
  (lambda (state)
    (cdr state)))

; Removes a (variable value) from the state 
(define M_remove-cps
  (lambda (var state return)
    (cond
      ((null? state) (error 'undefined))
      ((null? (expandScope state))  (M_remove_helper-cps var (lowScopeVariablesOf state) (lambda (v) (return (list v))))) 
      (else (M_remove_helper-cps var (lowScopeVariablesOf state) (lambda (v1) (M_remove-cps var (cdr state) (lambda (v2) (return (cons v1 v2)))))))
      )))

(define M_remove_helper-cps
  (lambda (var state return)
    (cond
      ((null? state) (return '()))
      ((equal? var (lowScopeVariable state)) (return (expandScope state)))
      (else (M_remove_helper-cps var (expandScope state) (lambda (v) (return (cons (lowScopeVariablesOf state) v))))))))

; Performs a nested remove and add to repalce the value of a variable (deep add)
(define M_replace-cps
  (lambda (var state newValue return)
    (cond
      ((null? state) (error 'undefined))
      ((null? (expandScope state))  (M_replace_helper-cps var (lowScopeVariablesOf state) newValue (lambda (v) (return (list v))))) 
      (else (M_replace_helper-cps var (lowScopeVariablesOf state) newValue (lambda (v1) (M_replace-cps var (expandScope state) newValue (lambda (v2) (return (cons v1 v2)))))))
      )))

(define M_replace_helper-cps
  (lambda (var state newValue return)
    (cond
      ((null? state) (return '()))
      ((equal? var (lowScopeVariable state)) (return (cons (list var newValue) (expandScope state))))
      (else (M_replace_helper-cps var (expandScope state) newValue (lambda (v) (return (cons (lowScopeVariablesOf state) v))))))))

; Gives the first (variable value) in the state
(define firstElementOf
  (lambda (state)
    (caar state)))

; Adds the (variable value) to the state
(define M_add
  (lambda (var value state)
    (cons (cons (list var value) (startOfGlobal state)) (restOfGlobal state))))

; Gets the start of the state
(define startOfGlobal
  (lambda (state)
    (car state)))

; Gets the rest of the state
(define restOfGlobal
  (lambda (state)
    (cdr state)))

; Assigns a value to a variable
(define M_state_assign-cps
  (lambda (var value state return)
    (cond
      ((equal? (m_value_int-cps (getValue value) state return) 'undefinedVar) (error 'undefined "Using before declaring"))
      ((M_lookup-cps var state (lambda (v1) (not (equal? v1 'undefinedVar))))(m_value_int-cps (getValue value) state (lambda (v2) (M_replace-cps var state v2 return))));(M_remove-cps var state (lambda (v3) (return (M_add var v2 v3))))))) 
      ((M_lookup-cps var state (lambda (v1) (and (equal? v1 'undefined) (isBooleanExpression? (getValue value))))) (m_value_boolean-cps (getValue value) state (lambda (v2) (M_replace-cps var state v2 return))));(M_remove-cps var state (lambda (v3) (return M_add var v2 v3))))))
      ((M_lookup-cps var state (lambda (v1) (equal? v1 'undefined))) (m_value_int-cps (getValue value) state (lambda (v2) (M_replace-cps var state v2 return))));(M_remove-cps var state (lambda (v3) (return (M_add var v2 v3)))))))
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
      ((and (list? (expressionOf input))(not(null? (valueOfBoolean input)))(isBooleanExpression? (booleanExpressionOf input))) (m_value_boolean-cps (expressionOf input) state (lambda (v) (return (M_add (variableOf input) v state)))))
      (else (m_value_int-cps (expressionOf input) state (lambda (v) (return (M_add (variableOf input) v state))))))))

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
  (lambda (cond then else state return break continueWhile breakWhile throw)
    (if (m_value_boolean-cps cond state (lambda (v) v))
        (M_state_stmt-cps then state return break continueWhile breakWhile throw)
        (M_state_stmt-cps else state return break continueWhile breakWhile throw))))


; Determines the state of a while loop
(define M_state_while-cps
  (lambda (condition body state return throw)
    (cond
      ((m_value_boolean-cps condition state (lambda (v) v))
       (call/cc
        (lambda (breakWhile)
          (M_state_while-cps condition body
                             (call/cc
                              (lambda (continueWhile)
                                (M_state_stmt-cps body state (lambda (v) v) (lambda (v2) v2) continueWhile breakWhile throw))) return throw))))
      (else (return state))
      )))

; Determines the value of the return statement
(define M_state_return-cps
  (lambda (statement state return)
    (if (equal? (M_lookup-cps 'returnVal state return) 'undefinedVar)
        (M_state_return_helper-cps statement state (lambda (v) (return (M_add_global 'returnVal v state))))
        state)))

(define M_state_return_helper-cps
  (lambda (statement state return)
    (cond
      ((number? statement) (return statement))
      ((boolean? statement) (return (booleanToText statement)))
      ((or(equal? statement 'true) (equal? statement 'false)) (m_value_boolean-cps (equal? statement 'true) state (lambda (v) (return (booleanToText v)))))
      ((M_lookup-cps statement state (lambda (v1) (and (not (equal? v1 'undefinedVar)) (boolean? v1)))) (M_lookup-cps statement state (lambda (v2) (return (booleanToText v2)))))
      ((M_lookup-cps statement state (lambda (v1) (not (equal? v1 'undefinedVar)))) (M_lookup-cps statement state return))
      ((and (not (list? statement))(M_lookup-cps statement state (lambda (v1) (equal? v1 'undefinedVar)))) (error 'undefined "Variable not declared"))
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

; Returns state of block
(define M_state_block-cps
  (lambda (stmt-list state return break continueWhile breakWhile throw)
    (M_state_stmt_list-cps stmt-list (cons '() state) return break continueWhile breakWhile throw)))

; Adds global variable
(define M_add_global
  (lambda (var value state)
    (if (null? (globalRestOf state))
        (M_add var value state)
        (cons (firstOfGlobal state) (M_add_global var value (globalRestOf state))))))

; Returns the begining of the state
(define firstOfGlobal
  (lambda (state)
    (car state)))

; Returns the rest of the state
(define globalRestOf
  (lambda (state)
    (cdr state)))

; Throw state
(define M_throw
  (lambda (statement state)
    (cond
      ((or (eq? 'true (throwValue statement))
           (eq? 'false (throwValue statement))
           (number? (throwValue statement)))
       (throwValue statement))
      (else
       (M_lookup-cps (throwValue statement) state (lambda (v) v))))))
; Returns the throw value
(define throwValue
  (lambda (statement)
    (cadr statement)))

; Executes the try catch finally of a try statement
(define M_try 
  (lambda (statement_list state return break continueWhile breakWhile throw)
    (call/cc
     (lambda (throw)
       (M_state_stmt_list-cps statement_list state return break continueWhile breakWhile throw)))))

(define M_catch
  (lambda (stmtlist state return break continueWhile breakWhile throw)
       (M_state_stmt_list-cps stmtlist state return break continueWhile breakWhile throw)))


; Determines what state should be called next
(define M_state_stmt-cps 
  (lambda (statement state return break continueWhile breakWhile throw)
    (cond
      ((null? statement) (return state))
      ((equal? 'var (stateOperator statement)) (M_state_declare-cps (variableOfDeclare statement) state return))
      ((equal? '= (stateOperator statement)) (M_state_assign-cps (variableOfAssign statement) (valueOfAssign statement) state return))
      ((and (equal? 'if (stateOperator statement))(pair?(ifThen statement))) (M_state_if-cps (ifCondition statement) (statement1 statement) (statement2 statement) state return break continueWhile breakWhile throw)) ; If Then statement
      ((equal? 'if (stateOperator statement)) (M_state_if-cps (ifCondition statement) (statement1 statement) (emptyList) state return break continueWhile breakWhile throw))                                      ; If statement
      ((equal? 'while (stateOperator statement)) (M_state_while-cps (whileCondition statement) (bodyOfWhile statement) state return throw))
      ((equal? 'begin (stateOperator statement))  (cdr (M_state_block-cps (cdr statement) state return break continueWhile breakWhile throw)));CPS needs to be addressed
      ((equal? 'continue (stateOperator statement)) (continueWhile state))
      ((equal? 'break (stateOperator statement)) (breakWhile (cdr state)))
      ((equal? 'throw (stateOperator statement)) (throw (M_add 'thrown (M_throw statement state) state)))
      ((equal? 'finally (stateOperator statement)) (M_state_stmt_list-cps (getFinallyBodyOf statement) state return break continueWhile breakWhile throw))
      ((equal? 'catch (stateOperator statement)) (if (not (equal? 'undefinedVar (M_lookup-cps 'thrown state (lambda (v)v))))
                                                     (M_catch (getCatchBodyOf statement) (M_add (getEValueOf statement) (M_lookup-cps 'thrown state (lambda (v)v)) (M_remove-cps 'thrown state (lambda (v)v))) return break continueWhile breakWhile throw)
                                                     state))
      ((equal? 'try (stateOperator statement)) (M_state_stmt_list-cps (getNotTryBodyOf statement) (call/cc
                                                (lambda (throw)
                                                  (M_try (getTryBodyOf statement) state return break continueWhile breakWhile throw))) return break continueWhile breakWhile throw))
      (else (equal? 'return (stateOperator statement)) (break (M_state_return-cps (valueOfReturn statement) state return))))))

; Gets the non-try part of the try catch statement
(define getNotTryBodyOf
  (lambda (statement)
    (cddr statement)))

; Retreives the body of the finally statement
(define getFinallyBodyOf
  (lambda (statement)
    (cadr statement)))

; Retreives the body of the catch statement
(define getCatchBodyOf
  (lambda (statement)
    (caddr statement)))

; Gets the 'e' value for the catch statement
(define getEValueOf
  (lambda (statement)
    (caadr statement)))

; Retreives the body of the try statement
(define getTryBodyOf
  (lambda (statement)
    (cadr statement)))

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
(define M_state_stmt_list-cps
  (lambda (slist s return break continueWhile breakWhile throw)
       (if (null? slist)
           s
           (M_state_stmt_list-cps (restOfStatementList slist) (M_state_stmt-cps (firstStatementOf slist) s (lambda (returnValue) returnValue) break continueWhile breakWhile throw) return break continueWhile breakWhile throw))))

; Gets all statements in the statements list following the first one
(define restOfStatementList
  (lambda (slist)
    (cdr slist)))

; Gets the first statement from the statement list
(define firstStatementOf
  (lambda (slist)
    (car slist)))

; INterprets the text file assuming it is written in Java style
(define interpret
  (lambda (filename)
    (M_lookup-cps 'returnVal (call/cc (lambda (returnBreak) (M_state_stmt_list-cps (parser filename) '(()) (lambda (v) v) returnBreak (lambda (cw) cw) (lambda (bw) bw) (lambda (th) th)))) (lambda (v) v))))