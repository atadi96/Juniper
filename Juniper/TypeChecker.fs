﻿module TypeChecker
module A = Ast
module T = TypedAst
open Constraint
open Extensions
open QuikGraph.Algorithms
open ConvertAst
open Error
open AstAnalysis
open Module
open TypedAst
open System.Threading

let rec typeof ((posE, e) : Ast.PosAdorn<Ast.Expr>)
               (dtenv : Map<string * string, T.DeclarationTy>)
               (menv : Map<string, string*string>)
               (localVars : Set<string>)
               (ienv : Map<string * string, int>)
               (tyVarMapping : Map<string, T.TyExpr>)
               (capVarMapping : Map<string, T.CapacityExpr>)
               // First bool represents mutability
               (gamma : Map<string, bool * T.TyScheme>) =
    let getTypes = List.map T.getType

    let convertType' = convertType menv dtenv tyVarMapping capVarMapping
    let convertCapacity' = convertCapacity capVarMapping

    // Taus is what the overall pattern's type should equal
    let rec checkPattern (posp, p) tau =
        let mutable gamma' = gamma
        let mutable localVars = Set.empty
        let rec checkPattern' (posp, p) tau =
            let rec checkPatterns pats =
                match pats with
                | [] -> ([], Trivial)
                | (p, tau)::ps ->
                    let (p', c) = checkPattern' p tau
                    let (ps', c') = checkPatterns ps
                    (p'::ps', c &&& c')
            match p with
            | Ast.MatchTuple (_, pats) ->
                let innerTaus = List.map freshtyvar pats
                let c = T.ConApp (T.TyCon T.TupleTy, innerTaus, []) =~= (tau, errStr [posp] "Tuple pattern does not match the expression.")
                let (pats', c') = checkPatterns (List.zip pats innerTaus)
                ((posp, tau, T.MatchTuple pats'), c &&& c')
            | Ast.MatchFalse _ ->
                ((posp, tau, T.MatchFalse), T.booltype =~= (tau, errStr [posp] "False pattern does not match the type of the expression."))
            | Ast.MatchTrue _ ->
                ((posp, tau, T.MatchTrue), T.booltype =~= (tau, errStr [posp] "True pattern does not match the type of the expression."))
            | Ast.MatchFloatVal (_, value) ->
                ((posp, tau, T.MatchFloatVal value), InterfaceConstraint (tau, IsReal, errStr [posp] "Float pattern must satisfy the interface real constraint. Are you sure that you're matching on a real number (float or double)?"))
            | Ast.MatchIntVal (_, value) ->
                ((posp, tau, T.MatchIntVal value), InterfaceConstraint (tau, IsNum, errStr [posp] "Integer pattern must satisfy the interface num constraint. Are you sure that you're matching on a number?"))
            | Ast.MatchUnderscore _ ->
                ((posp, tau, T.MatchUnderscore), Trivial)
            | Ast.MatchUnit (posu, _) ->
                ((posp, tau, T.MatchUnit), T.unittype =~= (tau, errStr [posu] "Unit pattern does not match the type of the expression."))
            | Ast.MatchVar { varName=(posv, varName); mutable_=(posm, mutable_); typ=typ } ->
                if Set.contains varName localVars then
                    raise <| TypeError' (errStr [posv] (sprintf "This pattern already contains a variable named %s." varName))
                else
                    localVars <- Set.add varName localVars
                    let (c', retTau) =
                        match typ with
                        | Some typ ->
                            let typ' = convertType' typ
                            gamma' <- Map.add varName (mutable_, T.Forall ([], [], [], typ')) gamma'
                            (tau =~= (typ', errStr [A.getPos typ] "Type constraint in pattern could not be satisfied"), typ')
                        | None ->
                            // NOTICE THAT WE DO NOT GENERALIZE HERE
                            // This is what makes this type system different from
                            // Hindley Milner
                            gamma' <- Map.add varName (mutable_, T.Forall ([], [], [], tau)) gamma'
                            (Trivial, tau)
                    ((posp, retTau, T.MatchVar { varName=varName; mutable_=mutable_; typ=tau}), c')
            | Ast.MatchRecCon (posf, fields) ->
                let fieldTaus = List.map freshtyvar fields
                let fieldConstraints =
                    List.zip fieldTaus fields |>
                    List.map
                        (fun (fieldTau, ((posn, name), _)) ->
                            InterfaceConstraint (tau, HasField (name, fieldTau), errStr [posn] (sprintf "Expected type to have a field named %s" name))) |>
                    conjoinConstraints
                let (pats', c) = checkPatterns (List.zip (List.map snd fields) fieldTaus)
                ((posp, tau, T.MatchRecCon (List.zip (List.map (fst >> A.unwrap) fields) pats')), fieldConstraints &&& c)
            | Ast.MatchValCon {name=(posn, decref); template=template; innerPattern=(posi, innerPattern)} ->
                let modQual =
                    match decref with
                    | Choice1Of2 name ->
                        if Set.contains name localVars then
                            raise <| TypeError' (errStr [posn] (sprintf "%s is a local variable and not a value constructor." name))
                        else
                            match Map.tryFind name menv with
                            | Some (mod_, name') ->
                                {T.ModQualifierRec.module_ = mod_; T.ModQualifierRec.name=name'}
                            | None ->
                                raise <| TypeError' (errStr [posn] (sprintf "Unable to find value constructor named %s." name))
                    | Choice2Of2 {module_ = mod_; name=name} ->
                        {T.ModQualifierRec.module_=Ast.unwrap mod_; T.ModQualifierRec.name=Ast.unwrap name}
                let {T.ModQualifierRec.module_=module_; T.ModQualifierRec.name=name} = modQual
                // Lookup a value constructor in dtenv
                match Map.tryFind (module_, name) dtenv with
                | Some (T.FunDecTy valueConstructor) ->
                    let id = Map.find (module_, name) ienv
                    // Value constructors do not currently allow interface constraints, so just ignore that field
                    let (inst, _) =
                        match template with
                        | Some (post, {tyExprs=(posm, tyExprs); capExprs=(posn, capExprs)}) ->
                            instantiate valueConstructor (List.map convertType' tyExprs) (List.map (Ast.unwrap >> convertCapacity') capExprs)
                        | None ->
                            let (instance, _, _) = freshInstance valueConstructor
                            instance
                    match inst with
                    | T.ConApp (T.TyCon T.FunTy, _::returnTau::argTaus, _) ->
                        if List.length argTaus = List.length innerPattern then
                            let c = returnTau =~= (tau, errStr [posn] "Value constructor pattern type does not match the expression.")
                            let (innerPattern'', cs) =
                                List.zip argTaus innerPattern |>
                                List.map
                                    (fun (valueConTau, innerPattern') ->
                                        checkPattern' innerPattern' valueConTau) |>
                                List.unzip
                            let c' = c &&& (conjoinConstraints cs)
                            ((posp, tau, T.MatchValCon {modQualifier=modQual; innerPattern=innerPattern''; id = id}), c')
                        else
                            raise <| TypeError' (errStr [posi] (sprintf "Value constructor named %s takes %d arguments, but there were %d inner patterns." name (List.length argTaus) (List.length innerPattern)))
                    | _ ->
                        raise <| TypeError' (errStr [posn] (sprintf "Found declaration named %s, but it wasn't a value constructor." name))
                | _ ->
                    raise <| TypeError' (errStr [posn] (sprintf "Unable to find value constructor named %s" name))
        let (pattern', c) = checkPattern' (posp, p) tau
        (pattern', c, localVars, gamma')
            
    and typesof exprs dtenv menv localVars gamma =
        match exprs with
        | [] -> ([], Trivial)
        | e::es ->
            let (tau, c) = typeof e dtenv menv localVars ienv tyVarMapping capVarMapping gamma
            let (taus, c') = typesof es dtenv menv localVars gamma
            (tau::taus, c &&& c')
    and ty ((posE, expr) : Ast.PosAdorn<Ast.Expr>) : (T.TyAdorn<T.Expr> * Constraint) =
        let adorn pos tau expr con =
            ((pos, tau, expr), con)
        match expr with
        | Ast.UnitExp (pos, ()) ->
            adorn posE T.unittype T.UnitExp Trivial
        | Ast.InlineCode (pos, code) ->
            adorn posE T.unittype (T.InlineCode code) Trivial
        | Ast.TrueExp (pos, ()) ->
            adorn posE T.booltype T.TrueExp Trivial
        | Ast.FalseExp (pos, ()) ->
            adorn posE T.booltype T.FalseExp Trivial
        | Ast.IntExp (pos, num) ->
            let tyVar = freshtyvar ()
            adorn posE tyVar (T.IntExp num) (InterfaceConstraint (tyVar, IsNum, errStr [pos] "Polymorphic integer literal must be constrained to a numeric type"))
        | Ast.Int8Exp (pos, num) ->
            adorn posE T.int8type (T.Int8Exp num) Trivial
        | Ast.Int16Exp (pos, num) ->
            adorn posE T.int16type (T.Int16Exp num) Trivial
        | Ast.Int32Exp (pos, num) ->
            adorn posE T.int32type (T.Int32Exp num) Trivial
        | Ast.Int64Exp (pos, num) ->
            adorn posE T.int64type (T.Int64Exp num) Trivial
        | Ast.UInt8Exp (pos, num) ->
            adorn posE T.uint8type (T.UInt8Exp num) Trivial
        | Ast.UInt16Exp (pos, num) ->
            adorn posE T.uint16type (T.UInt16Exp num) Trivial
        | Ast.UInt32Exp (pos, num) ->
            adorn posE T.uint32type (T.UInt32Exp num) Trivial
        | Ast.UInt64Exp (pos, num) ->
            adorn posE T.uint64type (T.UInt64Exp num) Trivial
        | Ast.FloatExp (pos, num) ->
            adorn posE T.floattype (T.FloatExp num) Trivial
        | Ast.DoubleExp (pos, num) ->
            adorn posE T.doubletype (T.DoubleExp num) Trivial
        | Ast.IfElseExp {condition=(posc, _) as condition; trueBranch=(post, _) as trueBranch; falseBranch=(posf, _) as falseBranch} ->
            let (exprs', c) = typesof [condition; trueBranch; falseBranch] dtenv menv localVars gamma
            let [condition'; trueBranch'; falseBranch'] = exprs'
            let [tauC; tauT; tauF] = getTypes exprs'
            let c' = c &&&
                        (tauC =~= (T.booltype, errStr [posc] "Condition of if statement expected to be type bool")) &&&
                        (tauT =~= (tauF, errStr [post; posf] "Branches of if statement expected to be of the same type"))
            adorn posE tauT (T.IfElseExp {condition=condition'; trueBranch=trueBranch'; falseBranch=falseBranch'}) c'
        | Ast.VarExp (posn, varName) ->
            match Map.tryFind varName gamma with
            | Some (_, tyscheme) ->
                let ((instance, interfaceConstraints), t, c) = freshInstance tyscheme
                let err = errStr [posn] "The interface constraints are not satisfied."
                let interfaceConstraints' = interfaceConstraints |> List.map (fun (conTau, con) -> InterfaceConstraint (conTau, con, err)) |> conjoinConstraints
                let nonfunVar = adorn posE instance (T.VarExp (varName, t, c)) interfaceConstraints'
                if Set.contains varName localVars then
                    nonfunVar
                else
                    match Map.find (Map.find varName menv) dtenv with
                    | FunDecTy _ ->
                        adorn posE instance (T.FunctionWrapperEmptyClosure (posE, instance, (T.VarExp (varName, t, c)))) interfaceConstraints'
                    | _ ->
                        nonfunVar
            | None ->
                raise <| TypeError' (errStr [posn] (sprintf "Variable named %s could not be found" varName))
        | Ast.ArrayAccessExp { array=(posa, _) as array; index=(posi, _) as index } ->
            let (exprs', c) = typesof [array; index] dtenv menv localVars gamma
            let [array'; index'] = exprs'
            let [tauA; tauI] = getTypes exprs'
            let tauElement = freshtyvar ()
            let arraySize = freshcapvar ()
            let tauArray = T.ConApp (T.TyCon T.ArrayTy, [tauElement], [arraySize])
            let c' = c &&& (tauA =~= (tauArray, errStr [posa] "An array access expression must access a value of an array type")) &&&
                           (InterfaceConstraint (tauI, IsInt, errStr [posi] "Expected index of array access expression to have integer type"))
            adorn posE tauElement (T.ArrayAccessExp {array=array'; index=index'}) c'
        | Ast.ArrayLitExp (posa, exprs) ->
            let (exprs', c) = typesof exprs dtenv menv localVars gamma
            let tauElement = freshtyvar ()
            let c' = List.fold (&&&) c (List.map (flip (T.getType >> (=~=)) (tauElement, errStr [posa] "Expected all elements of array to be of the same type")) exprs')
            let tauArray = T.ConApp (T.TyCon T.ArrayTy, [tauElement], [T.CapacityConst <| int64 (List.length exprs)])
            adorn posE tauArray (T.ArrayLitExp exprs') c'
        | Ast.ArrayMakeExp {typ=typ; initializer=maybeInitializer} ->
            let post = A.getPos typ
            let typ' = convertType' typ
            match typ' with
            | T.ConApp (T.TyCon T.ArrayTy, [tauElement], [cap]) ->
                let (maybeInitializer', c) =
                    match maybeInitializer with
                    | Some ((posi, _) as initializer) ->
                        let (initializer', c) = ty initializer
                        let c' = c &&& (T.getType initializer' =~= (tauElement, errStr [post; posi] "Expected initializer to have the same type as the type declaration."))
                        (Some initializer', c')
                    | None ->
                        (None, Trivial)
                adorn posE typ' (T.ArrayMakeExp {typ=typ'; initializer=maybeInitializer'}) c
            | _ ->
                raise <| TypeError' (errStr [post] "Type declaration should be an array type")
        | Ast.AssignExp {left=(posl, left); right=(posr, _) as right; ref=(posref, ref)} ->
            let rec checkLeft left =
                let ((_, taul, left'), c) =
                    match left with
                    | Ast.ModQualifierMutation (posmq, {module_=(posm, module_); name=(posn, name)}) ->                    
                        match Map.tryFind (module_, name) dtenv with
                        | Some (T.LetDecTy tau) ->
                            if ref then
                                // TODO: Update this if we decide to make module level values mutable
                                adorn posl tau (T.ModQualifierMutation {module_=module_; name=name}) Trivial
                            else
                                raise <| TypeError' (errStr [posmq] "Top level let declarations are not mutable. Did you mean to use 'set ref' instead?")
                        | Some _ ->
                            raise <| TypeError' (errStr [posn] (sprintf "Found a declaration named %s in module %s, but it was not a let declaration." name module_))
                        | None ->
                            raise <| TypeError' (errStr [posmq] (sprintf "Unable to find a let declaration named %s in module %s." name module_))
                    | Ast.ArrayMutation {array=(posa, array); index=(posi, _) as index} ->
                        let elementTau = freshtyvar ()
                        let capVar = freshcapvar ()
                        let (array', c1) = checkLeft array
                        let (index', c2) = ty index
                        let c' = c1 &&& c2 &&& (InterfaceConstraint (T.getType index', IsInt, errStr [posi] "Array index must be an integer type.")) &&&
                                               ((T.getType array') =~= (T.ConApp (T.TyCon T.ArrayTy, [elementTau], [capVar]), errStr [posa] "Expected an array type to perform an array mutation upon"))
                        adorn posl elementTau (T.ArrayMutation {array=T.unwrap array'; index=index'}) c'
                    | Ast.RecordMutation {record=(posr, record); fieldName=(posf, fieldName)} ->
                        let (record', c) = checkLeft record
                        let fieldTau = freshtyvar ()
                        let c' = c &&& InterfaceConstraint (T.getType record', HasField (fieldName, fieldTau), errStr [posE] (sprintf "Expected the expression to be a record type and have a field named %s" fieldName))
                        adorn posl fieldTau (T.RecordMutation {record=T.unwrap record'; fieldName=fieldName}) c'
                    | Ast.VarMutation (posn, name) ->
                        match Map.tryFind name gamma with
                        | Some (isMutable, tyscheme) ->
                            if ref || isMutable then
                                let ((tau, interfaceConstraints), _, _) = freshInstance tyscheme
                                let err = errStr [posn] "The interface constraints are not satisfied."
                                let interfaceConstraints' = interfaceConstraints |> List.map (fun (conTau, con) -> InterfaceConstraint (conTau, con, err)) |> conjoinConstraints
                                adorn posl tau (T.VarMutation name) interfaceConstraints'
                            else
                                raise <| TypeError' (errStr [posn] (sprintf "The variable named %s is not mutable." name))
                        | None ->
                            raise <| TypeError' (errStr [posn] (sprintf "Unable to find variable named %s in the current scope." name))
                let (rettau, c') =
                    if ref then
                        let tau = freshtyvar ()
                        (tau, c &&& (taul =~= (T.ConApp (T.TyCon T.RefTy, [tau], []), errStr [posl] "The left hand side of the set operation is not a reference, but 'set ref' was used. Do you mean to use just 'set' instead?")))
                    else
                        (taul, c)
                adorn posl rettau left' c'
            // End checkleft
            let (right', c1) = ty right
            let (left', c2) = checkLeft left
            let c' = c1 &&& c2 &&& (T.getType left' =~= (T.getType right', (errStr [posl; posr] "The type of the left hand side should match the type of the right hand side in a set expression.")))
            adorn posE (T.getType right') (T.AssignExp {left=left'; right=right'; ref=ref}) c'
        | Ast.BinaryOpExp {left=(posl, _) as left; op=(poso, A.Pipe); right=(posr, _) as right} ->
            match A.unwrap right with
            | A.CallExp {func=func; args=(posa, args)} ->
                ty (posr, A.CallExp {func=func; args=(posa, args @ [left])})
            | _ ->
                raise <| TypeError' (errStr [posr] "The right hand side of the pipe operator must be a function call expression")
        | Ast.BinaryOpExp {left=(posl, _) as left; op=(poso, op); right=(posr, _) as right} ->
            let op' =
                match op with
                | Ast.Add -> T.Add
                | Ast.BitshiftLeft -> T.BitshiftLeft
                | Ast.BitshiftRight -> T.BitshiftRight
                | Ast.BitwiseAnd -> T.BitwiseAnd
                | Ast.BitwiseOr -> T.BitwiseOr
                | Ast.BitwiseXor -> T.BitwiseXor
                | Ast.Divide -> T.Divide
                | Ast.Equal -> T.Equal
                | Ast.Greater -> T.Greater
                | Ast.GreaterOrEqual -> T.GreaterOrEqual
                | Ast.Less -> T.Less
                | Ast.LessOrEqual -> T.LessOrEqual
                | Ast.LogicalAnd -> T.LogicalAnd
                | Ast.LogicalOr -> T.LogicalOr
                | Ast.Modulo -> T.Modulo
                | Ast.Multiply -> T.Multiply
                | Ast.NotEqual -> T.NotEqual
                | Ast.Subtract -> T.Subtract
            let (left', c1) = ty left
            let (right', c2) = ty right
            let c' = c1 &&& c2
            let b' = T.BinaryOpExp {left=left'; op=op'; right=right'}
            match op with
            | (Ast.LogicalAnd | Ast.LogicalOr) ->
                let c'' = c' &&& (T.booltype =~= (T.getType left', errStr [posl] "Left hand side of binary expression should be of type boolean")) &&&
                                 (T.booltype =~= (T.getType right', errStr [posr] "Right hand side of binary expression should be of type boolean"))
                adorn posE T.booltype b' c''
            | (Ast.Equal | Ast.NotEqual) ->
                let c'' = c' &&& (T.getType left' =~= (T.getType right', errStr [posl; posr] "Left hand side and right hand side of binary expression should be the same type"))
                adorn posE T.booltype b' c''
            | (Ast.Greater | Ast.GreaterOrEqual | Ast.Less | Ast.LessOrEqual) ->
                let cLeft = InterfaceConstraint (T.getType left', IsNum, errStr [posl] "The left hand side must be a number type")
                let cRight = InterfaceConstraint (T.getType right', IsNum, errStr [posr] "The right hand side must be a number type")
                let c'' = c' &&& cLeft &&& cRight
                adorn posE T.booltype b' c''
            | (Ast.BitshiftLeft | Ast.BitshiftRight) ->
                let cLeft = InterfaceConstraint (T.getType left', IsInt, errStr [posl] "The left hand side of this bitshift operation must be an integer.")
                let cRight = InterfaceConstraint (T.getType right', IsInt, errStr [posr] "The right hand side of this bitshift operation must be an integer.")
                let c'' = c' &&& cLeft &&& cRight
                adorn posE (T.getType left') b' c''
            | (Ast.BitwiseAnd | Ast.BitwiseOr | Ast.BitwiseXor) ->
                let cLeft = InterfaceConstraint (T.getType left', IsInt, errStr [posl] "The left hand side of this bitwise operation must be an integer.")
                let cRight = InterfaceConstraint (T.getType right', IsInt, errStr [posr] "The right hand side of this bitwise operation must be an integer.")
                let cEq = ((T.getType left') =~= (T.getType right', errStr [posl; posr] "Left and right hand must be of the same type for this operation"))
                let c'' = c' &&& cLeft &&& cRight &&& cEq
                adorn posE (T.getType left') b' c''
            | (Ast.Add | Ast.Divide | Ast.Modulo | Ast.Multiply | Ast.Subtract) ->
                let cLeft = InterfaceConstraint (T.getType left', IsNum, errStr [posl] "The left hand side must be a number type")
                let cRight = InterfaceConstraint (T.getType right', IsNum, errStr [posr] "The right hand side must be a number type")
                let cEq = ((T.getType left') =~= (T.getType right', errStr [posl; posr] "Left and right hand must be of the same type for this operation"))
                let c'' = c' &&& cLeft &&& cRight &&& cEq
                adorn posE (T.getType left') b' c''
        | Ast.CallExp {func=(posf, _) as func; args=(posa, args)} ->
            let (func', c1) = ty func
            let (args', c2) = typesof args dtenv menv localVars gamma
            let closureTau = freshtyvar ()
            let returnTau = freshtyvar ()
            let placeholders = List.map freshtyvar args
            let c3 = T.ConApp (T.TyCon T.FunTy, closureTau::returnTau::placeholders, []) =~= (T.getType func', errStr [posf; posa] "The function being called does not have a function type or the number of parameters passed to the function is incorrect.")
            let c4 =
                List.map
                    (fun ((posa, _), arg', placeholder) ->
                        placeholder =~= (T.getType arg', errStr [posa] "The type of the argument is incorrect."))
                    (List.zip3 args args' placeholders)
            let c' = c1 &&& c2 &&& c3 &&& (List.fold (&&&) Trivial c4)
            adorn posE returnTau (T.CallExp {func=func'; args=args'}) c'
        | Ast.CaseExp {on=(poso, _) as on; clauses=(posc, clauses)} ->
            let (on', c1) = ty on
            let (clauses', c2) =
                List.map
                    (fun (pattern, ((pose, _) as expr)) ->
                        let (pattern', c1, localVars1, gamma') = checkPattern pattern (T.getType on')
                        let localVars' = Set.union localVars localVars1
                        let (expr', c2) = typeof expr dtenv menv localVars' ienv tyVarMapping capVarMapping gamma'
                        let c' = c1 &&& c2
                        ((pattern', expr'), c'))
                    clauses |>
                List.unzip
            match (List.map (snd >> Ast.getPos) clauses, List.map (snd >> T.getType) clauses') with
            | (firstClausePos::otherClausesPos, firstClauseTau::otherClausesTaus) ->
                let c3 =
                    List.zip otherClausesPos otherClausesTaus |>
                    List.map
                        (fun (pos, clauseTau) ->
                            firstClauseTau =~= (clauseTau, errStr [firstClausePos; pos] "All clauses in case expression should have the same type.")) |>
                    List.fold (&&&) Trivial
                let c' = List.fold (&&&) Trivial ((c1 &&& c3)::c2)
                adorn posE firstClauseTau (T.CaseExp {on=on'; clauses=clauses'}) c'
            | _ ->
                raise <| TypeError' (errStr [posc] "No clauses were found in the case statement")
        | Ast.DoWhileLoopExp {condition=(posc, _) as condition; body=(posb, _) as body} ->
            let (body', c1) = ty body
            let (condition', c2) = ty condition
            let c' = c1 &&& c2 &&& (T.getType condition' =~= (T.booltype, errStr [posc] "Condition of do while loop must be of boolean type")) &&&
                                   (T.getType body' =~= (T.unittype, errStr [posb] "Body of do while loop must return type unit"))
            adorn posE T.unittype (T.DoWhileLoopExp {condition=condition'; body=body'}) c'
        | Ast.WhileLoopExp {condition=(posc, _) as condition; body=(posb, _) as body} ->
            let (body', c1) = ty body
            let (condition', c2) = ty condition
            let c' = c1 &&& c2 &&& (T.getType condition' =~= (T.booltype, errStr [posc] "Condition of while loop must be of boolean type")) &&&
                                   (T.getType body' =~= (T.unittype, errStr [posb] "Body of while loop must return type unit"))
            adorn posE T.unittype (T.WhileLoopExp {condition=condition'; body=body'}) c'
        | Ast.ForLoopExp {typ=maybeTyp; varName=(posv, varName); start=(poss, _) as start; direction=(posd, direction); body=(posb, _) as body; end_=(pose, _) as end_} ->
            let direction' =
                match direction with
                | Ast.Upto -> T.Upto
                | Ast.Downto -> T.Downto
            let tauIterator =
                match maybeTyp with
                | Some tau ->
                    convertType' tau
                | None ->
                    freshtyvar ()
            let (start', c1) = ty start
            let (end_', c2) = ty end_
            let gamma' = Map.add varName (false, T.Forall ([], [], [], tauIterator)) gamma
            let (body', c3) = typeof body dtenv menv (Set.add varName localVars) ienv tyVarMapping capVarMapping gamma'
            let c' = c1 &&& c2 &&& c3 &&& (tauIterator =~= (T.getType start', errStr [posv; poss] "Type of the start expression does not match the type of the iterator")) &&&
                                          (tauIterator =~= (T.getType end_', errStr [posv; pose] "Type of the end expression doesn't match the type of the iterator")) &&&
                                          (T.getType body' =~= (T.unittype, errStr [posb] "Body of do while loop must return type unit")) &&&
                                          (InterfaceConstraint (tauIterator, IsInt, errStr [posv] "Variable must be of integer type"))
            adorn posE T.unittype (T.ForLoopExp {typ=tauIterator; varName=varName; start=start'; end_=end_'; body=body'; direction=direction'}) c'
        | Ast.LambdaExp (posf, {returnTy=maybeReturnTy; arguments=(posargs, arguments); body=(posb, _) as body; interfaceConstraints=(posi, interfaceConstraints)}) ->
            match interfaceConstraints with
            | [] -> ()
            | _ -> raise <| SemanticError' (errStr [posi] "Interface constraints are not supported for lambdas")
            let gamma' = gamma |> Map.map (fun varName (_, scheme) -> (false, scheme)) // Mark all variables as non-mutable within the lambda
            let (gamma1Lst, c1s, localVars1, arguments') =
                arguments |>
                List.map
                    (fun ((posa, argName), maybeArgTau) ->
                        let tau = freshtyvar ()
                        let argConstraint =
                            match maybeArgTau with
                            | Some tauConstraint ->
                                convertType' tauConstraint =~= (tau, errStr [A.getPos tauConstraint] "Invalid argument type constraint")
                            | None ->
                                Trivial
                        let gammaEntry = (argName, (false, T.Forall ([], [], [], tau)))
                        (gammaEntry, argConstraint, argName, (argName, tau))) |>
                List.unzip4
            let gamma'' = Map.merge gamma' (Map.ofList gamma1Lst)
            let c1 = c1s |> conjoinConstraints            
            let localVars' = Set.union localVars (Set.ofList localVars1)
            let (body', c2) = typeof body dtenv menv localVars' ienv tyVarMapping capVarMapping gamma''
            let closureVariables = Set.intersect localVars (AstAnalysis.closure body')
            let (closureList, interfaceConstraints) =
                closureVariables |>
                List.ofSeq |>
                List.map
                    (fun closedVarName ->
                        let (_, tyScheme) = Map.find closedVarName gamma
                        let ((inst, interfaceConstraints), _, _) = freshInstance tyScheme
                        ((closedVarName, inst), interfaceConstraints)) |>
                List.unzip
            let closure = Map.ofList closureList
            let err = errStr [posf] "The interface constraints generated by constructing the closure of the lambda could not be satisfied."
            let interfaceConstraints' = interfaceConstraints |> Seq.concat |> Seq.map (fun (conTau, con) -> InterfaceConstraint (conTau, con, err)) |> List.ofSeq |> conjoinConstraints
            let c3 = 
                match maybeReturnTy with
                | Some returnTau ->
                    convertType' returnTau =~= (T.getType body', errStr [A.getPos returnTau] "Invalid return type constraint")
                | None ->
                    Trivial
            let lambdaTau = T.ConApp ((T.TyCon T.FunTy), (ClosureTy closure)::(T.getType body')::(List.map snd arguments'), [])
            let c' = interfaceConstraints' &&& c1 &&& c2 &&& c3
            adorn posE lambdaTau (T.LambdaExp {closure = closure; returnTy=T.getType body'; arguments=arguments'; body=body'}) c'
        // Hit a let expression that is not part of a sequence
        // In this case its variable bindings are useless, but the right hand side might still
        // produce side effects
        // We also have to make sure that the pattern matching agrees with the type of the right
        | Ast.LetExp {left=left; right=(posr, _) as right} ->
            let (right', c1) = ty right
            let (left', c2, _, _) = checkPattern left (T.getType right')
            let c' = c1 &&& c2
            adorn posE (T.getType left') (T.LetExp {left=left'; right=right'}) c'
        | Ast.DeclVarExp {varName=varName; typ=typ} ->
            let typ' = convertType' typ
            adorn posE typ' (T.DeclVarExp {varName=A.unwrap varName; typ=typ'}) Trivial
        | Ast.ModQualifierExp (posmq, {module_=(pos, module_); name=(posn, name)}) ->
            let ((instance, interfaceConstraints), t, c) =
                match Map.tryFind (module_, name) dtenv with
                | Some (T.FunDecTy tyscheme) ->
                    freshInstance tyscheme
                | Some (T.LetDecTy tau) ->
                    ((tau, []), [], [])
                | Some (T.AliasDecTy _) ->
                    raise <| TypeError' (errStr [posmq] (sprintf "Found declaration named %s in module %s, but it was a alias type declaration and not a value declaration." name module_))
                | Some (T.UnionDecTy _) ->
                    raise <| TypeError' (errStr [posmq] (sprintf "Found declaration named %s in module %s, but it was an algebraic datatype declaration and not a value declaration." name module_))
                | None ->
                    raise <| TypeError' (errStr [posmq] (sprintf "Unable to find declaration named %s in module %s." name module_) |> ErrorMessage.withData (DeclarationNotFoundInModule (posn, module_)))
            let err = errStr [posmq] "The template arguments to the function do not satisfy the interface constraints."
            let interfaceConstraints' = interfaceConstraints |> List.map (fun (conTau, con) -> InterfaceConstraint (conTau, con, err)) |> conjoinConstraints
            adorn posE instance (T.ModQualifierExp ({module_=module_; name=name}, t, c)) interfaceConstraints'
        | Ast.QuitExp maybeTau ->
            let tau =
                match maybeTau with
                | Some tau ->
                    convertType' tau
                | None ->
                    freshtyvar ()
            adorn posE tau (T.QuitExp tau) Trivial
        | Ast.RecordAccessExp {record=(posr, _) as record; fieldName=(posf, fieldName)} ->
            let (record', c') = ty record
            let tau = freshtyvar ()
            let fieldConstraint = InterfaceConstraint (T.getType record', HasField (fieldName, tau), errStr [posE] (sprintf "Expected the expression to be a record type and have a field named %s" fieldName))
            let c'' = c' &&& fieldConstraint
            adorn posE tau (T.RecordAccessExp {record=record'; fieldName=fieldName}) c''
        | Ast.RecordExp { packed=maybePacked; initFields=(posi, initFields)} ->
            let initFieldNames = initFields |> List.map (fst >> Ast.unwrap)
            let maybePacked' =
                match maybePacked with
                | Some _ -> Some initFieldNames
                | None -> None
            let isPacked = Option.isSome maybePacked
            let (fieldExprs', c') =
                initFields |>
                List.map
                    (fun ((_, fieldName), fieldExpr) ->
                        let (fieldExpr', c) = ty fieldExpr
                        (fieldName, fieldExpr'), c) |>
                List.unzip
            let c'' = conjoinConstraints c'
            let tauFields =
                fieldExprs' |>
                List.map (fun (fieldName, fieldExpr') -> (fieldName, T.getType fieldExpr')) |>
                Map.ofList
            let tau = T.RecordTy (maybePacked', tauFields)
            adorn posE tau (T.RecordExp {packed=isPacked; initFields=fieldExprs'}) c''
        | Ast.RefExp ((pose, _) as expr) ->
            let (expr', c') = ty expr
            let tau = T.ConApp ((T.TyCon T.RefTy), [T.getType expr'], [])
            adorn posE tau (T.RefExp expr') c'
        | Ast.Smartpointer (ptr, destructor) ->
            let (ptr', c1) = ty ptr
            let (destructor', c2) = ty destructor
            let closureTy = ClosureTy Map.empty
            let destructorTy = T.ConApp (T.FunTy |> T.TyCon, [closureTy; T.unittype; T.rawpointertype], [])
            let c3 = T.getType ptr' =~= (T.rawpointertype, errStr [A.getPos ptr] "First argument to smartpointer keyword must be of type rawpointer")
            let c4 = T.getType destructor' =~= (destructorTy, errStr [A.getPos destructor] "Second argument to smartpointer keyword must be a destructor of type (||)(rawpointer) -> unit")
            let c' = c1 &&& c2 &&& c3 &&& c4
            adorn posE (T.TyPointer |> T.BaseTy |> T.TyCon) (T.Smartpointer (ptr', destructor')) c'
        | Ast.SequenceExp (poss, exps) ->
            let ((pose, _) as exp)::rest = exps
            let (exp', c1) = ty exp
            let (localVars', gamma', c2) =
                match exp with
                | (_, Ast.LetExp {left=left; right=right}) ->
                    // The constraints are already included in c1 above
                    let (_, c2, localVars', gamma') = checkPattern left (T.getType exp')
                    (Set.union localVars localVars', gamma', c2)
                | (_, Ast.DeclVarExp {varName=varName; typ=typ}) ->
                    let gamma' = Map.add (A.unwrap varName) (true, T.Forall ([], [], [], T.getType exp')) gamma
                    (Set.add (A.unwrap varName) localVars, gamma', Trivial)
                | _ ->
                    (localVars, gamma, Trivial)

            let (tau, rest', c3)  =
                if List.isEmpty rest then
                    // Last thing in the sequence
                    // so the type of the sequence is the type
                    // of the expression
                    (T.getType exp', [], Trivial)
                else
                    // Not the last thing in the sequence
                    // so the type of the sequence is the type
                    // of the rest
                    let ((_, tau, T.SequenceExp rest'), c3) = typeof (poss, Ast.SequenceExp (poss, rest)) dtenv menv localVars' ienv tyVarMapping capVarMapping gamma'
                    (tau, rest', c3)
                    
            let c' = c1 &&& c2 &&& c3
            adorn posE tau (T.SequenceExp (exp'::rest')) c'
        | Ast.CharListLiteral (pos, str) ->
            let codePoints =
                (String.explode str |> List.map (fun c -> int64(c))) @ [0L] |>
                List.map (fun c -> (pos, (pos, c) |> A.UInt8Exp))
            let len = int64 (String.length str + 1)
            // Convert the string literal into a list of uint8s
            ty (pos, A.RecordExp {packed=None;
                                  initFields=(pos, [((pos, "data"), (pos, A.ArrayLitExp (pos, codePoints)));
                                                    ((pos, "length"), (pos, A.UInt32Exp (pos, len)))])})
        | A.StringLiteral (pos, str) ->
            adorn posE T.stringtype (T.StringExp str) Trivial
        | Ast.TemplateApplyExp {func=(posf, func); templateArgs=(post, {tyExprs=(postyexprs, tyExprs); capExprs=(posc, capExprs)})} ->            
            let (func', scheme) =
                match func with
                | Choice1Of2 name ->
                    match Map.tryFind name gamma with
                    | Some (_, scheme) ->
                        (Choice1Of2 name, scheme)  
                    | None ->
                        raise <| TypeError' (errStr [posf] (sprintf "Unable to find function named '%s' in the current scope." name))
                | Choice2Of2 {module_=(posm, module_); name=(posn, name)} ->
                    match Map.tryFind (module_, name) dtenv with
                    | Some (T.FunDecTy scheme) ->
                        (Choice2Of2 ({module_=module_; name=name} : T.ModQualifierRec), scheme)
                    | Some _ ->
                        raise <| TypeError' (errStr [posf] (sprintf "Found declaration named '%s' in module '%s', but it was not a function declaration." name module_))
                    | None ->
                        raise <| TypeError' (errStr [posf] (sprintf "Unable to find declaration named '%s' in module '%s'" name module_) |> ErrorMessage.withData (DeclarationNotFoundInModule (posn, module_)))
            let templateArgs' = List.map convertType' tyExprs
            let templateArgsCaps' = List.map (Ast.unwrap >> convertCapacity') capExprs
            let (Forall (quantifiedTys, quantifiedCaps, _, _)) = scheme
            if List.length templateArgs' = List.length quantifiedTys then
                ()
            else
                raise <| TypeError' (errStr [postyexprs] (sprintf "Invalid number of template type arguments. Expected %d arguments but received %d." (List.length quantifiedTys) (List.length templateArgs')))
            if List.length templateArgsCaps' = List.length quantifiedCaps then
                ()
            else
                raise <| TypeError' (errStr [posc] (sprintf "Invalid number of template capacity arguments. Expected %d arguments but received %d." (List.length quantifiedCaps) (List.length templateArgsCaps')))
            let (tau, interfaceConstraints) = instantiate scheme templateArgs' templateArgsCaps'
            let err = errStr [post] "The template arguments to the function do not satisfy the interface constraints."
            let interfaceConstraints' = interfaceConstraints |> List.map (fun (conTau, con) -> InterfaceConstraint (conTau, con, err)) |> conjoinConstraints
            adorn posE tau (T.FunctionWrapperEmptyClosure (posE, tau, T.TemplateApplyExp {func=func'; templateArgs={tyExprs=templateArgs'; capExprs=templateArgsCaps'}})) interfaceConstraints'
        | Ast.TupleExp exprs ->
            let (exprs', c') = typesof exprs dtenv menv localVars gamma
            let subTaus = List.map T.getType exprs'
            let tau = T.ConApp ((T.TyCon T.TupleTy), subTaus, [])
            adorn posE tau (T.TupleExp exprs') c'
        | Ast.TypeConstraint {exp=(pose, _) as exp; typ=(post, _) as typ} ->
            let (exp', c1) = ty exp
            let c' = c1 &&& (convertType' typ =~= (T.getType exp', errStr [pose; post] "Type constraint could not be satisfied"))
            adorn posE (T.getType exp') (T.unwrap exp') c'
        | Ast.UnsafeTypeCast {exp=(pose, _) as exp; typ=typ} ->
            let (exp', c) = ty exp
            let typ' = convertType' typ
            adorn pose typ' (T.unwrap exp') c
        | Ast.UnaryOpExp {op=(poso, op); exp=(pose, _) as exp} ->
            let (exp', c1) = ty exp
            let (op', c2, tau) =
                match op with
                | Ast.LogicalNot ->
                    (T.LogicalNot, T.booltype =~= (T.getType exp', errStr [pose] "The type of an expression applied to a logical not operation must be a boolean"), T.booltype)
                | Ast.BitwiseNot ->
                    let c3 = InterfaceConstraint (T.getType exp', T.IsInt, errStr [pose] "Bitwise not operator argument must be a of integer type")
                    (T.BitwiseNot, c3, T.getType exp')
                | Ast.Negate ->
                    let c3 = InterfaceConstraint (T.getType exp', T.IsNum, errStr [pose] "Negation operator argument must be a number")
                    (T.Negate, c3, T.getType exp')
                | Ast.Deref ->
                    let retTau = freshtyvar ()
                    let c' = (T.ConApp (T.TyCon T.RefTy, [retTau], []) =~= (T.getType exp', errStr [pose] "Attempting to dereference an expression with a non-ref type."))
                    (T.Deref, c', retTau)
            let c' = c1 &&& c2
            adorn posE tau (T.UnaryOpExp {op=op'; exp=exp'}) c'
        | Ast.NullExp (posN, ()) ->
            adorn posN T.rawpointertype T.NullExp Trivial
    ty (posE, e)

let typecheckProgram (program : Ast.Module list) (fnames : string list) =
    let modNamesToMods =
        let names =
            List.zip program fnames |>
            List.map (fun (module_, fname) ->
                match nameInModule module_ with
                | Some name -> Ast.unwrap name
                | None -> raise <| SemanticError (sprintf "Semantic error in %s: The module does not contain exactly one module declaration." fname))
        Map.ofList (List.zip names program)
    
    let valueDecsSet =
        program |> List.map (fun decs ->
            let modName = nameInModule decs |> Option.get |> Ast.unwrap
            let valNames = valueDecsInModule decs
            List.zip
                (List.map (fun _ -> modName) valNames)
                valNames) |> List.concat |> Set.ofList

    let modNamesToMenvs =
        // maps names to module qualifiers
        let menvs0 = (List.map (fun (Ast.Module decs) ->
            let modName = nameInModule (Ast.Module decs) |> Option.get |> Ast.unwrap
            let typeNames = typesInModule (Ast.Module decs)
            let valNames = valueDecsInModule (Ast.Module decs)
            // Find the names of all of the value constructors as well
            let valConNames =
                decs |> List.map Ast.unwrap |> List.filter isUnionDec |>
                List.map
                    (fun (Ast.UnionDec {valCons=(_, valCons)}) -> valCons |> List.map (fun ((_, name), _) -> name)) |>
                List.concat
            let names = typeNames @ valNames @ valConNames
            match Seq.duplicates names with
            | dups when Seq.length dups = 0 ->
                List.fold (fun map2 name ->
                    Map.add name (modName, name) map2
                ) Map.empty names
            | dups ->
                let dupsStr = String.concat ", " dups
                raise <| SemanticError (sprintf "Semantic error in module %s: The following declarations have duplicate definitions: %s" modName dupsStr)
        ) program)

        let modNamesToMenvs0 = Map.ofList (List.zip (List.map (nameInModule >> Option.get >> Ast.unwrap) program) menvs0)

        // Merge the menvs together based on the open declarations
        (Map.map (fun modName menv0 ->
            let allOpens = List.map Ast.unwrap (opensInModule (Map.find modName modNamesToMods))
            List.fold (fun menv1 nameToMerge ->
                match Map.tryFind nameToMerge modNamesToMenvs0 with
                | Some menv' -> Map.merge menv' menv1 
                | None -> raise <| SemanticError (sprintf "Semantic error: Module %s opens %s, which does not exist." modName nameToMerge)
            ) menv0 allOpens
        ) modNamesToMenvs0)
    
    // Maps module qualifiers to their actual declarations
    let denv = (List.fold (fun map (Ast.Module decs) ->
        let modName = nameInModule (Ast.Module decs) |> Option.get
        let namedDecs = List.filter (Ast.unwrap >> isNamedDec) decs
        List.fold (fun map2 dec0 ->
            let decName = nameOfDec (Ast.unwrap dec0)
            let qual = (Ast.unwrap modName, Ast.unwrap decName)
            Map.add qual dec0 map2) map namedDecs
    ) Map.empty program)

    let ienv = (Map.fold (fun accumIenv ((module_, name) as modQual) d ->
        match Ast.unwrap d with
        | Ast.UnionDec {valCons=(_, valCons)} ->
            (List.mapi (fun i ((_, valConName), _) ->
                (valConName, i)
            ) valCons) |>
            (List.fold (fun accumIenv' (valConName, i) ->
                Map.add (module_, valConName) i accumIenv')
            accumIenv)
        | _ ->
            accumIenv
    ) Map.empty denv)
    
    let extractFromTemplate maybeTemplate =
        match maybeTemplate with
        | None -> ([], [])
        | Some (_, ({tyVars=(_, tyVars); capVars=maybeCapVars} : Ast.Template)) ->
            let t = List.map Ast.unwrap tyVars
            let c =
                match maybeCapVars with
                | None -> []
                | Some (_, capVars) ->
                    List.map Ast.unwrap capVars
            (t, c)

    let dtenv0 = (Map.fold (fun accumDtenv0 ((module_, name) as modQual) d ->
        let menv = Map.find module_ modNamesToMenvs
        match Ast.unwrap d with
        | Ast.AliasDec {template=maybeTemplate; typ=typ} ->
            let (t,c) = extractFromTemplate maybeTemplate
            let t' = t |> List.map freshtyvar
            let c' = c |> List.map freshcapvar
            let tyVarMapping = List.zip t t' |> Map.ofList
            let capVarMapping = List.zip c c' |> Map.ofList
            let t'str = t' |> List.map (fun (T.TyVar tyvar) -> tyvar)
            let c'str = c' |> List.map (fun (T.CapacityVar capvar) -> capvar)
            let aliasDecTy = T.AliasDecTy (t'str, c'str, (convertType menv Map.empty tyVarMapping capVarMapping) typ)
            Map.add modQual aliasDecTy accumDtenv0
        | Ast.UnionDec {valCons=(_, valCons); template=maybeTemplate} ->
            let (t, c) = extractFromTemplate maybeTemplate
            let udecty = T.UnionDecTy (t, c, {module_=module_; name=name})
            let retTyBase = T.TyCon <| T.ModuleQualifierTy {module_=module_; name=name}
            let retTy =
                match maybeTemplate with
                | None -> retTyBase
                | Some _ -> T.ConApp (retTyBase, List.map T.TyVar t, List.map T.CapacityVar c)
            let closureTy = T.ClosureTy Map.empty
            let accumDtenv2 = valCons |> (List.fold (fun accumDtenv1 ((_, valConName), innerTys) ->
                let paramTy = List.map (convertType menv Map.empty Map.empty Map.empty) innerTys
                let ts = T.FunDecTy <| T.Forall (t, c, [], T.ConApp (T.TyCon T.FunTy, closureTy::retTy::paramTy, []))
                Map.add (module_, valConName) ts accumDtenv1
            ) accumDtenv0)
            Map.add modQual udecty accumDtenv2
        | _ ->
            accumDtenv0
    ) Map.empty denv)

    // Check the dependency graph first to determine what order we need to typecheck in (topological sort)
    let valueGraph = new QuikGraph.AdjacencyGraph<string*string, QuikGraph.Edge<string*string>>()

    program |> List.iter (fun moduledef ->
        let module_ = nameInModule moduledef |> Option.get |> Ast.unwrap
        // List of all let and function declarations in
        // the module currently being considered
        let valueDecs = valueDecsInModule moduledef
        // Add all the declarations as vertices to the graph
        valueDecs |> List.iter (fun name ->
            valueGraph.AddVertex((module_, name)) |> ignore
        ))

    program |> List.iter (fun moduledef ->
        let module_ = nameInModule moduledef |> Option.get |> Ast.unwrap
        // List of all let and function declarations in
        // the module currently being considered
        let valueDecs = valueDecsInModule moduledef
        let menv = Map.find module_ modNamesToMenvs
        // Find out what declarations this declaration refers to
        valueDecs |> List.iter (fun name ->
            let (pos, dec) = Map.find (module_, name) denv
            let references =
                match dec with
                | (Ast.FunctionDec {template=template; clause=(_, {body=(_, expr); arguments=(_, arguments)})}) ->
                    let a1 = arguments |> List.map (fst >> Ast.unwrap) |> Set.ofList
                    // Capacities are local variables as well!
                    let a2 =
                        match template with
                        | Some (_, {capVars=Some (_, capVars)}) ->
                            capVars |> List.map Ast.unwrap |> Set.ofList
                        | _ -> Set.empty
                    decRefs valueDecsSet menv (Set.union a1 a2) expr
                | Ast.LetDec {right=(_, expr)} ->
                    decRefs valueDecsSet menv Set.empty expr
            // Add all the edges to the graph
            references |> Set.iter (fun reference ->
                if Map.containsKey reference denv then
                    valueGraph.AddEdge(new QuikGraph.Edge<string*string>((module_, name), reference)) |> ignore
                else
                    raise <| TypeError' (errStr [pos] (sprintf "Reference made to %s:%s which could not be found" (fst reference) (snd reference)))
            )
        )
    )

    // Also build a dependency graph for the types
    let typeGraph = new QuikGraph.AdjacencyGraph<string*string, QuikGraph.Edge<string*string>>()

    program |> List.iter (fun moduledef ->
        let (Ast.Module decs) = moduledef
        let module_ = nameInModule moduledef |> Option.get |> Ast.unwrap
        // List of all algebraic and alias datatypes in
        // the module currently being considered
        let typeDecs = typesInModule moduledef
        let menv = Map.find module_ modNamesToMenvs
        // Add all the declarations as vertices to the graph
        typeDecs |> List.iter (fun name ->
            typeGraph.AddVertex((module_, name)) |> ignore
            let (pos, dec) = Map.find (module_, name) denv
            let references = tyRefs menv dec
            references |> Set.iter (fun reference ->
                // TODO: Better check here
                if Map.containsKey reference dtenv0 then
                    typeGraph.AddEdge(new QuikGraph.Edge<string*string>((module_, name), reference)) |> ignore
                else
                    raise <| TypeError' (errStr [pos] (sprintf "Reference made to %s:%s which could not be found" (fst reference) (snd reference))))))
    
    let typeDependencyOrder =
        // Ensure that there are no circular type dependencies
        let typeCondensation = typeGraph.CondensateStronglyConnected<_, _, QuikGraph.AdjacencyGraph<_, _>>()
        typeCondensation.Vertices |>
        Seq.iter
            (fun scc ->
                let sccStr = scc.Vertices |> Seq.map (fun (m, n) -> sprintf "%s:%s" m n) |> String.concat ", "
                // Check for mutually recursive types
                if Seq.length scc.Vertices > 1 then
                    raise <| TypeError (sprintf "Semantic error: The following type declarations form an unresolvable dependency cycle: %s" sccStr)
                // Check for self-referential types
                elif Seq.length scc.Edges > 0 then
                    raise <| TypeError (sprintf "Semantic error: The following type refers to itself: %s" sccStr))
        typeGraph.TopologicalSort() |> Seq.rev |> List.ofSeq

    let includeDecs = 
        program |>
        List.map (fun (Ast.Module decs) -> List.filter (Ast.unwrap >> isInclude) decs) |>
        List.concat |>
        List.map (fun (_, Ast.IncludeDec (_, includes)) ->
            T.IncludeDec (List.map Ast.unwrap includes))

    let openDecs = 
        program |>
        List.map (fun (Ast.Module decs) ->
            let module_ = nameInModule (Ast.Module decs) |> Option.get |> Ast.unwrap
            let opens = List.filter (Ast.unwrap >> isOpen) decs
            let moduleNames = List.map (fun _ -> module_) opens
            List.zip moduleNames opens) |>
        List.concat |>
        List.map (fun (module_, (_, Ast.OpenDec (_, openModules))) ->
            (module_, T.OpenDec (List.map Ast.unwrap openModules)))
    
    let moduleNames = program |> List.map (fun decs -> nameInModule decs |> Option.get |> Ast.unwrap)

    let typeDecs =
        let unorderedDecs =
            program |>
            List.map (fun (Ast.Module decs) ->
                let module_ = nameInModule (Ast.Module decs) |> Option.get |> Ast.unwrap
                let types = List.filter (Ast.unwrap >> isTypeDec) decs
                let moduleNames = List.map (fun _ -> module_) types
                List.zip moduleNames types) |>
            List.concat |>
            List.map (fun (module_, (_, dec)) ->
                let menv = Map.find module_ modNamesToMenvs
                match dec with
                | Ast.UnionDec {name=(_, name); valCons=(_, valCons); template=template} ->
                    let valCons' =
                        valCons |> List.map (fun ((_, valConName), argTypes) ->
                            let argTypes' = List.map (convertType menv dtenv0 Map.empty Map.empty) argTypes
                            (valConName, argTypes'))
                    let ret = (module_, T.UnionDec {name=name; template=Option.map (Ast.unwrap >> convertTemplate) template; valCons=valCons'})
                    ((module_, name), ret)
                | Ast.AliasDec {name=(_, name); template=template; typ=typ} ->
                    let typ' = convertType menv dtenv0 Map.empty Map.empty typ
                    let ret = (module_, T.AliasDec {name=name; template=Option.map (Ast.unwrap >> convertTemplate) template; typ=typ'})
                    ((module_, name), ret))|>
            Map.ofList
        // Put the declarations into dependency order (reverse topological order)
        List.map (fun modQual -> Map.find modQual unorderedDecs) typeDependencyOrder
    
    let inlineCodeDecs = 
        program |>
        List.map (fun (Ast.Module decs) ->
            let module_ = nameInModule (Ast.Module decs) |> Option.get |> Ast.unwrap
            let inlineCode = List.filter (Ast.unwrap >> isInlineCodeDec) decs
            let moduleNames = List.map (fun _ -> module_) inlineCode
            List.zip moduleNames inlineCode) |>
        List.concat |>
        List.map (fun (module_, (_, Ast.InlineCodeDec (_, code))) ->
            (module_, T.InlineCodeDec code))
    
    let connectedComponents = new System.Collections.Generic.Dictionary<string * string, int>()
    valueGraph.StronglyConnectedComponents(connectedComponents) |> ignore

    let sccs = connectedComponents |> Map.ofDict |> Map.invertNonInjective |> Map.toList |> List.map snd

    // Now verify that each SCC either contains all functions or just a single let
    sccs |>
    List.iter
        (fun scc ->
            let sccStr = scc |> List.map (fun (m, n) -> sprintf "%s:%s" m n) |> String.concat ", "
            match scc with
            | [x] when isLet (Ast.unwrap (Map.find x denv)) ->
                ()
            | _ ->
                scc |> List.iter
                    (fun decref ->
                        let (pos, possibleLet) = Map.find decref denv
                        if isLet possibleLet then
                            let (module_, name) = decref
                            raise <| TypeError' (errStr [pos] (sprintf "Semantic error: The let named '%s' in module '%s' has a self reference. The following declarations form an unresolvable dependency cycle: %s" name module_ sccStr))
                        else
                            ()))
    
    let condensation = valueGraph.CondensateStronglyConnected<_, _, QuikGraph.AdjacencyGraph<_, _>>()
    let dependencyOrder = condensation.TopologicalSort() |> Seq.map (fun scc -> scc.Vertices |> List.ofSeq) |> Seq.rev |> List.ofSeq

    let localGamma globalGamma module_ =
        let menv = Map.find module_ modNamesToMenvs
        (Map.fold (fun gammaAccum localName moduleQual ->
            match Map.tryFind moduleQual globalGamma with
            | Some ty ->
                Map.add localName (false, ty) gammaAccum
            | None ->
                gammaAccum
        ) Map.empty menv)

    let globalGammaInit =
        program |> List.map (fun (Ast.Module decs) ->
            let module_ = nameInModule (Ast.Module decs) |> Option.get |> Ast.unwrap
            decs |> List.map Ast.unwrap |> List.filter isUnionDec |>
            List.map
                (fun (Ast.UnionDec {valCons=(_, valCons)}) ->
                    valCons |> List.map (fun ((_, name), _) ->
                        let modqual = (module_, name)
                        let (T.FunDecTy scheme) = Map.find modqual dtenv0
                        (modqual, scheme))) |>
            List.concat) |> List.concat |> Map.ofList

    // TODO: Topological sort for types
    let (checkedDecs, _) =
        dependencyOrder |> List.mapFold
            (fun (dtenv, globalGamma) scc ->
                match scc with
                // Found a SCC containing a single let statement
                | [(module_, name) as modqual] when isLet (Ast.unwrap (Map.find modqual denv)) ->
                    let (posl, Ast.LetDec {varName=varName; typ=typ; right=right}) = Map.find modqual denv
                    let menv = Map.find module_ modNamesToMenvs
                    let localVars = Set.empty
                    let gamma = localGamma globalGamma module_
                    let (right', c1) = typeof right dtenv menv localVars ienv Map.empty Map.empty gamma
                    let c2 =
                        match typ with
                        | Some ((post, _) as ty) ->
                            T.getType right' =~= (convertType menv dtenv Map.empty Map.empty ty, errStr [post; Ast.getPos right] "The type of the right hand side of the let expression violates the given type constraint.")
                        | None ->
                            Trivial
                    let c = c1 &&& c2
                    let (theta, kappa, interfaceConstraintMap) = solve c []
                    let interfaceConstraintMap' = interfaceConstraintMap |> Map.map (fun name constrs -> constrs |> List.map fst)
                    let tau = tycapsubst theta kappa (T.getType right')
                    let elabtau = generalize Set.empty Set.empty interfaceConstraintMap' tau
                    match elabtau with
                    | T.Forall ([], [], _, _) ->
                        ()
                    | _ ->
                        raise <| TypeError' (errStr [posl] (sprintf "Let declaration was inferred to have the following type scheme:\n\n%s\n\nTop level let declarations do not support values of polymorphic type. Once the Arduino IDE turns on C++14 by default this may change (variable templates). In the meantime, consider either removing the polymorphism, add a type annotation, or wrap your variable in a function." (schemeString elabtau)))
                    match AstAnalysis.findFreeVars theta kappa right' with
                    | ([], []) ->
                        ()
                    | ((badPos, _)::_, _) ->
                        raise <| TypeError' (errStr [badPos] "Too much polymorphism! The following expression has a type that was detected to contain a type variable. Consider adding a type constraint to fix the source of this problem")
                    | (_, (badPos, _)::_) ->
                        raise <| TypeError' (errStr [badPos] "Too much polymorphism! The following expression has a capacity that was detected to contain a capacity variable. Consider adding a type constraint to fix the source of this problem")
                    let globalGamma' = Map.add modqual elabtau globalGamma
                    let dtenv' = Map.add modqual (T.LetDecTy tau) dtenv
                    let let' = T.LetDec {varName=name; typ=tau; right=right'}
                    (([(module_, let')], theta, kappa), (dtenv', globalGamma'))
                // Found a SCC containing mutually recursive function(s)
                | _ ->
                    let alphas = List.map freshtyvar scc
                    let alphaSchemes = List.map (fun a -> T.Forall ([], [], [], a)) alphas
                    let tempGlobalGamma =
                        List.fold (fun globalGammaAccum (modqual, alphaScheme) ->
                            Map.add modqual alphaScheme globalGammaAccum
                        ) globalGamma (List.zip scc alphaSchemes)
                    let tempGammas = List.map (fst >> localGamma tempGlobalGamma) scc
                    let tempdtenv =
                        List.fold (fun accumdtenv (modqual, alphaScheme) ->
                            Map.add modqual (T.FunDecTy alphaScheme) accumdtenv
                        ) dtenv (List.zip scc alphaSchemes)
                    let (funDecsInfo, capVarNames, cs) =
                        List.zip3 scc tempGammas alphas |>
                        List.map
                            (fun ((module_, name) as modqual, tempGamma, alpha) ->
                                let (posf, Ast.FunctionDec {template=template; clause=(posc, {arguments=(posa, arguments); body=body; returnTy=maybeReturnTy; interfaceConstraints=(posi, interfaceConstraints) })}) = Map.find modqual denv
                                let menv = Map.find module_ modNamesToMenvs
                                let localArguments = List.map (fst >> Ast.unwrap) arguments |> Set.ofList
                                if Set.count localArguments = List.length arguments then
                                    ()
                                else
                                    raise <| TypeError' (errStr [posa] "There are duplicate argument names")
                                let (tyVarMapping, capVarMapping, maybeTemplate', capVarNames, localCapacities) =
                                    match template with
                                    | None -> (Map.empty, Map.empty, None, [], Set.empty)
                                    | Some (_, {tyVars=(_, tyVars); capVars=maybeCapVars}) ->
                                        let capVars = maybeCapVars |> Option.map Ast.unwrap |> Option.toList |> List.concat
                                        let tyVars' = List.map freshtyvar tyVars
                                        let tyVars'Names = List.map (fun (T.TyVar name) -> name) tyVars'
                                        let capVars' = List.map freshcapvar capVars
                                        let capVars'Names = List.map (fun (T.CapacityVar name) -> name) capVars'
                                        let t = List.zip (List.map Ast.unwrap tyVars) tyVars' |> Map.ofList
                                        let c = List.zip (List.map Ast.unwrap capVars) capVars' |> Map.ofList
                                        let localVars1 = capVars |> List.map Ast.unwrap |> Set.ofList
                                        let templatePos = (tyVars |> List.map Ast.getPos, capVars |> List.map Ast.getPos)
                                        (t, c, Some (({tyVars=tyVars'Names; capVars=capVars'Names} : T.Template), templatePos, tyVars, capVars), capVars'Names, localVars1)
                                let localVars = Set.union localArguments localCapacities
                                // Add the arguments to the type environment (gamma)
                                let (argTys, tempGamma') =
                                    arguments |>
                                    List.mapFold
                                        (fun accumTempGamma' ((posn, name), maybeTy) ->
                                            let argTy = match maybeTy with
                                                        | Some ty -> convertType menv tempdtenv tyVarMapping capVarMapping ty
                                                        | None -> freshtyvar ()
                                            let argTyScheme = T.Forall ([], [], [], argTy)
                                            (argTy, Map.add name (false, argTyScheme) accumTempGamma'))
                                        tempGamma
                                // Add the capacities to the type environment
                                let tempGamma'' = localCapacities |> (Set.fold (fun accum capVarName ->
                                    Map.add capVarName (false, T.Forall ([], [], [], T.int32type)) accum
                                ) tempGamma')
                                // Finally typecheck the body
                                let (body', c1) = typeof body tempdtenv menv localVars ienv tyVarMapping capVarMapping tempGamma''
                                let closureTy = T.ClosureTy Map.empty
                                let c2 = alpha =~= (T.ConApp (T.TyCon T.FunTy, closureTy::(T.getType body')::argTys, []), errStr [posf] "The inferred type of the function violated a constraint based on the function declaration")
                                let c3 =
                                    match maybeReturnTy with
                                    | None -> Trivial
                                    | Some ((post, _) as ty) ->
                                        (T.getType body') =~= (convertType menv tempdtenv tyVarMapping capVarMapping ty, errStr [posf; post] "The type of the body did not match the type of the return constraint")
                                let c4s =
                                    interfaceConstraints |>
                                    List.map (fun (posCon, (tau, (_, con))) ->
                                        let tau' = convertType menv tempdtenv tyVarMapping capVarMapping tau
                                        convertInterfaceConstraint menv tempdtenv con |>
                                        List.map (fun con' -> InterfaceConstraint (tau', con', errStr [posCon] "The specified type did not meet the interface constraint")) |>
                                        conjoinConstraints)
                                let c4 = conjoinConstraints c4s
                                let c = c1 &&& c2 &&& c3 &&& c4
                                let argNames = arguments |> List.map (fst >> Ast.unwrap)
                                ((modqual, maybeTemplate', argNames, argTys, T.getType body', body'), capVarNames, c)) |> List.unzip3
                    let c = conjoinConstraints cs
                    let capVarNames' = List.concat capVarNames
                    // Solve the entire SCC at once
                    let (theta, kappa, interfaceConstraints) = solve c capVarNames'
                    let (funDecs', (dtenv', globalGamma')) =
                        funDecsInfo |>
                        List.mapFold
                            (fun (accumDtenv', accumGlobalGamma') ((module_, name) as modqual, maybeTemplate', argNames, argTys, retTy, body') ->
                                let retTy' = tycapsubst theta kappa retTy
                                let argTys' = argTys |> List.map (tycapsubst theta kappa)
                                let arguments' = List.zip argNames argTys'
                                let (t, c, maybeTemplate'', maybeOriginalTyVars, maybeOriginalCapVars) =
                                    match maybeTemplate' with
                                    | None ->
                                        let (freets, freecs) =
                                            retTy'::argTys' |> List.fold
                                                (fun (accumFreeT, accumFreeC) argTy ->
                                                    let (freeT, freeC) = freeVars argTy
                                                    (Set.union freeT accumFreeT, Set.union freeC accumFreeC))
                                                (Set.empty, Set.empty)
                                        if Set.isEmpty freets && Set.isEmpty freecs then
                                            ([], [], None, None, None)
                                        else
                                            let tyVars = List.ofSeq freets
                                            let capVars = List.ofSeq freecs
                                            (tyVars, capVars, Some ({tyVars=tyVars; capVars=capVars} : T.Template), None, None)
                                    | Some ({tyVars=tyVars; capVars=capVars}, (tyVarsPos, capVarsPos), originalTyVars, originalCapVars) ->
                                        let tyVars' = List.zip tyVars tyVarsPos |> List.map (fun (tyvar, post) ->
                                            match tycapsubst theta kappa (T.TyVar tyvar) with
                                            | T.TyVar tyvar' ->
                                                tyvar'
                                            | x ->
                                                raise <| TypeError' (errStr [post] (sprintf "The type parameter was inferred to be equivalent to the non-type variable '%s'" (T.typeString x))))
                                        let capVars' = List.zip capVars capVarsPos |> List.map (fun (capvar, posc) ->
                                            match simplifyCap (capsubst kappa (T.CapacityVar capvar)) with
                                            | T.CapacityVar capvar' ->
                                                capvar'
                                            | x ->
                                                raise <| TypeError' (errStr [posc] (sprintf "The capacity parameter was inferred to be equivalent to the non-capacity variable '%s'" (T.capacityString x))))   
                                        (tyVars', capVars', Some ({tyVars=tyVars'; capVars=capVars'} : T.Template), Some originalTyVars, Some originalCapVars)
                                // Now expand the set of type variables to account for interface field constraints
                                // this process is roughly equivalent to deriving functional dependency rules for record
                                // types. The type variables discovered during this process will be used in the next
                                // step only where we check for too much polymorphism. Example of the logic used here:
                                // Suppose we have an interface constraint 'a : { myField : 'b }, that is 'a is a type
                                // variable that is a record with a field named myField of type 'b. When detecting excess
                                // polymorphism, we would like to take into account that fixing a type for 'a also fixes
                                // its field type. In functional dependency terms, 'a -> 'b

                                // The return type of expandFunctionalDependency is (Set<string>*Set<string>), where the first
                                // set is the expanded type variables and the second is capacity variables
                                let rec expandFunctionalDependency (tyVar: string) : Set<string>*Set<string> =
                                    match Map.tryFind tyVar interfaceConstraints with
                                    | Some constraints ->
                                        let (extraTyVars, extraCapVars) =
                                            constraints |>
                                            List.map
                                                (fun (constraintType, _) ->
                                                    match constraintType with
                                                    | HasField (_, tau) ->
                                                        freeVars tau
                                                    | _ ->
                                                        (Set.empty, Set.empty)) |>
                                            List.unzip
                                        let (extraTyVars', extraCapVars') =
                                            extraTyVars |>
                                            Set.unionMany |>
                                            List.ofSeq |>
                                            List.map (expandFunctionalDependency) |>
                                            List.unzip
                                        let extraTyVars'' = Set.union (Set.unionMany extraTyVars') (Set.unionMany extraTyVars)
                                        let extraCapVars'' = Set.union (Set.unionMany extraCapVars') (Set.unionMany extraCapVars)
                                        (Set.add tyVar extraTyVars'', extraCapVars'')
                                    | None ->
                                        (Set.singleton tyVar, Set.empty)
                                let (funDepsTs, funDepsCs) = t |> List.map expandFunctionalDependency |> List.unzip
                                let funDepsTs' = Set.unionMany funDepsTs
                                let funDepsCs' = Set.union (Set.ofList c) (Set.unionMany funDepsCs)
                                // Now check to see if any of the interface constraints contain free variables
                                // that are not also members of the template. If this is the case, there is
                                // polymorphism within the function that cannot be reified by inferring either the
                                // arguments or return types. This is an error
                                match Set.difference (Map.keys interfaceConstraints) funDepsTs' |> List.ofSeq with
                                | [] -> ()
                                | badFreeVar::_ ->
                                    let (_, errMsg)::_ = Map.find badFreeVar interfaceConstraints
                                    errMsg
                                    |> ErrorMessage.mapMsg (sprintf "Too much polymorphism! A polymorphic interface constraint was detected containing a type variable that would not be reified by fixing either the argument types or return types. Consider adding a type constraint to fix the source of this problem.\n\n%s")
                                    |> TypeError'
                                    |> raise
                                let (freeTyVarsInBody, freeCapVarsInBody) = AstAnalysis.findFreeVars theta kappa body'
                                match Set.difference (List.map A.unwrap freeTyVarsInBody |> Set.ofList) funDepsTs' |> List.ofSeq with
                                | [] -> ()
                                | badFreeVar::_ ->
                                    let (pos, _) = List.find (fun freeVar -> (A.unwrap freeVar) = badFreeVar) freeTyVarsInBody
                                    raise <| TypeError' (errStr [pos] "Too much polymorphism! The following expression has a type that was detected to contain a type variable that would not be reified by fixing either the argument types or return types. Consider adding a type constraint to fix the source of this problem")
                                match Set.difference (List.map A.unwrap freeCapVarsInBody |> Set.ofList) funDepsCs' |> List.ofSeq with
                                | [] -> ()
                                | badFreeVar::_ ->
                                    let (pos, _) = List.find (fun freeVar -> (A.unwrap freeVar) = badFreeVar) freeCapVarsInBody
                                    raise <| TypeError' (errStr [pos] "Too much polymorphism! The following expression has a capacity that was detected to contain a capacity variable that would not be reified by fixing either the argument types or return types. Consider adding a type constraint to fix the source of this problem")
                                let relevantInterfaceConstraints =
                                    t |>
                                    List.map
                                        (fun tau ->
                                            match Map.tryFind tau interfaceConstraints with
                                            | Some constraints ->
                                                constraints |>
                                                List.map (fun (constTyp, _) -> (T.TyVar tau, constTyp))
                                            | None -> []) |>
                                    Seq.concat |>
                                    List.ofSeq
                                let closureTy = T.ClosureTy Map.empty
                                let funScheme = T.Forall (t, c, relevantInterfaceConstraints, T.ConApp (T.TyCon T.FunTy, closureTy::retTy'::argTys', []))
                                let body'' =
                                    match maybeOriginalTyVars with
                                    | (None | Some []) ->
                                        body'
                                    | Some originalTyVars ->
                                        let (pos, tau, innerBody) = body'
                                        let saveTyVars =
                                            List.zip originalTyVars t |> List.map (fun ((post, originalTyVar), newTyVar) ->
                                                (post, T.unittype, T.InternalUsing {varName = originalTyVar; typ = T.TyVar newTyVar}))
                                        let innerBody' = T.SequenceExp (saveTyVars @ [body'])
                                        (pos, tau, innerBody')
                                let body''' =
                                    match maybeOriginalCapVars with
                                    | (None | Some []) ->
                                        body''
                                    | Some originalCapVars ->
                                        let (pos, tau, innerBody) = body''
                                        let saveCapVars =
                                            List.zip originalCapVars c |> List.map (fun ((posc, originalCapVar), newCapVar) ->
                                                (posc, T.int32type, T.InternalUsingCap {varName=originalCapVar; cap=T.CapacityVar newCapVar}))
                                        let innerBody' = T.SequenceExp (saveCapVars @ [body'])
                                        (pos, tau, innerBody')
                                let closure = Map.empty
                                let funDec' = T.FunctionDec {name=name; template=maybeTemplate''; clause={closure=closure; returnTy=retTy'; arguments=arguments'; body=body'''}}
                                let accumDtenv'' = Map.add modqual (T.FunDecTy funScheme) accumDtenv'
                                let globalGamma'' = Map.add modqual funScheme accumGlobalGamma'
                                ((module_, funDec'), (accumDtenv'', globalGamma'')))
                            (dtenv, globalGamma)
                    ((funDecs', theta, kappa), (dtenv', globalGamma'))
            ) (dtenv0, globalGammaInit)
    (moduleNames, openDecs, includeDecs, typeDecs, inlineCodeDecs, checkedDecs)