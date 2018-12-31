

module Term(X : Suplex.Lang.S) =
  struct

    let leaf = X.int64 0

    let node x y = X.tuple [X.T.Ex_repr x ; X.T.Ex_repr y]

    let term = 
      X.(
        (int64 3) >|= fun i ->
        (int64 4) >|= fun j ->
        (add Int64 i j) >|= fun isum64 ->
        (int16 2) >|= fun i ->
        (int16 4) >|= fun j ->
        (add Int16 i j) >|= fun isum16 ->
        (print isum64) >|= fun _ ->
        (print isum16) >|= fun _ ->
        (print (float 888.0)) >|= fun _ ->
        node isum16 isum64 >|= fun leaf ->
        node leaf leaf >|= fun node ->
        proj leaf { index = 0 ; desired_type = T.(TNum Int16) } >|= fun _result ->
        proj node { index = 0 ; desired_type = T.(TTpl [ Ex_typ (TNum Int16) ; Ex_typ (TNum Int64) ])  } >|= fun result1 ->
        proj result1 { index = 1 ; desired_type = T.(TNum Int64)  } >|= fun _result2 ->
        (print _result2)
          (* (
           *  (cond (lt Int16 i j)
           *     (print Unit unit)
           *     (print Bool tt))) *)
        )

  end


module Foo = Suplex.Compile.Codegen(Term)
