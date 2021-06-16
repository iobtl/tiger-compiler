structure Main =
struct
  structure P = Parse
  structure Sem = Semant

  fun main filename =
    let
      val abExp = Parse.parse filename
    in
      Sem.transProg abExp
    end

  fun try () =
    let
      val fnames = ["../testcases/test1.tig",
                    "../testcases/test2.tig",
                    "../testcases/test3.tig",
                    "../testcases/test4.tig",
                    "../testcases/test5.tig",
                    "../testcases/test6.tig",
                    "../testcases/test7.tig",
                    "../testcases/test8.tig",
                    "../testcases/test9.tig",
                    "../testcases/test10.tig",
                    "../testcases/test11.tig",
                    "../testcases/test12.tig",
                    "../testcases/test13.tig",
                    "../testcases/test14.tig",
                    "../testcases/test15.tig",
                    "../testcases/test16.tig"]
      val abExps = List.map Parse.parse fnames
    in
      List.map Sem.transProg abExps
    end
end