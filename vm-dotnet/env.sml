
(* Create Make Command *)

fun make(srcPath, file) =
  let
    val src = "../vm-com+/" ^ srcPath ^ "/" ^ file ^ ".aml"
    val dst = "../vm-com+/" ^ file ^ ".il"
    val obj = "../vm-com+/" ^ file ^ ".dll"
  in
    SMLToComPlusMain.compile(src, dst, "");
    OS.Process.system("ilasm /dll " ^ dst ^ " /out=" ^ obj)
  end

(* Create Clean Command *)

fun clean(file) =
  let
    val fileIL  = "../vm-com+/" ^ file ^ ".il "
    val filePDB = "../vm-com+/" ^ file ^ ".pdb "
    val fileDLL = "../vm-com+/" ^ file ^ ".dll" 
  in
    OS.Process.system("rm -rf " ^ fileIL ^ filePDB ^ fileDLL)
  end

(* Build Base Environment *)

local
  val EnvS    = ["CommandLine", "IO", "OS", "TextIO", "Tools"]
  fun Make(s) = make("", s)
in
  fun makeEnv()  = List.map Make EnvS
  fun cleanEnv() = List.map clean EnvS
end
