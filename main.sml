structure Main = 
struct
  structure Tr = Translate
  structure F = MipsFrame

  fun getsome (SOME x) = x

  fun temp_reg_map alloc temp =
    case Temp.Table.look(alloc, temp) of
      SOME(reg) => reg
    | NONE => Temp.makestring temp

  fun emitproc out (F.PROC{body,frame}) =
    let
      val _ = print ("emit " ^ (F.name frame) ^ "\n")
  (*         val _ = Printtree.printtree(out,body); *)
      val stms = Canon.linearize body
  (*         val _ = app (fn s => Printtree.printtree(out,s)) stms; *)
      val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
      val instrs = List.concat(map (MipsGen.codegen frame) stms') 
      val instrs' = F.procEntryExit2(frame, instrs)
      val (instrs'', alloc) = RegAlloc.alloc(instrs, frame)
      val {prolog, body, epilog} = F.procEntryExit3(frame, instrs'')

      val format0 = Assem.format(temp_reg_map alloc) 
    in  
      TextIO.output(out, prolog);
      app (fn i => TextIO.output(out,format0 i)) instrs'';
      TextIO.output(out, epilog)
    end
    | emitproc out (F.STRING(lab,s)) = TextIO.output(out,F.string(lab,s))
  
  fun withOpenFile fname f = 
    let 
      val out = TextIO.openOut fname
    in 
      (f out before TextIO.closeOut out) handle e => (TextIO.closeOut out; raise e)
    end 

  fun compile filename = 
    let 
      val absyn = Parse.parse filename
      val frags = (FindEscape.findEscape absyn; Semant.transProg absyn)
    in 
      withOpenFile (filename ^ ".s") (fn out => (app (emitproc out) frags))
    end
end



