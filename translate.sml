structure Translate : TRANSLATE = 
struct 
  structure F = MipsFrame
  
  datatype level = Top
                 | Level of {parent: level, frame: F.frame}
  type access = level * F.access

  val outermost = Top
  fun newLevel {parent, name, formals} =
    Level({parent=parent, frame=F.newFrame({name=name, formals=true::formals})})

  fun formals level =
    case level of
      Top => nil
    | Level({parent, frame}) => List.map (fn x => (level, x)) (F.formals frame)

  fun allocLocal (level : level) esc =
    case level of
      Level({parent, frame}) => (level, F.allocLocal frame esc)
end