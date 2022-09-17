import std/[os, tables, unicode, strutils, random, math]

const debug = false

type
  Direction = enum
    S, R, UR, U, UL, L, DL, D, DR
  CollideType = enum
    # none, horizontal, vertical, forward (slash), backward (slash), octagon
    N, H, V, F, B, O
  PTable = object
    w, h: int
    colliders: seq[Collider]
    cueIdx: int
    cuedir: Direction
    balls, toSpawn: seq[Ball]
  Ball = ref object of RootObj
    dir: Direction
    pos: array[2, int]
  IntBall = ref object of Ball
    val: int
  IntReadBall = ref object of IntBall
  StrBall = ref object of Ball
    val: string
  StrReadBall = ref object of StrBall
  Collider = ref object of RootObj
    ct: CollideType
    pos: array[2, int]
  InstrKind = enum
    Destroy
    Out
    OutS
    Clone
    If
    Incr
    Decr
    Zero
    Square
    Invert
    Eight
    Pop
    Random
    Redirect
  Instruction = object
    kind: InstrKind
    dir: Direction
  Hole = ref object of Collider
    instrs: seq[Instruction]
  Stack = ref object of Collider
    single, repeat: seq[Ball]
    hasCue: bool

converter rune(str: string): Rune = str.runeAt(0)

method `$`(obj: Collider): string {.base.} =
  "collider " & $obj.ct & " at " & $obj.pos

method `$`(obj: Stack): string =
  "stack at " & $obj.pos & " with " & $(obj.single.len + obj.repeat.len) & " ball(s)"

method `$`(obj: Hole): string =
  "hole at " & $obj.pos & " with " & $obj.instrs.len & " instr(s)"

method `$`(obj: Ball): string {.base.} =
  "ball at " & $obj.pos & " moving " & $obj.dir

method `$`(obj: IntBall): string =
  "int ball (" & $obj.val & ") at " & $obj.pos & " moving " & $obj.dir

method `$`(obj: StrBall): string =
  "str ball (" & obj.val & ") at " & $obj.pos & " moving " & $obj.dir

proc prettyPTable(table: PTable) =
  if not debug: return
  echo "table state:"
  var ostr: seq[string]
  ostr.setLen(table.h)
  for i in 0 ..< ostr.len:
    for _ in 0 ..< table.w:
      ostr[i].add(" ")
  for col in table.colliders:
    var rep: char
    if col of Stack:
      rep = '^'
    elif col of Hole:
      rep = 'v'
    else:
      case col.ct:
      of N:
        rep = '.'
      of H:
        rep = '-'
      of V:
        rep = '|'
      of F:
        rep = '/'
      of B:
        rep = '\\'
      of O:
        rep = '+'
    ostr[col.pos[1]][col.pos[0]] = rep
  for ball in table.balls:
    ostr[ball.pos[1]][ball.pos[0]] = 'o'
  for line in ostr:
    echo line

proc addBallToStack(stack: var Stack, ball: Ball, repeat: bool) =
  if repeat:
    stack.repeat.add(ball)
  else:
    stack.single.add(ball)

proc createStack(input: string): Stack =
  result = new Stack
  result.ct = O
  result.hasCue = false
  var repeating = false
  for ball in input:
    case ball:
    of 'c':
      if repeating:
        echo "cannot repeat cue ball"
      elif result.single.len > 0:
        echo "invalid cue ball position"
      else:
        result.addBallToStack(Ball(), false)
        result.hasCue = true
    of 'o':
      result.addBallToStack(Ball(), repeating)
    of 'i':
      result.addBallToStack(IntBall(), repeating)
    of 's':
      result.addBallToStack(StrBall(), repeating)
    of 'n':
      result.addBallToStack(IntReadBall(), repeating)
    of 't':
      result.addBallToStack(StrReadBall(), repeating)
    of '|':
      repeating = true
    else:
      discard

proc parseDir(input: string): Direction =
  case input:
  of "S":
    return S
  of "R":
    return R
  of "UR":
    return UR
  of "U":
    return U
  of "UL":
    return UL
  of "L":
    return L
  of "DL":
    return DL
  of "D":
    return D
  of "DR":
    return DR

proc readDir(input: string, idx: int): Direction =
  var chk = input.substr(idx + 1, idx + 2)
  if chk.high < 1 or chk[1] notin ['L', 'R']:
    chk = $chk[0]
  return parseDir(chk)

proc createHole(input: string): Hole =
  result = new Hole
  result.ct = N
  var i = 0
  while i < len input:
    case input[i]:
    of 'x':
      result.instrs.add(Instruction(kind: Destroy))
    of 'o':
      result.instrs.add(Instruction(kind: Out))
    of 'c':
      result.instrs.add(Instruction(kind: OutS))
    of 'd':
      result.instrs.add(Instruction(kind: Clone, dir: readDir(input, i)))
    of 'i':
      result.instrs.add(Instruction(kind: If, dir: readDir(input, i)))
    of '+':
      result.instrs.add(Instruction(kind: Incr))
    of '-':
      result.instrs.add(Instruction(kind: Decr))
    of '0':
      result.instrs.add(Instruction(kind: Zero))
    of 's':
      result.instrs.add(Instruction(kind: Square))
    of '!':
      result.instrs.add(Instruction(kind: Invert))
    of '8':
      result.instrs.add(Instruction(kind: Eight))
    of 'p':
      result.instrs.add(Instruction(kind: Pop, dir: readDir(input, i)))
    of 'b':
      result.ct = O
    of '/':
      result.ct = F
    of '_':
      result.ct = H
    of '\\':
      result.ct = B
    of '|':
      result.ct = V
    of '?':
      result.instrs.add(Instruction(kind: Random))
    of ':':
      result.instrs.add(Instruction(kind: Redirect, dir: readDir(input, i)))
    else:
      discard
    inc i

proc constructTable(input: string): PTable =
  var
    tableStarted = false
    stacks = newTable[Rune, Stack]()
    holes = newTable[Rune, Hole]()
    cx, cy: int
    foundCue = false
  stacks["o"] = createStack("c")
  for line in input.splitLines:
    if not tableStarted:
      if line.isEmptyOrWhitespace: continue
      let
        name = line.runeAtPos(0)
        kind = line.runeAtPos(1)
        val = line.runeSubStr(2)
      case kind:
      of ">":
        if debug: echo "stack ", name, ": ", val
        stacks[name] = createStack(val)
      of "/":
        if debug: echo "hole ", name, ": ", val
        holes[name] = createHole(val)
      of "-":
        if debug: echo "cue ", val
        result.cuedir = parseDir(val)
        tableStarted = true
      else:
        # ignore
        discard
    else:
      inc result.h
      if line.runeLen > result.w:
        result.w = line.runeLen
      for sym in line.toRunes():
        case sym:
        of "/":
          result.colliders.add(Collider(ct: F, pos: [cx, cy]))
        of "-":
          result.colliders.add(Collider(ct: H, pos: [cx, cy]))
        of "\\":
          result.colliders.add(Collider(ct: B, pos: [cx, cy]))
        of "|":
          result.colliders.add(Collider(ct: V, pos: [cx, cy]))
        else:
          if sym in holes:
            var resHole = deepCopy holes[sym]
            resHole.pos = [cx, cy]
            result.colliders.add(resHole)
          elif sym in stacks:
            var resStack = deepCopy stacks[sym]
            if resStack.hasCue:
              if foundCue:
                raise newException(ValueError, "duplicate cue ball at " & $[cx, cy])
              else:
                foundCue = true
                result.cueIdx = result.colliders.len
            resStack.pos = [cx, cy]
            result.colliders.add(resStack)
        inc cx
      inc cy
      cx = 0
  if not foundCue:
    raise newException(ValueError, "no cue ball!")

proc move(ball: Ball) =
  case ball.dir:
  of S:
    discard
  of R:
    inc ball.pos[0]
  of UR:
    inc ball.pos[0]
    dec ball.pos[1]
  of U:
    dec ball.pos[1]
  of UL:
    dec ball.pos[0]
    dec ball.pos[1]
  of L:
    dec ball.pos[0]
  of DL:
    dec ball.pos[0]
    inc ball.pos[1]
  of D:
    inc ball.pos[1]
  of DR:
    inc ball.pos[0]
    inc ball.pos[1]

proc collideDir(col: CollideType, ball: Direction): Direction =
  case col:
  of N:
    return ball
  of H:
    case ball:
    of L, R, S: return S
    of U: return D
    of D: return U
    of UR: return DR
    of UL: return DL
    of DR: return UR
    of DL: return UL
  of V:
    case ball:
    of U, D, S: return S
    of L: return R
    of R: return L
    of UR: return UL
    of UL: return UR
    of DR: return DL
    of DL: return DR
  of F:
    discard
  of B:
    discard
  of O:
    var ndir = ball.int
    ndir += 4
    if ndir > Direction.high.int:
      ndir = ndir mod Direction.high.int
    return ndir.Direction

template collideDir(col: Collider, ball: Ball): Direction =
  collideDir(col.ct, ball.dir)

proc cullBalls(table: var PTable) =
  var i = 0
  while i < table.balls.len:
    if table.balls[i].pos[0] < 0 or table.balls[i].pos[0] >= table.w or table.balls[i].pos[1] < 0 or table.balls[i].pos[1] >= table.h:
      table.balls.delete(i)
      dec i
    inc i

proc pop(stack: Stack): Ball =
  if stack.single.len > 0:
    result = deepCopy stack.single[0]
    stack.single.delete(0)
    result.pos = stack.pos
  else:
    result = deepCopy stack.repeat[0]
    stack.repeat.delete(0)
    stack.repeat.add(result)
    result.pos = stack.pos

proc isEmpty(stack: Stack): bool =
  stack.single.len <= 0 and stack.repeat.len <= 0

proc pop(table: var PTable, idx: int): Ball =
  result = table.colliders[idx].Stack.pop()
  if table.colliders[idx].Stack.isEmpty:
    table.colliders.del(idx)

method onCollide(col: Collider, ball: Ball, table: var PTable) {.base.} =
  if debug: echo "collide " & $col & "/" & $ball
  ball.move()

method onCollide(hole: Hole, ball: Ball, table: var PTable) =
  if debug: echo "hole collide " & $hole & "/" & $ball
  if hole.ct != N:
    ball.move()
  for instr in hole.instrs:
    case instr.kind:
    of Destroy:
      # will be culled
      ball.pos = [-100, -100]
    of Out:
      if ball of IntBall:
        stdout.write($ball.IntBall.val & "\n")
      elif ball of StrBall:
        stdout.write($ball.StrBall.val & "\n")
    of OutS:
      if ball of IntBall:
        stdout.write($ball.IntBall.val.Rune)
      elif ball of StrBall:
        stdout.write($ball.StrBall.val)
    of Clone:
      # TODO test
      var nball = deepCopy ball
      nball.dir = instr.dir
      table.toSpawn.add(nball)
    of If:
      # TODO test
      if ball of IntBall and ball.IntBall.val > 0:
        ball.dir = instr.dir
      elif ball of StrBall and ball.StrBall.val.len <= 0:
        ball.dir = instr.dir
    of Incr:
      if ball of IntBall:
        inc ball.IntBall.val
    of Decr:
      if ball of IntBall:
        dec ball.IntBall.val
    of Zero:
      if ball of IntBall:
        ball.IntBall.val = 0
    of Square:
      if ball of IntBall:
        ball.IntBall.val = ball.IntBall.val ^ 2
    of Invert:
      if ball of IntBall:
        ball.IntBall.val = -ball.IntBall.val
    of Eight:
      if ball of IntBall:
        ball.IntBall.val += 8
    of Pop:
      # TODO test
      if ball of StrBall:
        table.toSpawn.add(IntBall(dir: instr.dir, pos: hole.pos, val: ball.StrBall.val.runeAt(0).int))
        ball.StrBall.val.removePrefix($ball.StrBall.val.runeAt(0))
    of Random:
      # TODO test
      ball.dir = (rand(Direction.high.int - 1) + 1).Direction
    of Redirect:
      ball.dir = instr.dir

proc simulate(table: var PTable) =
  # set the cue
  table.balls.add(table.pop(table.cueIdx))
  table.balls[0].dir = table.cuedir
  
  prettyPTable table
  while true:
    table.toSpawn.setLen(0)
    
    # move balls
    for ball in table.balls:
      ball.move()
    table.cullBalls()

    # process colliders
    # TODO
    var i = 0
    var gotCollide = false
    while i < table.colliders.len:
      for ball in table.balls:
        if table.colliders[i].pos[0] == ball.pos[0] and table.colliders[i].pos[1] == ball.pos[1]:
          if table.colliders[i].ct != N:
            gotCollide = true
          ball.dir = collideDir(table.colliders[i], ball)
          table.colliders[i].onCollide(ball, table)
          if table.colliders[i] of Stack:
            var nball = table.pop(i)
            nball.dir = collideDir(O, ball.dir)
            table.toSpawn.add(nball)
      table.cullBalls()
      inc i
      if i >= table.colliders.len and gotCollide:
        i = 0
        gotCollide = false

    # process ball collision
    # TODO

    # spawn balls
    for ball in table.toSpawn:
      if ball of IntReadBall:
        stdout.write("i> ")
        ball.IntBall.val = readLine(stdin).parseInt()
      elif ball of StrReadBall:
        stdout.write("s> ")
        ball.StrBall.val = readLine(stdin)
      table.balls.add(ball)

    prettyPTable table
    if table.balls.len <= 0:
      break

when isMainModule:
  randomize()
  let infile = open(commandLineParams()[0])
  var table = constructTable(infile.readAll)
  close infile
  prettyPTable table
  simulate table
