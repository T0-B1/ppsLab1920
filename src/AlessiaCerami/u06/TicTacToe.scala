package AlessiaCerami.u06

object TicTacToe extends App {
  sealed trait Player{
    def other: Player = this match {case X => O; case _ => X}
    override def toString: String = this match {case X => "X"; case _ => "O"}
  }
  case object X extends Player
  case object O extends Player

  case class Mark(x: Double, y: Double, player: Player)
  type Board = List[Mark]
  type Game = List[Board]

  def find(board: Board, x: Double, y: Double): Option[Player] =
    board.find(mark => mark.x == x && mark.y == y).map(_.player)

  def placeAnyMark(board: Board, player: Player): Seq[Board] = {
    var possibleBoard:Seq[Board] = Seq()
    for (x <- 0 to 2;y <- 0 to 2){
      if(find(board,x,y).isEmpty)
        possibleBoard = possibleBoard.:+(board.:+(Mark(x,y,player)))
    }
    possibleBoard
  }

  def computeAnyGame(player: Player, moves: Int): LazyList[Game] = {
    def _computeAnyGame(board:Board, player: Player, moves: Int):LazyList[Game]= moves match {
      case m if m > 0 =>
        placeAnyMark(board, player).map(b => _computeAnyGame(b, player.other, moves-1).map(_ :+ b))
          .foldLeft(LazyList.empty[Game])(_ :++ _)

      case _ => LazyList(List.empty)
    }
    _computeAnyGame(List(),player,moves)
  }

  def checkWin(game:Game):Boolean = {
    val board=game.last

    val groupX = board.groupBy(m => m.x)
    val groupY = board.groupBy(m => m.y+3)
    val groupD = board.groupBy(m => if (m.x+m.y==2) 5 else 6).filter(kv => kv._1 != 6)
    val groupAD = board.groupBy(m => if(m.x == m.y) 6 else 7).filter(kv => kv._1 != 7)

    (groupX ++ groupY ++ groupD ++ groupAD).filter(rc => rc._2.size>2)
      .map(kv => kv._2.groupBy(m => m.player).size).count(_ == 1) > 0
  }

  def computeGameVictory(player: Player, moves: Int): LazyList[Game] = {
    def _computeGameVictory(game:Game, player: Player, moves: Int):LazyList[Game] = moves match {
      case m if m > 0 => {
        placeAnyMark(game.lastOption.getOrElse(List.empty), player).map(game :+ _)
          .collect({
            case g if (g.length<5 || !checkWin(g)) => _computeGameVictory(g, player.other, moves - 1)
            case g => LazyList(g)
          })
          .foldLeft(LazyList.empty[Game])(_ :++ _)
      }
      case _ => LazyList()
    }
    _computeGameVictory(List(),player,moves)
  }

  def printBoards(game: Seq[Board]): Unit =
    for (y <- 0 to 2; board <- game.reverse; x <- 0 to 2) {
      print(find(board, x, y) map (_.toString) getOrElse ("."))
      if (x == 2) { print(" "); if (board == game.head) println()}
    }

  // Exercise 1: implement find such that..
  println(find(List(Mark(0,0,X)),0,0)) // Some(X)
  println(find(List(Mark(0,0,X),Mark(0,1,O),Mark(0,2,X)),0,1)) // Some(O)
  println(find(List(Mark(0,0,X),Mark(0,1,O),Mark(0,2,X)),1,1)) // None

  // Exercise 2: implement placeAnyMark such that..
  printBoards(placeAnyMark(List(),X))
  //... ... ..X ... ... .X. ... ... X..
  //... ..X ... ... .X. ... ... X.. ...
  //..X ... ... .X. ... ... X.. ... ...

  println("----------------------------------")
  printBoards(placeAnyMark(List(Mark(0,0,O)),X))
  //O.. O.. O.X O.. O.. OX. O.. O..
  //... ..X ... ... .X. ... ... X..
  //..X ... ... .X. ... ... X.. ...

  // Exercise 3 (ADVANCED!): implement computeAnyGame such that..
  val res1 = computeAnyGame(O, 9)
  val res = computeGameVictory(O, 9)
  //res foreach {g => printBoards(g); println()}
  println(res1.size)
  println(res.size)
  //... X.. X.. X.. XO.
  //... ... O.. O.. O..
  //... ... ... X.. X..
  //              ... computes many such games (they should be 9*8*7*6 ~ 3000).. also, e.g.:
  //
  //... ... .O. XO. XOO
  //... ... ... ... ...
  //... .X. .X. .X. .X.

  // Exercise 4 (VERY ADVANCED!) -- modify the above one so as to stop each game when someone won!!
}