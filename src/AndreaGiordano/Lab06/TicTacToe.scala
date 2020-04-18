package AndreaGiordano.Lab06

import scala.collection.mutable

object TicTacToe extends App {
  sealed trait Player{
    def other: Player = this match {case X => O; case _ => X}
    override def toString: String = this match {case X => "X"; case _ => "O"}
  }
  case object X extends Player
  case object O extends Player

  case class Mark(x: Int, y: Int, player: Player)
  type Board = List[Mark]
  type Game = List[Board]

  def find(board: Board, x: Int, y: Int): Option[Player] = board.find(m=>m.x==x && m.y==y).map(_.player)

  def placeAnyMark(board: Board, player: Player): Seq[Board] = {
    val res:mutable.Queue[Board] = mutable.Queue().empty
    for(x<-0 to 2; y<-0 to 2)
      find(board,x,y).fold(res += board :+ Mark(x,y,player))(_=>res)
    res.toSeq
  }

  def computeAnyGame(player: Player, moves: Int): LazyList[Game] = {
    def _computeAnyGame(player: Player, moves: Int, board: Board): LazyList[Game] = moves match {
      case m if m > 0 => {
        placeAnyMark(board, player)
          .map(b=>_computeAnyGame(player.other, moves-1, b).map(_ :+ b))
          .foldLeft(LazyList.empty[Game])(_ :++ _)
      }
      case _ => LazyList(List.empty)
    }
    _computeAnyGame(player.other,moves,List())
  }

  def anyWinner(game: Game): Boolean = {
    game.last.map(m=>List((m.x,m.x,m.y,m.player),(m.y+3,m.x,m.y,m.player),
      (if(m.x+m.y==2)6 else 7,m.x,m.y,m.player),(if(m.x==m.y)8 else 9,m.x,m.y,m.player)))
      .foldLeft(List.empty[(Int,Int,Int,Player)])((q,l)=>q ++ l)
      .groupBy(_._1).filter(kv=>kv._2.size>2 && kv._1!=7 && kv._1!=9).map(kv=>kv._2.groupBy(_._4).size).count(_==1)>0
  }

  def goodAnyWinner(game: Game): Boolean = {
    val board = game.last
    val groupX = board.groupBy(m=>m.x)
    val groupY = board.groupBy(m=>m.y+3)
    val groupA = board.groupBy(m=> if(m.x+m.y == 2) 6 else 7) - 7
    val groupD = board.groupBy(m=> if(m.x==m.y) 7 else 8) - 8
    val group = groupX ++ groupY ++ groupD ++ groupA
    group.filter(kv=>kv._2.size>2).map(kv=>kv._2.groupBy(m=>m.player).size).count(_ == 1) > 0
  }

  def dummyAnyWinner(game: Game): Boolean = {
    val board = game.last
    for(c<-0 to 2){
      if(find(board,c,0) == find(board,c,1) && find(board,c,1) == find(board,c,2) && find(board,c,2).isDefined)
        return true
      if(find(board,0,c) == find(board,1,c) && find(board,1,c) == find(board,2,c) && find(board,2,c).isDefined)
        return true
    }
    if(find(board,0,0) == find(board,1,1) && find(board,1,1) == find(board,2,2) && find(board,2,2).isDefined)
      return true
    if(find(board,0,2) == find(board,1,1) && find(board,1,1) == find(board,2,0) && find(board,2,0).isDefined)
      return true
    false
  }

  def computeGameUntilWin(player: Player, moves: Int, checker: Game=>Boolean): LazyList[Game] = {
    def _computeGameUntilWin(player: Player, moves: Int, game: Game): LazyList[Game] = moves match {
      case m if m>0 =>
        placeAnyMark(game.lastOption.getOrElse(List()), player).map(game :+ _).collect({
          case g if g.length<5 || !checker(g)=> _computeGameUntilWin(player.other, moves-1, g )
          case g => LazyList(g)
        }).foldLeft(LazyList.empty[Game])(_ :++ _)
      case _ => LazyList()
    }
    _computeGameUntilWin(player.other,moves,List())
  }

  def printBoards(game: Seq[Board]): Unit =
    for (y <- 0 to 2; board <- game.reverse; x <- 0 to 2) {
      print(find(board, x, y) map (_.toString) getOrElse ("."))
      if (x == 2) { print(" "); if (board == game.head) println()}
    }

  // Exercise 1: implement find such that..
  println("-------------Find-------------")
  println(find(List(Mark(0,0,X)),0,0)) // Some(X)
  println(find(List(Mark(0,0,X),Mark(0,1,O),Mark(0,2,X)),0,1)) // Some(O)
  println(find(List(Mark(0,0,X),Mark(0,1,O),Mark(0,2,X)),1,1)) // None

  // Exercise 2: implement placeAnyMark such that..
  println("-------------PlaceAnyMark()-------------")
  printBoards(placeAnyMark(List(),X))
  //... ... ..X ... ... .X. ... ... X..
  //... ..X ... ... .X. ... ... X.. ...
  //..X ... ... .X. ... ... X.. ... ...
  println("-------------PlaceAnyMark(0,0,O)-------------")
  printBoards(placeAnyMark(List(Mark(0,0,O)),X))
  //O.. O.. O.X O.. O.. OX. O.. O..
  //... ..X ... ... .X. ... ... X..
  //..X ... ... .X. ... ... X.. ...

  import AndreaGiordano.PerformanceUtils.PerformanceUtils._
  // Exercise 3 (ADVANCED!): implement computeAnyGame such that..
  println("-------------computeAnyGame(O,9)-------------")
  measure("computeAnyGame(O,9)"){computeAnyGame(O, 9)}
  //val games = computeAnyGame(O, 9)
  //println("games size: "+games.size)

  //... X.. X.. X.. XO.
  //... ... O.. O.. O..
  //... ... ... X.. X..
  //              ... computes many such games (they should be 9*8*7*6 ~ 3000).. also, e.g.:
  //
  //... ... .O. XO. XOO
  //... ... ... ... ...
  //... .X. .X. .X. .X.


  // Exercise 4 (VERY ADVANCED!) -- modify the above one so as to stop each game when someone won!!
  println("-------------computeGameUntilWin(O,9) FinalAnyWinner-------------")
  measure("computeGameUntilWin(O,9)"){computeGameUntilWin(O, 9, anyWinner)}
  //val gamesUntilWin = computeGameUntilWin(O, 9)
  //println("games size: "+gamesUntilWin.size)

  println("-------------computeGameUntilWin(O,9) FinalAnyWinner-------------")
  measure("computeGameUntilWin(O,9)"){computeGameUntilWin(O, 9, goodAnyWinner)}

  println("-------------computeGameUntilWin(O,9) FinalAnyWinner-------------")
  measure("computeGameUntilWin(O,9)"){computeGameUntilWin(O, 9, dummyAnyWinner)}
}