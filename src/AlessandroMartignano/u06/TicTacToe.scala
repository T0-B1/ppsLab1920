package AlessandroMartignano.u06

import scala.language.postfixOps
import AlessandroMartignano.u05.PerformanceUtils._

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
  val emptyBoard = List[Mark]()
  val newGame = List[Board](emptyBoard)
  val size = 3

  def find(board: Board, x: Double, y: Double): Option[Player] = board.collectFirst {
    case mark if mark.x == x && mark.y == y => mark.player
  }

  def placeAnyMark(board: Board, player: Player): Seq[Board] =
    for(x <- 0 until size; y <- 0 until size; if find(board, x, y).isEmpty)
      yield board :+ Mark(x, y, player)

  def computeAnyGameV1Verbose(player: Player, moves: Int): Stream[Game] = {
    def computeGameEvolutions(player: Player, mv: Int, board: Board): Stream[Game] = mv match {
      case mv if mv > 0 => {
        printIndent(moves-mv)
        println("Computing "+mv+ " "+player);
        printIndent(moves-mv)
        println("Input board: "+mv)
        printBoardsIndent(List(board), moves-mv)
        // Calcolo tutti i possibili piazzamenti
        val placings: Stream[Board] = placeAnyMark(board, player).toStream
        // Calcolo ogni scenario futuro, Aggiungo la board corrente in fondo a ogni game calcolato
        val evos: Stream[Stream[Game]] = placings.map(b=>{
          printIndent(moves-mv)
          println("Placing board "+mv)
          printBoardsIndent(List(b), moves-mv)
          val s: Stream[Game] = computeGameEvolutions(player.other, mv - 1, b).map(game => {
            printIndent(moves-mv)
            println("game before")
            if(game.isEmpty) {
              printIndent(moves-mv)
              println("empty")
            }
            else
              printBoardsIndent(game, moves-mv)
            var g: Game = game :+ b //elem board in fondo a lista game
            //board :: game // elem board in testa a lista game
            printIndent(moves-mv)
            println("game after")
            printBoardsIndent(g, moves-mv)
            g
          })
          printIndent(moves-mv)
          println("s: "+s.size)
          s
        })
        val allGames: Stream[Game] = evos.flatten
       allGames
      }
      case _ => Stream(List.empty)
    }
    computeGameEvolutions(player,moves,emptyBoard)
  }

  def computeAnyGameV2Verbose(player: Player, moves: Int): Stream[Game] = {
    moves match {
      case x if x > 0 => {
        println("Computing: "+moves)
        var res: Stream[Game] = Stream.empty
        val retMoves: Stream[Game] = computeAnyGameV2Verbose(player other, moves-1)
        println("returned moves: "+retMoves.size)
        for (move <- retMoves) {
          println("move:")
          printBoards(move)
          val placings: Seq[Board] = placeAnyMark(move head, player)
          println("possible placings: "+placings.size)
          for (board <- placings) {
            println("placing: ")
            printBoards(Seq(board))
            println("added cur board to cur game")
            val g: Game = board :: move
            printBoards(g)
            res = res.:+(g)
          }
        }
        println("returning games: "+res.size)
      res
      }
      case _ => {
        println("ret base newgame")
        Stream(newGame)
      }
    }
  }

  def computeAnyGameV2(player: Player, moves: Int): Stream[Game] = {
    moves match {
      case x if x > 0 =>
        for {
          possibleGame <- computeAnyGameV2(player other, moves-1)
          nextGames <- for (board <- placeAnyMark(possibleGame head, player)) yield board :: possibleGame
        } yield nextGames
      case _ => {
        Stream(newGame)
      }
    }
  }

  def computeAnyGameV3(player: Player, moves: Int): Stream[Game] =
    if (moves > 0)
      for {
        possibleGame <- computeAnyGameV3(player other, moves-1)
        nextGames <- for (board <- placeAnyMark(possibleGame head, player)) yield board :: possibleGame
      } yield nextGames
    else
      Stream(newGame)

  def computeAnyGame(player: Player, moves: Int): Stream[Game] =
    if (moves > 0)
      for {
        possibleGame <- computeAnyGame(player other, moves-1)
        nextGames <- if (checkWin(possibleGame head).nonEmpty) Set(possibleGame)
        else for (board <- placeAnyMark(possibleGame head, player)) yield board :: possibleGame
      } yield nextGames
    else
      Stream(newGame)

  def checkWin(board: Board): Option[Player] =
    (for (
      moves <- board.groupBy(m=>m.player);
      playerMarks = moves._2
      if (playerMarks.groupBy(_.x).exists(_._2.size == size)
        || playerMarks.groupBy(_.y).exists(_._2.size == size)
        || playerMarks.count(x => x.x == x.y) == size
        || playerMarks.count(x => x.x + x.y == (size - 1)) == size)
    ) yield moves._1).collectFirst(p=>p)

  def printBoards(game: Seq[Board]): Unit =
    for (y <- 0 to 2; board <- game.reverse; x <- 0 to 2) {
      print(find(board, x, y) map (_.toString) getOrElse ("."))
      if (x == 2) { print(" "); if (board == game.head) println()}
    }

  def printIndent(indent: Int): Unit = {
    print(" "*2*indent)
  }

  def printBoardsIndent(game: Seq[Board], indent: Int): Unit =
    for (y <- 0 to 2) {
      print(" "*2*indent)
      for( board <- game.reverse; x <- 0 to 2) {
        print(find(board, x, y) map (_.toString) getOrElse ("."))
        if (x == 2) { print(" "); if (board == game.head) println()}
      }
    }

/*
  // Exercise 1: implement find such that..
  println(find(List(Mark(0,0,X)),0,0)) // Some(X)
  println(find(List(Mark(0,0,X),Mark(0,1,O),Mark(0,2,X)),0,1)) // Some(O)
  println(find(List(Mark(0,0,X),Mark(0,1,O),Mark(0,2,X)),1,1)) // None

  // Exercise 2: implement placeAnyMark such that..
  printBoards(placeAnyMark(List(),X))
  //... ... ..X ... ... .X. ... ... X..
  //... ..X ... ... .X. ... ... X.. ...
  //..X ... ... .X. ... ... X.. ... ...

  printBoards(placeAnyMark(List(Mark(0,0,O)),X))
  //O.. O.. O.X O.. O.. OX. O.. O..
  //... ..X ... ... .X. ... ... X..
  //..X ... ... .X. ... ... X.. ...

  // Exercise 3 (ADVANCED!): implement computeAnyGame such that..

  //computeAnyGame(O, 4) foreach {g => printBoards(g); println()}
  //... X.. X.. X.. XO.
  //... ... O.. O.. O..
  //... ... ... X.. X..
  //              ... computes many such games (they should be 9*8*7*6 ~ 3000).. also, e.g.:
  //
  //... ... .O. XO. XOO
  //... ... ... ... ...
  //... .X. .X. .X. .X.
*/

  // Exercise 4 (VERY ADVANCED!) -- modify the above one so as to stop each game when someone won!!
  var s: Stream[Game] = Stream.empty
  measure("computeGameUntilWin(O,9)"){s = computeAnyGame(O, 9)}
  val l = s.toList
  println(l.size)
  for(i <- 0 to 100) {
    printBoards(l(i))
    println()
  }
}