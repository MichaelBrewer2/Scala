package object MainObjectPackage {
  def main(args: Array[String]){
    val ls = List(10, 10, 3,  1,  4,  5,  4,  2,  3,  10,  9)
    	//Index:  0   1   2   3   4   5   6   7   8   9   10
    
    //***************** CHECKS *****************//
    
    println("list = (10, 10, 3,  1,  4,  5,  4,  2,  3,  10,  9)")
    println("Index:  0   1   2   3   4   5   6   7   8   9   10")
    println()
    println("		Index Of Last Occurrence")
    println("Value:		Expected:	Actual:")
    println("10		9		" + findlast(ls, 10))
    println("4		6		" + findlast(ls, 4))
    println("3		8		" + findlast(ls, 3))
    println("9		10		" + findlast(ls, 9))
    println("2		7		" + findlast(ls, 2))
    println("5		5		" + findlast(ls, 5))
    println("1		3		" + findlast(ls, 1))
    println("6		-1		" + findlast(ls, 6))
  }
  
  
  /*
   * Function:	findlast(xs: List[Int], y: Int): Int = {}
   * 
   * Use of 3 helper functions. First, 'xs' is reversed and then passed into 'find'.
   * In 'find', the original 'xs' is traversed backwards, so the first time 'y' is
   * encountered, it is actually the last time it occurs in the original list. However,
   * the return value of 'find' must then be subtracted from the length of 'xs' in order
   * to find the correct index.
   */
  
  def findlast(xs: List[Int], y: Int): Int = {
    def find(xs: List[Int]): Int ={
      xs match{
        case Nil => len(xs) + 1
        case x::xs1 => x match{
          case `y` => 1
          case _ => find(xs1) + 1
        }
      }
    }
    
    def rev(xs: List[Int]): List[Int] = {
      xs match{
        case List() => xs
        case x::xs1 => rev(xs1):::List(x)
      }
    }
    
    def len(xs: List[Int]): Int ={
      xs match{
        case Nil => 0
        case x::xs1 => 1 + len(xs1)
      }
    }
    
    len(xs) - find(rev(xs))
  }
}