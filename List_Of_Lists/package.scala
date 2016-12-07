package object mainPackageObject {
  def main(args: Array[String]){
    val nums = List(1, 2, 3, 4)
    val nums2 = List(1, 2, 3, 4, 5)
    val nums3 = List(-2, -1, 0, 1, 2)
    
    println(adjsublist(nums))
    println(adjsublist(nums2))
    println(adjsublist(nums3))
    
  }
  
  /*
   * Function:	adjsublist(xs: List[Int]): List[List[Int]] = {}
   * 
   * This confusing next part begins by taking 'xs' and 
   * then creates two new lists. One has the first element
   * removed, the other has the last element removed. Then
   * those new lists are appended in front (I forget the right
   * word for it) of 'xs' in a recursive manner. So the original
   * list is continually broken down one by one. However, this 
   * results in MANY duplicates. So there are a series of filters
   * and sortWith methods that put everything in order and
   * remove the duplicates.
   * 
   */
  
  def adjsublist(xs: List[Int]): List[List[Int]] = {
    xs match{
      case Nil => Nil
      case x::xs1 => {
        ((adjsublist(xs dropRight 1) ::: adjsublist(xs1) ::: List(xs))).filter(x => x match{
          case List() => false
          case _ => true
          }).sortWith(_.head < _.head).sortWith(_.length < _.length).distinct
      }
    }
  }
}