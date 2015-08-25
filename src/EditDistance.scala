/**
 * Created by skplanet on 2015-08-19.
 */
object EditDistance {
  def main(args: Array[String]) = {
    val dist = distance(args(0), args(1))
    print(" distance : " + dist)
  }

  def distance (str1:String, str2:String) : Int = {
    print(str1 + " " + str2)
    val matrix = Array.ofDim[Int](str1.length + 1, str2.length + 1)
    for (i <- 0 to str1.length;
         j <- 0 to str2.length) {

      if (i == 0) matrix(i)(j) = j
      else if (j == 0) matrix(i)(j) = i
      else matrix(i)(j) = min( matrix(i - 1)(j)  + 1,
                               matrix(i - 1)(j - 1) + (if (str1(i - 1) == str2(j - 1)) 0 else 1),
                               matrix(i)(j - 1) + 1 )
    }
    //print(" distance : " + matrix(str1.length)(str2.length))
    return matrix(str1.length)(str2.length)
  }

  def min(num1: Int, num2: Int, num3: Int) : Int = {
    if (num1 < num2) num1
    else if (num3 < num2) num3
    else num2
  }
}
