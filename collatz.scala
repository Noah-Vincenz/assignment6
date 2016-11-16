// Part 1 about the 3n+1 conceture
//=================================


//(1) Complete the collatz function below. It should
//    recursively calculate the number of steps needed 
//    until the collatz series reaches the number 1.
//    If needed you can use an auxilary function that
//    performs the recursion. The function should expect
//    arguments in the range of 1 to 1 Million.

def collatz(n: Long): Int = {
  if (n == 1) 0
  else collatzFinish(n,1)
}
def collatzFinish(input: Long, countIn: Int): Int = {
  if (input == 1) countIn
  else if (input % 2 == 0) collatzFinish(input/2, countIn+1)
  else collatzFinish(input * 3 + 1, countIn+1)
}
//(2)  Complete the collatz bound function below. It should
//     calculuate how many steps are needed for each number 
//     from 1 upto a bound and returns the maximum number of
//     steps and the corresponding number that needs that many 
//     steps. You should expect bounds in the range of 1
//     upto 1 million. The first component of the pair is
//     the maximum number of steps and the second is the 
//     corresponding number.

def collatz_max(bnd: Int): (Int, Int) = {
  var x = 1
  var maxSteps = 0
  var numberWithMaxSteps = 1
  while (x < bnd) {
    if (collatz(x) > maxSteps) {
      maxSteps = collatz(x)
      numberWithMaxSteps = x
    }
    x = x + 1
  }
  (maxSteps, numberWithMaxSteps)
}

