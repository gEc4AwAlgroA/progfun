val w="aabcd"
val w2="ace"
val w3="cae"
val s=List(w,w2)
val d=List(w,w2,w3)

type Word = String
type Sentence = List[Word]
type Occurrences = List[(Char, Int)]

val occ:Occurrences = List(('a', 2), ('b', 2), ('c', 1))
val occ2:Occurrences = List(('h', 1),('j', 1))

  val lardd = List(('a', 1), ('d', 2), ('l', 1), ('r', 1))
  val r = List(('r', 1))
  val lad = List(('a', 1), ('d', 1), ('l', 1))

occ.length
occ(0)._2
occ(1)

//http://stackoverflow.com/questions/24150494/list-of-all-combinations
//struggled to get the iteration combinations correctly ordered
def combinations(occurrences: Occurrences): List[Occurrences] = occurrences match {
  case Nil => List(List())
  case (c, n) :: tail =>
    val rest = combinations(tail)
    (for {k <- rest; j <- 1 to n} yield (c, j) :: k) ::: rest
}

//http://scala4fun.tumblr.com/post/84792374567/mergemaps
//still don't get the reduceLeft
def subtract(x: Occurrences, y: Occurrences): Occurrences =
(List(x.toMap, y.toMap).reduceLeft ((r, m) => m.foldLeft(r) {
  case (dict, (k, v)) => dict + (k -> (dict.getOrElse(k, 0)-v))
}) toList ) filter {case(c,i) => i>0} sortBy (_._1)

subtract(lardd, lad)

def wordOccurrences(w: Word): Occurrences =
(w.toLowerCase groupBy (c => c) map {case (k, v) => (k, v.length)} toList) sortBy (_._1)

def sentenceOccurrences(s: Sentence): Occurrences =
((s flatMap wordOccurrences) groupBy {case (k, v) => k} map {case (k, v) => (k, v map (_._2) sum)} toList) sortBy (_._1)

lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
  (List("cat", "rule", "Line", "Zulu", "Rex", "Lin") map (w => (wordOccurrences(w), w))) groupBy {case (k, v) => k} mapValues (v => v map (x => x._2))

//def sentenceAnagrams(sentence: Sentence): List[Sentence] = ???

//http://shirleyisnotageek.blogspot.co.uk/2015/10/scala-note-5-sentence-anagrams.html
def sentenceAnagrams(sentence: Sentence)= {
  def sentenceInt(socc:Occurrences):List[Sentence] = socc match {
    case Nil => List(Nil)
    case socc => {
      val combs = combinations(socc)
      for {
        i <- combs if dictionaryByOccurrences.keySet(i)
        find <- dictionaryByOccurrences(i)
        rest <- sentenceInt(subtract(socc, i))
      } yield (find :: rest)
    }
  }
  sentenceInt(sentenceOccurrences(sentence))
}


sentenceAnagrams(List("Linux", "rulez"))

sentenceAnagrams(List())

//test("subtract: lard - r") {
//  val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
//  val r = List(('r', 1))
//  val lad = List(('a', 1), ('d', 1), ('l', 1))
//  assert(subtract(lard, r) === lad)
//}