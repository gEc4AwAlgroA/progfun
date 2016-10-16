val w="aabcd"
val w2="ace"
val w3="cae"
val s=List(w,w2)
val d=List(w,w2,w3)

type Word = String
type Sentence = List[Word]
type Occurrences = List[(Char, Int)]

def wordOccurrences(w: Word): Occurrences =
  (w.toLowerCase groupBy (c => c) map {case (k, v) => (k, v.length)} toList) sortBy (_._1)

(s flatMap wordOccurrences) groupBy {case (k, v) => k} map {case (k, v) => (k, v map (_._2) sum)}

val dict = (d map (w => (wordOccurrences(w), w))) groupBy {case (k, v) => k} mapValues (v => v map (x => x._2))

dict(wordOccurrences("ace"))

/** Converts a sentence into its character occurrence list. */
def sentenceOccurrences(s: Sentence): Occurrences = ???
//s map wordOccurrences groupBy


//test("sentenceOccurrences: abcd e") {
//  assert(sentenceOccurrences(List("abcd", "e")) === List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
//}
//
//test("sentenceOccurrences: abcd ace") {
//  assert(sentenceOccurrences(List("abcd", "ace")) === List(('a', 2), ('b', 1), ('c', 2), ('d', 1), ('e', 1)))
//}